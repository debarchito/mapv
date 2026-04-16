open Core

type gen = Young | Old

type event =
  | Minor_start
  | Minor_end of { promoted : int }
  | Major_mark of { steps : int }
  | Major_sweep of { steps : int; freed : int }
  | Major_end

type stats = {
  young_used : int;
  young_total : int;
  young_limit : int;
  old_used : int;
  old_total : int;
  n_chunks : int;
  alloc_count : int;
}

type obj_info = {
  addr : int;
  tag : int;
  size : int;
  gen : gen;
  marked : bool;
  fwd : int;
  fields : Value.t array;
}

module type TRACER = sig
  type ctx

  val on_alloc : ctx -> addr:int -> size:int -> tag:int -> unit
  val on_free : ctx -> addr:int -> unit
  val on_promote : ctx -> addr:int -> unit
  val on_gc : ctx -> event -> unit
  val on_read : ctx -> addr:int -> field:int -> Value.t -> unit
  val on_write : ctx -> addr:int -> field:int -> Value.t -> unit
end

module No_tracing = struct
  type ctx = unit

  let on_alloc () ~addr:_ ~size:_ ~tag:_ = ()
  let on_free () ~addr:_ = ()
  let on_promote () ~addr:_ = ()
  let on_gc () _ = ()
  let on_read () ~addr:_ ~field:_ _ = ()
  let on_write () ~addr:_ ~field:_ _ = ()
end

module Tracing = struct
  type ctx = {
    mutable allocs : (int * int * int * int) list;
    mutable frees : (int * int) list;
    mutable promotes : (int * int) list;
    mutable events : (int * event) list;
    mutable reads : (int * int * int) list;
    mutable writes : (int * int * int * Value.t) list;
    metadata : bytes;
    chunk_size : int;
    sample_rate : int;
    mutable tick : int;
  }

  let make ~max_chunks ~chunk_size ~sample_rate =
    {
      allocs = [];
      frees = [];
      promotes = [];
      events = [];
      reads = [];
      writes = [];
      metadata = Bytes.make (max_chunks * chunk_size) '\000';
      chunk_size;
      sample_rate;
      tick = 0;
    }

  let on_alloc ctx ~addr ~size ~tag =
    ctx.allocs <- (ctx.tick, addr, size, tag) :: ctx.allocs

  let on_free ctx ~addr = ctx.frees <- (ctx.tick, addr) :: ctx.frees
  let on_promote ctx ~addr = ctx.promotes <- (ctx.tick, addr) :: ctx.promotes
  let on_gc ctx ev = ctx.events <- (ctx.tick, ev) :: ctx.events

  let on_read ctx ~addr ~field v =
    ctx.tick <- ctx.tick + 1;
    if ctx.tick mod ctx.sample_rate = 0 then
      ctx.reads <- (ctx.tick, addr, field) :: ctx.reads;
    ignore v

  let on_write ctx ~addr ~field v =
    ctx.tick <- ctx.tick + 1;
    if ctx.tick mod ctx.sample_rate = 0 then begin
      ctx.writes <- (ctx.tick, addr, field, v) :: ctx.writes;
      let slot = addr + field in
      if slot < Bytes.length ctx.metadata then
        Bytes.set ctx.metadata slot
          (match v with
          | Value.Nil -> '\x00'
          | Value.Int _ -> '\x01'
          | Value.Float _ -> '\x02'
          | Value.Bool _ -> '\x03'
          | Value.Ptr _ -> '\x04'
          | Value.NativePtr _ -> '\x05'
          | Value.NativeFun _ -> '\x06')
    end
end

let log2 n =
  let rec go acc x = if x = 1 then acc else go (acc + 1) (x lsr 1) in
  go 0 n

module type S = sig
  type tracer_ctx

  type chunk = {
    data : Value.t array;
    size : int;
    mutable top : int;
    gen : gen;
    mutable free_list : int array;
  }

  type t

  val create : Config.Heap.t -> tracer_ctx -> t
  val alloc : t -> size:int -> tag:int -> int
  val alloc_old : t -> size:int -> tag:int -> int
  val free_old : t -> int -> unit
  val read : t -> int -> int -> Value.t
  val write : t -> int -> int -> Value.t -> unit
  val get_tag : t -> int -> int
  val get_size : t -> int -> int
  val get_mark : t -> int -> bool
  val get_fwd : t -> int -> int
  val set_tag : t -> int -> int -> unit
  val set_mark : t -> int -> bool -> unit
  val set_fwd : t -> int -> int -> unit
  val is_young : t -> int -> bool
  val is_old : t -> int -> bool
  val is_card_dirty : t -> int -> bool
  val clear_card : t -> int -> unit
  val needs_minor_gc : t -> bool
  val run_finalizers_young : t -> unit
  val reset_young : t -> unit
  val chunk_size : t -> int
  val on_gc : t -> event -> unit
  val on_promote : t -> int -> unit
  val iter_young_chunks : t -> (int -> chunk -> unit) -> unit
  val iter_old_chunks : t -> (int -> chunk -> unit) -> unit
  val iter_dirty_old_chunks : t -> (int -> chunk -> unit) -> unit
  val iter_chunk_objects : t -> int -> (int -> unit) -> unit
  val iter_objects : t -> (int -> unit) -> unit
  val stats : t -> stats
  val inspect : t -> int -> obj_info
  val tracer : t -> tracer_ctx
end

module Make (H : TRACER) : S with type tracer_ctx = H.ctx = struct
  type tracer_ctx = H.ctx

  let num_classes = 10
  let header_words = 1
  let min_free_size = 3

  let size_class sz =
    if sz <= 3 then 0
    else if sz = 4 then 1
    else if sz <= 6 then 2
    else if sz <= 8 then 3
    else if sz <= 16 then 4
    else if sz <= 32 then 5
    else if sz <= 64 then 6
    else if sz <= 128 then 7
    else if sz <= 256 then 8
    else 9

  type chunk = {
    data : Value.t array;
    size : int;
    mutable top : int;
    gen : gen;
    mutable free_list : int array;
  }

  type t = {
    mutable chunks : chunk array;
    mutable n_chunks : int;
    chunk_size : int;
    chunk_shift : int;
    chunk_mask : int;
    mutable young : int;
    mutable alloc_count : int;
    mutable old_live_words : int;
    young_limit : int;
    card_table : bytes;
    tracer_ctx : H.ctx;
  }

  let chunk_of t addr = addr lsr t.chunk_shift
  let slot_of t addr = addr land t.chunk_mask
  let is_young t addr = t.chunks.(chunk_of t addr).gen = Young
  let is_old t addr = t.chunks.(chunk_of t addr).gen = Old

  let encode_header ~tag ~size ~mark =
    (size lsl 10) lor (tag lsl 2) lor if mark then 2 else 0

  let get_raw_header t addr =
    let c = t.chunks.(chunk_of t addr) in
    let s = slot_of t addr - header_words in
    match c.data.(s) with
    | Value.Int h -> h
    | _ ->
        raise (Exception.Panic (Exception.Alloc_error "heap: corrupted header"))

  let get_tag t addr = (get_raw_header t addr lsr 2) land 0xFF
  let get_size t addr = get_raw_header t addr lsr 10
  let get_mark t addr = get_raw_header t addr land 2 <> 0

  let get_fwd t addr =
    if get_tag t addr = Tag.forward then
      match t.chunks.(chunk_of t addr).data.(slot_of t addr) with
      | Value.Int f -> f
      | _ -> -1
    else -1

  let set_raw_header t addr h =
    let ci = chunk_of t addr in
    let s = slot_of t addr - header_words in
    t.chunks.(ci).data.(s) <- Value.Int h;
    H.on_write t.tracer_ctx ~addr ~field:(-1) (Value.Int h)

  let set_header t addr ~tag ~size ~mark =
    set_raw_header t addr (encode_header ~tag ~size ~mark)

  let set_tag t addr v =
    set_header t addr ~tag:v ~size:(get_size t addr) ~mark:(get_mark t addr)

  let set_mark t addr v =
    set_header t addr ~tag:(get_tag t addr) ~size:(get_size t addr) ~mark:v

  let set_fwd t addr v =
    set_header t addr ~tag:Tag.forward ~size:(get_size t addr)
      ~mark:(get_mark t addr);
    let ci = chunk_of t addr in
    let s = slot_of t addr in
    t.chunks.(ci).data.(s) <- Value.Int v;
    H.on_write t.tracer_ctx ~addr ~field:0 (Value.Int v)

  let mark_card t addr = Bytes.set t.card_table (chunk_of t addr) '\001'
  let clear_card t ci = Bytes.set t.card_table ci '\000'
  let is_card_dirty t ci = Bytes.get t.card_table ci <> '\000'

  let write_barrier t addr value =
    match value with
    | Value.Ptr _ | Value.NativePtr _ -> if is_old t addr then mark_card t addr
    | _ -> ()

  let check_bounds t addr field =
    let sz = get_size t addr in
    if field < 0 || field >= sz then
      raise
        (Exception.Signal
           (Exception.Bounds_error
              (Format.asprintf "heap field %d out of bounds (size %d)" field sz)))

  let read t addr field =
    check_bounds t addr field;
    let v = t.chunks.(chunk_of t addr).data.(slot_of t addr + field) in
    H.on_read t.tracer_ctx ~addr ~field v;
    v

  let write t addr field value =
    check_bounds t addr field;
    write_barrier t addr value;
    let ci = chunk_of t addr in
    let s = slot_of t addr + field in
    t.chunks.(ci).data.(s) <- value;
    H.on_write t.tracer_ctx ~addr ~field value

  let make_chunk size gen =
    let data = Array.make size Value.Nil in
    data.(0) <- Value.Int 0;
    { data; size; top = 0; gen; free_list = Array.make num_classes (-1) }

  let create (cfg : Config.Heap.t) ctx =
    let shift = log2 cfg.chunk_size in
    let mask = cfg.chunk_size - 1 in
    {
      chunks =
        Array.init cfg.max_chunks (fun _ -> make_chunk cfg.chunk_size Young);
      n_chunks = 1;
      chunk_size = cfg.chunk_size;
      chunk_shift = shift;
      chunk_mask = mask;
      young = 0;
      alloc_count = 0;
      old_live_words = 0;
      young_limit = cfg.young_limit;
      card_table = Bytes.make cfg.max_chunks '\000';
      tracer_ctx = ctx;
    }

  let add_chunk t gen =
    if t.n_chunks >= Array.length t.chunks then
      raise
        (Exception.Panic
           (Exception.Alloc_error "heap exhausted: max_chunks reached"));
    let idx = t.n_chunks in
    t.chunks.(idx) <- make_chunk t.chunk_size gen;
    t.n_chunks <- t.n_chunks + 1;
    idx

  let fl_get_next (c : chunk) slot =
    match c.data.(slot + 1) with Value.Int n -> n | _ -> -1

  let fl_get_prev (c : chunk) slot =
    match c.data.(slot + 2) with Value.Int n -> n | _ -> -1

  let fl_set_next (c : chunk) slot v = c.data.(slot + 1) <- Value.Int v
  let fl_set_prev (c : chunk) slot v = c.data.(slot + 2) <- Value.Int v

  let fl_insert (c : chunk) cls slot size =
    c.data.(slot) <- Value.Int (encode_header ~tag:Tag.free ~size ~mark:false);
    c.data.(slot + size) <- Value.Int size;
    let old_head = c.free_list.(cls) in
    fl_set_next c slot old_head;
    fl_set_prev c slot (-1);
    if old_head <> -1 then fl_set_prev c old_head slot;
    c.free_list.(cls) <- slot

  let fl_remove (c : chunk) cls slot =
    let next = fl_get_next c slot in
    let prev = fl_get_prev c slot in
    if prev = -1 then c.free_list.(cls) <- next else fl_set_next c prev next;
    if next <> -1 then fl_set_prev c next prev

  let alloc t ~size ~tag =
    let size = max min_free_size size in
    let needed = header_words + size in
    if needed > t.chunk_size then
      raise
        (Exception.Panic
           (Exception.Alloc_error
              (Format.asprintf "alloc: size %d exceeds chunk_size" size)));
    let yc = t.chunks.(t.young) in
    if yc.top + needed > yc.size then t.young <- add_chunk t Young;
    let yc = t.chunks.(t.young) in
    let addr = (t.young * t.chunk_size) + yc.top + header_words in
    set_header t addr ~tag ~size ~mark:false;
    yc.top <- yc.top + needed;
    t.alloc_count <- t.alloc_count + 1;
    H.on_alloc t.tracer_ctx ~addr ~size ~tag;
    addr

  let alloc_old t ~size ~tag =
    let size = max min_free_size size in
    let needed = header_words + size in
    if needed > t.chunk_size then
      raise
        (Exception.Panic
           (Exception.Alloc_error
              (Format.asprintf "alloc_old: size %d exceeds chunk_size" size)));
    let found = ref (-1) in
    let start_cls = size_class size in
    let ci = ref 0 in
    while !found = -1 && !ci < t.n_chunks do
      let c = t.chunks.(!ci) in
      if c.gen = Old then begin
        let cls = ref start_cls in
        while !found = -1 && !cls < num_classes do
          let cur = ref c.free_list.(!cls) in
          while !found = -1 && !cur <> -1 do
            let h = match c.data.(!cur) with Value.Int h -> h | _ -> 0 in
            let free_size = h lsr 10 in
            let next = fl_get_next c !cur in
            if free_size >= size then begin
              fl_remove c !cls !cur;
              let remainder = free_size - size - header_words in
              let actual_size =
                if remainder >= min_free_size then begin
                  let new_slot = !cur + header_words + size in
                  fl_insert c (size_class remainder) new_slot remainder;
                  size
                end
                else free_size
              in
              let addr = (!ci * t.chunk_size) + !cur + header_words in
              set_header t addr ~tag ~size:actual_size ~mark:false;
              Array.fill c.data (!cur + header_words) actual_size Value.Nil;
              t.old_live_words <- t.old_live_words + actual_size;
              found := addr
            end
            else cur := next
          done;
          incr cls
        done;
        if !found = -1 && c.top + needed <= c.size then begin
          let addr = (!ci * t.chunk_size) + c.top + header_words in
          set_header t addr ~tag ~size ~mark:false;
          c.top <- c.top + needed;
          t.old_live_words <- t.old_live_words + size;
          found := addr
        end
      end;
      incr ci
    done;
    if !found = -1 then begin
      let idx = add_chunk t Old in
      let c = t.chunks.(idx) in
      let addr = (idx * t.chunk_size) + c.top + header_words in
      set_header t addr ~tag ~size ~mark:false;
      c.top <- c.top + needed;
      t.old_live_words <- t.old_live_words + size;
      found := addr
    end;
    H.on_alloc t.tracer_ctx ~addr:!found ~size ~tag;
    !found

  let free_old t addr =
    let ci = chunk_of t addr in
    let c = t.chunks.(ci) in
    let slot = slot_of t addr - header_words in
    let size = get_size t addr in
    for i = 0 to size - 1 do
      match c.data.(slot + header_words + i) with
      | Value.NativePtr np -> Value.call_finalizer np
      | _ -> ()
    done;
    Array.fill c.data (slot + header_words) size Value.Nil;
    t.old_live_words <- max 0 (t.old_live_words - size);
    let mut_slot = ref slot in
    let mut_size = ref size in
    let next_slot = slot + header_words + size in
    if next_slot < c.top then begin
      let next_h = match c.data.(next_slot) with Value.Int h -> h | _ -> 0 in
      let next_tag = (next_h lsr 2) land 0xFF in
      if next_tag = Tag.free then begin
        let next_size = next_h lsr 10 in
        fl_remove c (size_class next_size) next_slot;
        mut_size := !mut_size + header_words + next_size
      end
    end;
    if slot > 0 then begin
      let footer_val =
        match c.data.(slot - 1) with Value.Int n -> n | _ -> -1
      in
      if footer_val >= min_free_size then begin
        let prev_size = footer_val in
        let prev_slot = slot - header_words - prev_size in
        if prev_slot >= 0 then begin
          let prev_h =
            match c.data.(prev_slot) with Value.Int h -> h | _ -> 0
          in
          let prev_tag = (prev_h lsr 2) land 0xFF in
          if prev_tag = Tag.free && prev_h lsr 10 = prev_size then begin
            fl_remove c (size_class prev_size) prev_slot;
            mut_slot := prev_slot;
            mut_size := !mut_size + header_words + prev_size
          end
        end
      end
    end;
    fl_insert c (size_class !mut_size) !mut_slot !mut_size;
    H.on_free t.tracer_ctx ~addr

  let run_finalizers_young t =
    for i = 0 to t.n_chunks - 1 do
      let c = t.chunks.(i) in
      if c.gen = Young then begin
        let pos = ref 0 in
        while !pos < c.top do
          let h = match c.data.(!pos) with Value.Int h -> h | _ -> 0 in
          let tag = (h lsr 2) land 0xFF in
          let size = max min_free_size (h lsr 10) in
          if tag <> Tag.free && tag <> Tag.forward then
            for f = 0 to size - 1 do
              match c.data.(!pos + header_words + f) with
              | Value.NativePtr np -> Value.call_finalizer np
              | _ -> ()
            done;
          pos := !pos + header_words + size
        done
      end
    done

  let needs_minor_gc t = t.alloc_count >= t.young_limit

  let reset_young t =
    let c = t.chunks.(t.young) in
    c.top <- 0;
    Array.fill c.data 0 c.size Value.Nil;
    t.alloc_count <- 0

  let chunk_size t = t.chunk_size

  let iter_young_chunks t f =
    for i = 0 to t.n_chunks - 1 do
      if t.chunks.(i).gen = Young then f i t.chunks.(i)
    done

  let iter_old_chunks t f =
    for i = 0 to t.n_chunks - 1 do
      if t.chunks.(i).gen = Old then f i t.chunks.(i)
    done

  let iter_dirty_old_chunks t f =
    for i = 0 to t.n_chunks - 1 do
      if t.chunks.(i).gen = Old && is_card_dirty t i then f i t.chunks.(i)
    done

  let iter_chunk_objects t ci f =
    let c = t.chunks.(ci) in
    let pos = ref 0 in
    while !pos < c.top do
      let h = match c.data.(!pos) with Value.Int h -> h | _ -> 0 in
      let tag = (h lsr 2) land 0xFF in
      let size = max min_free_size (h lsr 10) in
      if tag <> Tag.free && tag <> Tag.forward then
        f ((ci * t.chunk_size) + !pos + header_words);
      pos := !pos + header_words + size
    done

  let iter_objects t f =
    for ci = 0 to t.n_chunks - 1 do
      iter_chunk_objects t ci f
    done

  let stats t =
    let yu = ref 0 in
    let yt = ref 0 in
    let ot = ref 0 in
    for i = 0 to t.n_chunks - 1 do
      let c = t.chunks.(i) in
      if c.gen = Young then (
        yu := !yu + c.top;
        yt := !yt + c.size)
      else ot := !ot + c.size
    done;
    {
      young_used = !yu;
      young_total = !yt;
      young_limit = t.young_limit;
      old_used = t.old_live_words;
      old_total = !ot;
      n_chunks = t.n_chunks;
      alloc_count = t.alloc_count;
    }

  let inspect t addr =
    let size = get_size t addr in
    {
      addr;
      tag = get_tag t addr;
      size;
      gen = t.chunks.(chunk_of t addr).gen;
      marked = get_mark t addr;
      fwd = get_fwd t addr;
      fields = Array.init size (read t addr);
    }

  let tracer t = t.tracer_ctx
  let on_gc t ev = H.on_gc t.tracer_ctx ev

  let on_promote t a =
    let size = get_size t a in
    t.old_live_words <- t.old_live_words + size;
    H.on_promote t.tracer_ctx ~addr:a
end
