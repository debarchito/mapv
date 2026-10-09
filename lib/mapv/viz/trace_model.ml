open Core
module Ev = Trace.Event

type bucket = {
  mutable lo : int;
  mutable hi : int;
  mutable instr : int;
  mutable minor : int;
  mutable major : int;
  mutable promoted : int;
  mutable freed : int;
  mutable allocs : int;
  mutable calls : int;
  mutable yields : int;
  mutable throws : int;
}

type t = {
  max_window : int;
  bucket_size : int;
  max_buckets : int;
  ring : Ev.t Queue.t;
  mutable count : int;
  mutable head : int;
  mutable lo : int;
  snapshot : Value.t array;
  written_before : bool array;
  mutable depth_before : int;
  mutable in_gc_before : bool;
  mutable archive : bucket Queue.t;
  mutable n_buckets : int;
  mutable cur : bucket option;
}

let new_bucket seq =
  {
    lo = seq;
    hi = seq;
    instr = 0;
    minor = 0;
    major = 0;
    promoted = 0;
    freed = 0;
    allocs = 0;
    calls = 0;
    yields = 0;
    throws = 0;
  }

let create ?(window = max_int) ?(bucket_size = 1000) ?(max_buckets = 4096) () =
  {
    max_window = max 1 window;
    bucket_size = max 1 bucket_size;
    max_buckets = max 2 max_buckets;
    ring = Queue.create ();
    count = 0;
    head = -1;
    lo = -1;
    snapshot = Array.make 256 Value.Nil;
    written_before = Array.make 256 false;
    depth_before = 0;
    in_gc_before = false;
    archive = Queue.create ();
    n_buckets = 0;
    cur = None;
  }

let merge (a : bucket) (b : bucket) =
  {
    lo = a.lo;
    hi = b.hi;
    instr = a.instr + b.instr;
    minor = a.minor + b.minor;
    major = a.major + b.major;
    promoted = a.promoted + b.promoted;
    freed = a.freed + b.freed;
    allocs = a.allocs + b.allocs;
    calls = a.calls + b.calls;
    yields = a.yields + b.yields;
    throws = a.throws + b.throws;
  }

let age t =
  let all = Queue.fold (fun acc b -> b :: acc) [] t.archive |> List.rev in
  let rec go = function
    | a :: b :: rest -> merge a b :: go rest
    | [ x ] -> [ x ]
    | [] -> []
  in
  let q = Queue.create () in
  List.iter (fun b -> Queue.push b q) (go all);
  t.archive <- q;
  t.n_buckets <- Queue.length t.archive

let archive_add t (ev : Ev.t) =
  let idx = ev.seq / t.bucket_size in
  let b =
    match t.cur with
    | Some b when b.hi / t.bucket_size = idx -> b
    | _ ->
        (match t.cur with
        | Some b ->
            Queue.push b t.archive;
            t.n_buckets <- t.n_buckets + 1
        | None -> ());
        let nb = new_bucket ev.seq in
        t.cur <- Some nb;
        nb
  in
  b.hi <- ev.seq;
  (match ev.kind with
  | Ev.Instr _ -> b.instr <- b.instr + 1
  | Ev.Call _ -> b.calls <- b.calls + 1
  | Ev.Con_yield _ -> b.yields <- b.yields + 1
  | Ev.Throw _ -> b.throws <- b.throws + 1
  | Ev.Alloc _ -> b.allocs <- b.allocs + 1
  | Ev.Gc { event = Ev.Minor_end { promoted } } ->
      b.minor <- b.minor + 1;
      b.promoted <- b.promoted + promoted
  | Ev.Gc { event = Ev.Major_end } -> b.major <- b.major + 1
  | Ev.Gc { event = Ev.Major_sweep { freed; _ } } -> b.freed <- b.freed + freed
  | _ -> ());
  if t.n_buckets > t.max_buckets then age t

let evict t =
  if t.max_window <> max_int then
    while t.count > t.max_window do
      let ev = Queue.take t.ring in
      t.count <- t.count - 1;
      match ev.kind with
      | Ev.Reg_write { reg; value } ->
          t.snapshot.(reg) <- value;
          t.written_before.(reg) <- true
      | Ev.Call _ -> t.depth_before <- t.depth_before + 1
      | Ev.Ret _ -> t.depth_before <- max 0 (t.depth_before - 1)
      | Ev.Gc { event = Ev.Minor_start } | Ev.Gc { event = Ev.Major_mark _ } ->
          t.in_gc_before <- true
      | Ev.Gc { event = Ev.Minor_end _ } | Ev.Gc { event = Ev.Major_end } ->
          t.in_gc_before <- false
      | _ -> ()
    done;
  if t.count > 0 then t.lo <- (Queue.peek t.ring).seq

let ingest t (ev : Ev.t) =
  t.head <- ev.seq;
  if t.count = 0 then t.lo <- ev.seq;
  Queue.push ev t.ring;
  t.count <- t.count + 1;
  archive_add t ev;
  evict t

let head t = t.head
let lo t = t.lo
let count t = t.count
let iter t f = Queue.iter f t.ring

let scan t f =
  let acc = ref [] in
  Queue.iter
    (fun ev -> match f ev with Some x -> acc := x :: !acc | None -> ())
    t.ring;
  List.rev !acc

let reg_value_at t tick reg =
  let last = ref None in
  Queue.iter
    (fun (ev : Ev.t) ->
      match ev.kind with
      | Ev.Reg_write { reg = r; value } when r = reg && ev.seq <= tick ->
          last := Some value
      | _ -> ())
    t.ring;
  match !last with
  | Some v -> Some v
  | None -> if t.written_before.(reg) then Some t.snapshot.(reg) else None

let reg_last_tick t tick reg =
  let lt = ref (-1) in
  Queue.iter
    (fun (ev : Ev.t) ->
      match ev.kind with
      | Ev.Reg_write { reg = r; _ } when r = reg && ev.seq <= tick ->
          lt := ev.seq
      | _ -> ())
    t.ring;
  !lt

let active_regs t tick =
  let tbl = Hashtbl.create 64 in
  Array.iteri (fun r w -> if w then Hashtbl.replace tbl r ()) t.written_before;
  Queue.iter
    (fun (ev : Ev.t) ->
      match ev.kind with
      | Ev.Reg_write { reg; _ } when ev.seq <= tick ->
          Hashtbl.replace tbl reg ()
      | _ -> ())
    t.ring;
  tbl

let current_instr t tick =
  let last = ref None in
  Queue.iter
    (fun (ev : Ev.t) ->
      match ev.kind with
      | Ev.Instr { pc; op } when ev.seq <= tick -> last := Some (ev.seq, pc, op)
      | _ -> ())
    t.ring;
  !last

let writes_at_tick t tick =
  scan t (fun ev ->
      match ev.kind with
      | Ev.Reg_write { reg; value } when ev.seq = tick ->
          Some (ev.seq, reg, value)
      | _ -> None)

let calls_at t tick =
  scan t (fun ev ->
      match ev.kind with
      | Ev.Call { pc; target } when ev.seq <= tick -> Some (ev.seq, pc, target)
      | _ -> None)

let rets_at t tick =
  scan t (fun ev ->
      match ev.kind with
      | Ev.Ret { pc } when ev.seq <= tick -> Some (ev.seq, pc)
      | _ -> None)

let allocs_at t tick =
  scan t (fun ev ->
      match ev.kind with
      | Ev.Alloc { addr; size; tag } when ev.seq <= tick ->
          Some (ev.seq, addr, size, tag)
      | _ -> None)

let frees_at t tick =
  scan t (fun ev ->
      match ev.kind with
      | Ev.Free { addr } when ev.seq <= tick -> Some addr
      | _ -> None)

let promotes_at t tick =
  scan t (fun ev ->
      match ev.kind with
      | Ev.Promote { addr } when ev.seq <= tick -> Some addr
      | _ -> None)

let gc_events_at t tick =
  scan t (fun ev ->
      match ev.kind with
      | Ev.Gc { event } when ev.seq <= tick -> Some (ev.seq, event)
      | _ -> None)

let gc_minor_count t tick =
  List.fold_left
    (fun a (_, ev) -> match ev with Ev.Minor_end _ -> a + 1 | _ -> a)
    0 (gc_events_at t tick)

let gc_major_count t tick =
  List.fold_left
    (fun a (_, ev) -> match ev with Ev.Major_end -> a + 1 | _ -> a)
    0 (gc_events_at t tick)

let gc_promoted_total t tick =
  List.fold_left
    (fun a (_, ev) ->
      match ev with Ev.Minor_end { promoted } -> a + promoted | _ -> a)
    0 (gc_events_at t tick)

let gc_freed_total t tick =
  List.fold_left
    (fun a (_, ev) ->
      match ev with Ev.Major_sweep { freed; _ } -> a + freed | _ -> a)
    0 (gc_events_at t tick)

let in_gc t tick =
  let r = ref t.in_gc_before in
  Queue.iter
    (fun (ev : Ev.t) ->
      if ev.seq <= tick then
        match ev.kind with
        | Ev.Gc { event = Ev.Minor_start } | Ev.Gc { event = Ev.Major_mark _ }
          ->
            r := true
        | Ev.Gc { event = Ev.Minor_end _ } | Ev.Gc { event = Ev.Major_end } ->
            r := false
        | _ -> ())
    t.ring;
  !r

let gen_split t tick =
  let allocs = allocs_at t tick in
  let promoted = promotes_at t tick in
  let old_set = Hashtbl.create 16 in
  List.iter (fun addr -> Hashtbl.replace old_set addr ()) promoted;
  let young =
    List.filter (fun (_, a, _, _) -> not (Hashtbl.mem old_set a)) allocs
  in
  let old = List.filter (fun (_, a, _, _) -> Hashtbl.mem old_set a) allocs in
  (young, old)

let continuations_at t tick =
  let news =
    scan t (fun ev ->
        match ev.kind with
        | Ev.Con_new { pc = _; con_id } when ev.seq <= tick ->
            Some (ev.seq, con_id)
        | _ -> None)
  in
  List.map
    (fun (birth, cid) ->
      let yields =
        List.length
          (scan t (fun ev ->
               match ev.kind with
               | Ev.Con_yield { con_id; _ } when ev.seq <= tick && con_id = cid
                 ->
                   Some ()
               | _ -> None))
      in
      let resumes =
        List.length
          (scan t (fun ev ->
               match ev.kind with
               | Ev.Con_resume { con_id; _ } when ev.seq <= tick && con_id = cid
                 ->
                   Some ()
               | _ -> None))
      in
      let last_yield = ref (-1) and last_resume = ref (-1) in
      Queue.iter
        (fun (ev : Ev.t) ->
          if ev.seq <= tick then
            match ev.kind with
            | Ev.Con_yield { con_id; _ } when con_id = cid ->
                last_yield := max !last_yield ev.seq
            | Ev.Con_resume { con_id; _ } when con_id = cid ->
                last_resume := max !last_resume ev.seq
            | _ -> ())
        t.ring;
      let status =
        if !last_resume > !last_yield && !last_resume >= 0 then `Running
        else if !last_yield >= 0 then `Suspended
        else `New
      in
      (cid, birth, status, yields, resumes))
    news

let call_depth t tick =
  let d = ref t.depth_before in
  Queue.iter
    (fun (ev : Ev.t) ->
      if ev.seq <= tick then
        match ev.kind with
        | Ev.Call _ -> incr d
        | Ev.Ret _ -> d := max 0 (!d - 1)
        | _ -> ())
    t.ring;
  !d

let active_call_frames t tick =
  let calls = calls_at t tick in
  let depth = call_depth t tick in
  let n = List.length calls in
  let start = max 0 (n - depth) in
  let arr = Array.of_list calls in
  Array.to_list (Array.sub arr start (n - start))

let last_write t tick addr field =
  let last = ref Value.Nil in
  Queue.iter
    (fun (ev : Ev.t) ->
      match ev.kind with
      | Ev.Write { addr = a; field = f; value }
        when a = addr && f = field && ev.seq <= tick ->
          last := value
      | _ -> ())
    t.ring;
  !last

let bucket_list t =
  let all = Queue.fold (fun acc b -> b :: acc) [] t.archive |> List.rev in
  match t.cur with Some b -> all @ [ b ] | None -> all

let totals t =
  List.fold_left
    (fun acc b ->
      {
        acc with
        instr = acc.instr + b.instr;
        minor = acc.minor + b.minor;
        major = acc.major + b.major;
        promoted = acc.promoted + b.promoted;
        freed = acc.freed + b.freed;
        allocs = acc.allocs + b.allocs;
        calls = acc.calls + b.calls;
        yields = acc.yields + b.yields;
        throws = acc.throws + b.throws;
      })
    (new_bucket 0) (bucket_list t)

let of_read (r : Trace.Serializer.Read.t) =
  let t = create () in
  let evs = ref [] in
  let add seq kind = evs := { Ev.seq; kind } :: !evs in
  Array.iter (fun (s, pc, op) -> add s (Ev.Instr { pc; op })) r.vm.instrs;
  Array.iter (fun (s, pc, target) -> add s (Ev.Call { pc; target })) r.vm.calls;
  Array.iter (fun (s, pc) -> add s (Ev.Ret { pc })) r.vm.rets;
  Array.iter (fun (s, pc) -> add s (Ev.Throw { pc })) r.vm.throws;
  let cid = ref 0 in
  Array.iter
    (fun (s, pc) ->
      add s (Ev.Con_new { pc; con_id = !cid });
      incr cid)
    r.vm.con_news;
  Array.iter
    (fun (s, con_id, pc) -> add s (Ev.Con_yield { con_id; pc }))
    r.vm.con_yields;
  Array.iter
    (fun (s, con_id, pc) -> add s (Ev.Con_resume { con_id; pc }))
    r.vm.con_resumes;
  Array.iter
    (fun (s, reg, value) -> add s (Ev.Reg_write { reg; value }))
    r.vm.reg_writes;
  Array.iter
    (fun (s, addr, size, tag) -> add s (Ev.Alloc { addr; size; tag }))
    r.heap.allocs;
  Array.iter (fun (s, addr) -> add s (Ev.Free { addr })) r.heap.frees;
  Array.iter (fun (s, addr) -> add s (Ev.Promote { addr })) r.heap.promotes;
  Array.iter
    (fun (s, addr, field) -> add s (Ev.Read { addr; field }))
    r.heap.reads;
  Array.iter
    (fun (s, addr, field, value) -> add s (Ev.Write { addr; field; value }))
    r.heap.writes;
  Array.iter (fun (s, event) -> add s (Ev.Gc { event })) r.gc.events;
  let sorted = List.sort (fun a b -> compare a.Ev.seq b.Ev.seq) !evs in
  List.iter (ingest t) sorted;
  t

let of_file path = of_read (Trace.Serializer.deserialize_from_file path)
