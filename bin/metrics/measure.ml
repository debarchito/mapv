open Mapv
open Mapv.Core
module Heap = Mapv.Heap
module Vm = Mapv.Vm
module Trace = Mapv.Trace

type mode = Off | On | Count

let mode_name = function Off -> "off" | On -> "on" | Count -> "count"
let ns_of a b = Int64.of_float ((b -. a) *. 1e9)

(* Wall-clock time spent inside Mapv's collector.  [Minor_start]/[Minor_end]
   delimit a minor collection.  [Major_mark] is published after marking, so a
   major collection starts at the preceding [Minor_end] and ends at
   [Major_end]; this includes the mark and sweep work. *)
type gc_timer = {
  mutable minor_started : float option;
  mutable major_started : float option;
  mutable last_minor_end : float option;
  mutable minor_ns : int64;
  mutable major_ns : int64;
  mutable minor_count : int;
  mutable major_count : int;
}

let gc_timer_create () =
  {
    minor_started = None;
    major_started = None;
    last_minor_end = None;
    minor_ns = 0L;
    major_ns = 0L;
    minor_count = 0;
    major_count = 0;
  }

let gc_timer_note timer (ev : Trace.Event.gc) =
  match ev with
  | Trace.Event.Minor_start ->
      timer.last_minor_end <- None;
      if timer.minor_started = None then
        timer.minor_started <- Some (Unix.gettimeofday ())
  | Trace.Event.Minor_end _ ->
      let now = Unix.gettimeofday () in
      (match timer.minor_started with
      | Some started ->
          timer.minor_ns <- Int64.add timer.minor_ns (ns_of started now);
          timer.minor_count <- timer.minor_count + 1
      | None -> ());
      timer.minor_started <- None;
      timer.last_minor_end <- Some now
  | Trace.Event.Major_mark _ ->
      if timer.major_started = None then begin
        timer.major_started <-
          Some
            (match timer.last_minor_end with
            | Some t -> t
            | None -> Unix.gettimeofday ());
        timer.last_minor_end <- None
      end
  | Trace.Event.Major_sweep _ -> ()
  | Trace.Event.Major_end ->
      let now = Unix.gettimeofday () in
      (match timer.major_started with
      | Some started ->
          timer.major_ns <- Int64.add timer.major_ns (ns_of started now);
          timer.major_count <- timer.major_count + 1
      | None -> ());
      timer.major_started <- None;
      timer.last_minor_end <- None

module Heap_timed = struct
  type ctx = gc_timer

  let make () = gc_timer_create ()
  let on_alloc _ ~addr:_ ~size:_ ~tag:_ = ()
  let on_free _ ~addr:_ = ()
  let on_promote _ ~addr:_ = ()
  let on_gc timer ev = gc_timer_note timer ev
  let on_read _ ~addr:_ ~field:_ _ = ()
  let on_write _ ~addr:_ ~field:_ _ = ()
end

(* Event counts, either as observed in a serialized trace or as produced by the
   counting tracer.  GC sub-kinds are part of [gc] and must not be added to it
   again when computing [total]. *)
type counts = {
  total : int;
  instr : int;
  call : int;
  ret : int;
  throw : int;
  con_new : int;
  con_yield : int;
  con_resume : int;
  reg_write : int;
  alloc : int;
  free : int;
  promote : int;
  read : int;
  write : int;
  gc : int;
  gc_minor_start : int;
  gc_minor_end : int;
  gc_major_mark : int;
  gc_major_sweep : int;
  gc_major_end : int;
}

let zero_counts =
  {
    total = 0;
    instr = 0;
    call = 0;
    ret = 0;
    throw = 0;
    con_new = 0;
    con_yield = 0;
    con_resume = 0;
    reg_write = 0;
    alloc = 0;
    free = 0;
    promote = 0;
    read = 0;
    write = 0;
    gc = 0;
    gc_minor_start = 0;
    gc_minor_end = 0;
    gc_major_mark = 0;
    gc_major_sweep = 0;
    gc_major_end = 0;
  }

type actual = {
  a_total : int;
  a_instr : int;
  a_call : int;
  a_ret : int;
  a_throw : int;
  a_con_new : int;
  a_con_yield : int;
  a_con_resume : int;
  a_reg_write : int;
  a_alloc : int;
  a_free : int;
  a_promote : int;
  a_read : int;
  a_write : int;
  a_gc : int;
  a_gc_minor_start : int;
  a_gc_minor_end : int;
  a_gc_major_mark : int;
  a_gc_major_sweep : int;
  a_gc_major_end : int;
  a_sampled_read : int;
  a_sampled_write : int;
  a_live_objects : int;
}

let zero_actual =
  {
    a_total = 0;
    a_instr = 0;
    a_call = 0;
    a_ret = 0;
    a_throw = 0;
    a_con_new = 0;
    a_con_yield = 0;
    a_con_resume = 0;
    a_reg_write = 0;
    a_alloc = 0;
    a_free = 0;
    a_promote = 0;
    a_read = 0;
    a_write = 0;
    a_gc = 0;
    a_gc_minor_start = 0;
    a_gc_minor_end = 0;
    a_gc_major_mark = 0;
    a_gc_major_sweep = 0;
    a_gc_major_end = 0;
    a_sampled_read = 0;
    a_sampled_write = 0;
    a_live_objects = 0;
  }

type result = {
  setup_ns : int64;
  exec_ns : int64;
  serialize_ns : int64;
  exec_user_s : float;
  exec_sys_s : float;
  serialize_user_s : float;
  serialize_sys_s : float;
  rss_before_kb : int;
  rss_after_kb : int;
  rss_peak_kb : int;
  gc_allocated_bytes : float;
  gc_minor_words : float;
  gc_promoted_words : float;
  gc_major_words : float;
  gc_minor_collections : int;
  gc_major_collections : int;
  gc_forced_major_collections : int;
  gc_compactions : int;
  gc_heap_words : int;
  gc_live_words : int;
  gc_live_blocks : int;
  gc_free_words : int;
  gc_fragments : int;
  gc_top_heap_words : int;
  gc_heap_chunks : int;
  gc_stack_size : int;
  gc_live_stacks_words : int;
  mapv_young_used : int;
  mapv_young_total : int;
  mapv_young_limit : int;
  mapv_old_used : int;
  mapv_old_total : int;
  mapv_n_chunks : int;
  mapv_alloc_count : int;
  mapv_gc_minor_ns : int64;
  mapv_gc_major_ns : int64;
  mapv_gc_minor_count : int;
  mapv_gc_major_count : int;
  program_bytes : int;
  metadata_bytes : int;
  recorder_events : int;
  trace_file_bytes : int;
  trace_load_ns : int64;
  traced : counts;
  actual : actual;
  seq_head : int;
  seq_gaps : int;
  seq_duplicates : int;
  roundtrip_ok : bool;
  status : string;
}

(* ------------------------------------------------------------------ *)
(* Counting tracer: sees every operation, publishes nothing. *)
(* ------------------------------------------------------------------ *)

module Heap_counter = struct
  type ctx = {
    mutable alloc : int;
    mutable free : int;
    mutable promote : int;
    mutable gc : int;
    mutable gc_minor_start : int;
    mutable gc_minor_end : int;
    mutable gc_major_mark : int;
    mutable gc_major_sweep : int;
    mutable gc_major_end : int;
    mutable read : int;
    mutable write : int;
    sample_rate : int;
    mutable tick : int;
    mutable sampled_read : int;
    mutable sampled_write : int;
    timer : gc_timer;
  }

  let make ~sample_rate =
    {
      alloc = 0;
      free = 0;
      promote = 0;
      gc = 0;
      gc_minor_start = 0;
      gc_minor_end = 0;
      gc_major_mark = 0;
      gc_major_sweep = 0;
      gc_major_end = 0;
      read = 0;
      write = 0;
      sample_rate;
      tick = 0;
      sampled_read = 0;
      sampled_write = 0;
      timer = gc_timer_create ();
    }

  let on_alloc c ~addr:_ ~size:_ ~tag:_ = c.alloc <- c.alloc + 1
  let on_free c ~addr:_ = c.free <- c.free + 1
  let on_promote c ~addr:_ = c.promote <- c.promote + 1

  let on_gc c ev =
    gc_timer_note c.timer ev;
    c.gc <- c.gc + 1;
    match ev with
    | Trace.Event.Minor_start -> c.gc_minor_start <- c.gc_minor_start + 1
    | Trace.Event.Minor_end _ -> c.gc_minor_end <- c.gc_minor_end + 1
    | Trace.Event.Major_mark _ -> c.gc_major_mark <- c.gc_major_mark + 1
    | Trace.Event.Major_sweep _ -> c.gc_major_sweep <- c.gc_major_sweep + 1
    | Trace.Event.Major_end -> c.gc_major_end <- c.gc_major_end + 1

  (* Mirrors Heap.Tracing.on_read/on_write: one shared tick, incremented on
     every read and every write, publish when [tick mod sample_rate = 0]. *)
  let on_read c ~addr:_ ~field:_ _ =
    c.read <- c.read + 1;
    c.tick <- c.tick + 1;
    if c.tick mod c.sample_rate = 0 then c.sampled_read <- c.sampled_read + 1

  let on_write c ~addr:_ ~field:_ _ =
    c.write <- c.write + 1;
    c.tick <- c.tick + 1;
    if c.tick mod c.sample_rate = 0 then c.sampled_write <- c.sampled_write + 1
end

module Heap_count = Heap.Make (Heap_counter)

module Heap_tracing_timed = struct
  type ctx = { tracing : Heap.Tracing.ctx; timer : gc_timer }

  let make ~max_chunks ~chunk_size ~sample_rate =
    {
      tracing = Heap.Tracing.make ~max_chunks ~chunk_size ~sample_rate;
      timer = gc_timer_create ();
    }

  let set_bus ctx bus = Heap.Tracing.set_bus ctx.tracing bus

  let on_alloc ctx ~addr ~size ~tag =
    Heap.Tracing.on_alloc ctx.tracing ~addr ~size ~tag

  let on_free ctx ~addr = Heap.Tracing.on_free ctx.tracing ~addr
  let on_promote ctx ~addr = Heap.Tracing.on_promote ctx.tracing ~addr

  let on_gc ctx ev =
    gc_timer_note ctx.timer ev;
    Heap.Tracing.on_gc ctx.tracing ev

  let on_read ctx ~addr ~field v =
    Heap.Tracing.on_read ctx.tracing ~addr ~field v

  let on_write ctx ~addr ~field v =
    Heap.Tracing.on_write ctx.tracing ~addr ~field v
end

module Vm_counter = struct
  type ctx = {
    mutable instr : int;
    mutable call : int;
    mutable ret : int;
    mutable throw : int;
    mutable con_new : int;
    mutable con_yield : int;
    mutable con_resume : int;
    mutable reg_write : int;
    mutable con_count : int;
  }

  let make () =
    {
      instr = 0;
      call = 0;
      ret = 0;
      throw = 0;
      con_new = 0;
      con_yield = 0;
      con_resume = 0;
      reg_write = 0;
      con_count = 0;
    }

  let on_instr c ~pc:_ ~op:_ = c.instr <- c.instr + 1
  let on_call c ~pc:_ ~target:_ = c.call <- c.call + 1
  let on_ret c ~pc:_ = c.ret <- c.ret + 1
  let on_throw c ~pc:_ = c.throw <- c.throw + 1

  let on_con_new c ~pc:_ =
    let id = c.con_count in
    c.con_count <- id + 1;
    c.con_new <- c.con_new + 1;
    id

  let on_con_yield c ~con_id:_ ~pc:_ = c.con_yield <- c.con_yield + 1
  let on_con_resume c ~con_id:_ ~pc:_ = c.con_resume <- c.con_resume + 1
  let on_reg_write c ~reg:_ ~value:_ = c.reg_write <- c.reg_write + 1
end

module Vm_count = Vm.Make (Heap_count) (Vm_counter)

let actual_of_counters (h : Heap_counter.ctx) (v : Vm_counter.ctx) =
  let a_total =
    h.alloc + h.free + h.promote + h.read + h.write + h.gc + v.instr + v.call
    + v.ret + v.throw + v.con_new + v.con_yield + v.con_resume + v.reg_write
  in
  {
    a_total;
    a_instr = v.instr;
    a_call = v.call;
    a_ret = v.ret;
    a_throw = v.throw;
    a_con_new = v.con_new;
    a_con_yield = v.con_yield;
    a_con_resume = v.con_resume;
    a_reg_write = v.reg_write;
    a_alloc = h.alloc;
    a_free = h.free;
    a_promote = h.promote;
    a_read = h.read;
    a_write = h.write;
    a_gc = h.gc;
    a_gc_minor_start = h.gc_minor_start;
    a_gc_minor_end = h.gc_minor_end;
    a_gc_major_mark = h.gc_major_mark;
    a_gc_major_sweep = h.gc_major_sweep;
    a_gc_major_end = h.gc_major_end;
    a_sampled_read = h.sampled_read;
    a_sampled_write = h.sampled_write;
    a_live_objects = h.alloc - h.free;
  }

(* ------------------------------------------------------------------ *)
(* Trace inspection *)
(* ------------------------------------------------------------------ *)

let count_gc pred events =
  Array.fold_left (fun n (_, ev) -> if pred ev then n + 1 else n) 0 events

let counts_of_trace (t : Trace.Serializer.Read.t) =
  let open Trace.Event in
  let g = t.gc.events in
  let gc = Array.length g in
  let instr = Array.length t.vm.instrs in
  let call = Array.length t.vm.calls in
  let ret = Array.length t.vm.rets in
  let throw = Array.length t.vm.throws in
  let con_new = Array.length t.vm.con_news in
  let con_yield = Array.length t.vm.con_yields in
  let con_resume = Array.length t.vm.con_resumes in
  let reg_write = Array.length t.vm.reg_writes in
  let alloc = Array.length t.heap.allocs in
  let free = Array.length t.heap.frees in
  let promote = Array.length t.heap.promotes in
  let read = Array.length t.heap.reads in
  let write = Array.length t.heap.writes in
  {
    total =
      instr + call + ret + throw + con_new + con_yield + con_resume + reg_write
      + alloc + free + promote + read + write + gc;
    instr;
    call;
    ret;
    throw;
    con_new;
    con_yield;
    con_resume;
    reg_write;
    alloc;
    free;
    promote;
    read;
    write;
    gc;
    gc_minor_start = count_gc (function Minor_start -> true | _ -> false) g;
    gc_minor_end = count_gc (function Minor_end _ -> true | _ -> false) g;
    gc_major_mark = count_gc (function Major_mark _ -> true | _ -> false) g;
    gc_major_sweep = count_gc (function Major_sweep _ -> true | _ -> false) g;
    gc_major_end = count_gc (function Major_end -> true | _ -> false) g;
  }

let seq_stats (t : Trace.Serializer.Read.t) =
  let head = t.head in
  if head < 0 then (0, 0, 0)
  else begin
    let seen = Bytes.make (head + 1) '\000' in
    let dup = ref 0 in
    let mark s =
      if s >= 0 && s <= head then
        if Bytes.get seen s = '\001' then incr dup else Bytes.set seen s '\001'
    in
    Array.iter (fun (s, _, _) -> mark s) t.vm.instrs;
    Array.iter (fun (s, _, _) -> mark s) t.vm.calls;
    Array.iter (fun (s, _) -> mark s) t.vm.rets;
    Array.iter (fun (s, _) -> mark s) t.vm.throws;
    Array.iter (fun (s, _) -> mark s) t.vm.con_news;
    Array.iter (fun (s, _, _) -> mark s) t.vm.con_yields;
    Array.iter (fun (s, _, _) -> mark s) t.vm.con_resumes;
    Array.iter (fun (s, _, _) -> mark s) t.vm.reg_writes;
    Array.iter (fun (s, _, _, _) -> mark s) t.heap.allocs;
    Array.iter (fun (s, _) -> mark s) t.heap.frees;
    Array.iter (fun (s, _) -> mark s) t.heap.promotes;
    Array.iter (fun (s, _, _) -> mark s) t.heap.reads;
    Array.iter (fun (s, _, _, _) -> mark s) t.heap.writes;
    Array.iter (fun (s, _) -> mark s) t.gc.events;
    let gaps = ref 0 in
    for i = 0 to head do
      if Bytes.get seen i = '\000' then incr gaps
    done;
    (head, !gaps, !dup)
  end

(* ------------------------------------------------------------------ *)
(* Process / GC probes *)
(* ------------------------------------------------------------------ *)

let read_status_kb key =
  let ic = open_in "/proc/self/status" in
  let key_len = String.length key in
  let rec go () =
    match input_line ic with
    | line ->
        if String.length line >= key_len && String.sub line 0 key_len = key then begin
          let i = ref key_len in
          while
            !i < String.length line && (line.[!i] < '0' || line.[!i] > '9')
          do
            incr i
          done;
          let j = ref !i in
          while
            !j < String.length line && line.[!j] >= '0' && line.[!j] <= '9'
          do
            incr j
          done;
          let v =
            if !j > !i then int_of_string (String.sub line !i (!j - !i)) else 0
          in
          close_in ic;
          v
        end
        else go ()
    | exception End_of_file ->
        close_in ic;
        0
  in
  go ()

let mapv_of_heap (s : Heap.stats) =
  ( s.young_used,
    s.young_total,
    s.young_limit,
    s.old_used,
    s.old_total,
    s.n_chunks,
    s.alloc_count )

(* ------------------------------------------------------------------ *)
(* Runners *)
(* ------------------------------------------------------------------ *)

let execute_off ~workload =
  let module H = Heap.Make (Heap_timed) in
  let module VM = Vm.Make (H) (Vm.No_tracing) in
  let entry = Workload.entry workload in
  let program_bytes = Bytes.length entry in
  let cfg = workload.Workload.config in
  let rss_before = read_status_kb "VmRSS:" in
  let gc0 : Stdlib.Gc.stat = Stdlib.Gc.quick_stat () in
  let alloc0 = Stdlib.Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  let heap_ctx = Heap_timed.make () in
  let vm = VM.create cfg entry heap_ctx () in
  let t_setup = Unix.gettimeofday () in
  let c0 : Unix.process_times = Unix.times () in
  VM.run vm;
  let c1 : Unix.process_times = Unix.times () in
  let t_exec = Unix.gettimeofday () in
  let mapv = H.stats (VM.heap vm) in
  let status = VM.get_status vm in
  let rss_after = read_status_kb "VmRSS:" in
  let rss_peak = read_status_kb "VmHWM:" in
  let gc1 : Stdlib.Gc.stat = Stdlib.Gc.quick_stat () in
  let alloc1 = Stdlib.Gc.allocated_bytes () in
  let yu, yt, yl, ou, ot, nc, ac = mapv_of_heap mapv in
  {
    setup_ns = ns_of t0 t_setup;
    exec_ns = ns_of t_setup t_exec;
    serialize_ns = 0L;
    exec_user_s = c1.tms_utime -. c0.tms_utime;
    exec_sys_s = c1.tms_stime -. c0.tms_stime;
    serialize_user_s = 0.;
    serialize_sys_s = 0.;
    rss_before_kb = rss_before;
    rss_after_kb = rss_after;
    rss_peak_kb = rss_peak;
    gc_allocated_bytes = alloc1 -. alloc0;
    gc_minor_words = gc1.minor_words -. gc0.minor_words;
    gc_promoted_words = gc1.promoted_words -. gc0.promoted_words;
    gc_major_words = gc1.major_words -. gc0.major_words;
    gc_minor_collections = gc1.minor_collections - gc0.minor_collections;
    gc_major_collections = gc1.major_collections - gc0.major_collections;
    gc_forced_major_collections =
      gc1.forced_major_collections - gc0.forced_major_collections;
    gc_compactions = gc1.compactions - gc0.compactions;
    gc_heap_words = gc1.heap_words;
    gc_live_words = gc1.live_words;
    gc_live_blocks = gc1.live_blocks;
    gc_free_words = gc1.free_words;
    gc_fragments = gc1.fragments;
    gc_top_heap_words = gc1.top_heap_words;
    gc_heap_chunks = gc1.heap_chunks;
    gc_stack_size = gc1.stack_size;
    gc_live_stacks_words = gc1.live_stacks_words;
    mapv_young_used = yu;
    mapv_young_total = yt;
    mapv_young_limit = yl;
    mapv_old_used = ou;
    mapv_old_total = ot;
    mapv_n_chunks = nc;
    mapv_alloc_count = ac;
    mapv_gc_minor_ns = heap_ctx.minor_ns;
    mapv_gc_major_ns = heap_ctx.major_ns;
    mapv_gc_minor_count = heap_ctx.minor_count;
    mapv_gc_major_count = heap_ctx.major_count;
    program_bytes;
    metadata_bytes = 0;
    recorder_events = 0;
    trace_file_bytes = 0;
    trace_load_ns = 0L;
    traced = zero_counts;
    actual = zero_actual;
    seq_head = -1;
    seq_gaps = 0;
    seq_duplicates = 0;
    roundtrip_ok = false;
    status;
  }

let execute_count ~workload ~sample_rate =
  let module H = Heap.Make (Heap_counter) in
  let module VM = Vm.Make (H) (Vm_counter) in
  let entry = Workload.entry workload in
  let program_bytes = Bytes.length entry in
  let cfg = workload.Workload.config in
  let rss_before = read_status_kb "VmRSS:" in
  let gc0 : Stdlib.Gc.stat = Stdlib.Gc.quick_stat () in
  let alloc0 = Stdlib.Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  let heap_ctx = Heap_counter.make ~sample_rate in
  let vm_ctx = Vm_counter.make () in
  let vm = VM.create cfg entry heap_ctx vm_ctx in
  let t_setup = Unix.gettimeofday () in
  let c0 : Unix.process_times = Unix.times () in
  VM.run vm;
  let c1 : Unix.process_times = Unix.times () in
  let t_exec = Unix.gettimeofday () in
  let mapv = H.stats (VM.heap vm) in
  let status = VM.get_status vm in
  let rss_after = read_status_kb "VmRSS:" in
  let rss_peak = read_status_kb "VmHWM:" in
  let gc1 : Stdlib.Gc.stat = Stdlib.Gc.quick_stat () in
  let alloc1 = Stdlib.Gc.allocated_bytes () in
  let actual = actual_of_counters heap_ctx vm_ctx in
  let yu, yt, yl, ou, ot, nc, ac = mapv_of_heap mapv in
  {
    setup_ns = ns_of t0 t_setup;
    exec_ns = ns_of t_setup t_exec;
    serialize_ns = 0L;
    exec_user_s = c1.tms_utime -. c0.tms_utime;
    exec_sys_s = c1.tms_stime -. c0.tms_stime;
    serialize_user_s = 0.;
    serialize_sys_s = 0.;
    rss_before_kb = rss_before;
    rss_after_kb = rss_after;
    rss_peak_kb = rss_peak;
    gc_allocated_bytes = alloc1 -. alloc0;
    gc_minor_words = gc1.minor_words -. gc0.minor_words;
    gc_promoted_words = gc1.promoted_words -. gc0.promoted_words;
    gc_major_words = gc1.major_words -. gc0.major_words;
    gc_minor_collections = gc1.minor_collections - gc0.minor_collections;
    gc_major_collections = gc1.major_collections - gc0.major_collections;
    gc_forced_major_collections =
      gc1.forced_major_collections - gc0.forced_major_collections;
    gc_compactions = gc1.compactions - gc0.compactions;
    gc_heap_words = gc1.heap_words;
    gc_live_words = gc1.live_words;
    gc_live_blocks = gc1.live_blocks;
    gc_free_words = gc1.free_words;
    gc_fragments = gc1.fragments;
    gc_top_heap_words = gc1.top_heap_words;
    gc_heap_chunks = gc1.heap_chunks;
    gc_stack_size = gc1.stack_size;
    gc_live_stacks_words = gc1.live_stacks_words;
    mapv_young_used = yu;
    mapv_young_total = yt;
    mapv_young_limit = yl;
    mapv_old_used = ou;
    mapv_old_total = ot;
    mapv_n_chunks = nc;
    mapv_alloc_count = ac;
    mapv_gc_minor_ns = heap_ctx.timer.minor_ns;
    mapv_gc_major_ns = heap_ctx.timer.major_ns;
    mapv_gc_minor_count = heap_ctx.timer.minor_count;
    mapv_gc_major_count = heap_ctx.timer.major_count;
    program_bytes;
    metadata_bytes = 0;
    recorder_events = 0;
    trace_file_bytes = 0;
    trace_load_ns = 0L;
    traced = zero_counts;
    actual;
    seq_head = -1;
    seq_gaps = 0;
    seq_duplicates = 0;
    roundtrip_ok = false;
    status;
  }

let execute_on ~workload ~sample_rate ~trace_path =
  let module H = Heap.Make (Heap_tracing_timed) in
  let module VM = Vm.Make (H) (Vm.Tracing) in
  let entry = Workload.entry workload in
  let program_bytes = Bytes.length entry in
  let cfg = workload.Workload.config in
  let metadata_bytes = cfg.heap.max_chunks * cfg.heap.chunk_size in
  let rss_before = read_status_kb "VmRSS:" in
  let gc0 : Stdlib.Gc.stat = Stdlib.Gc.quick_stat () in
  let alloc0 = Stdlib.Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  let heap_ctx =
    Heap_tracing_timed.make ~max_chunks:cfg.heap.max_chunks
      ~chunk_size:cfg.heap.chunk_size ~sample_rate
  in
  let vm_ctx = Vm.Tracing.make () in
  let bus = Trace.Bus.create () in
  let rec_ = Trace.Recorder.attach bus in
  Heap_tracing_timed.set_bus heap_ctx bus;
  Vm.Tracing.set_bus vm_ctx bus;
  let vm = VM.create cfg entry heap_ctx vm_ctx in
  let t_setup = Unix.gettimeofday () in
  let c0 : Unix.process_times = Unix.times () in
  VM.run vm;
  let c1 : Unix.process_times = Unix.times () in
  let t_exec = Unix.gettimeofday () in
  let recorder_events = Trace.Recorder.count rec_ in
  let mapv = H.stats (VM.heap vm) in
  let status = VM.get_status vm in
  let t_ser0 = Unix.gettimeofday () in
  let cs0 : Unix.process_times = Unix.times () in
  Trace.Serializer.serialize_to_file trace_path rec_;
  let cs1 : Unix.process_times = Unix.times () in
  let t_ser1 = Unix.gettimeofday () in
  let trace_file_bytes = (Unix.stat trace_path).st_size in
  let rss_after = read_status_kb "VmRSS:" in
  let rss_peak = read_status_kb "VmHWM:" in
  let gc1 : Stdlib.Gc.stat = Stdlib.Gc.quick_stat () in
  let alloc1 = Stdlib.Gc.allocated_bytes () in
  let t_load0 = Unix.gettimeofday () in
  let traced, seq_head, seq_gaps, seq_duplicates, roundtrip_ok =
    try
      let t = Trace.Serializer.deserialize_from_file trace_path in
      let traced = counts_of_trace t in
      let h, g, d = seq_stats t in
      (traced, h, g, d, recorder_events = traced.total)
    with _ -> (zero_counts, -1, 0, 0, false)
  in
  let t_load1 = Unix.gettimeofday () in
  let yu, yt, yl, ou, ot, nc, ac = mapv_of_heap mapv in
  {
    setup_ns = ns_of t0 t_setup;
    exec_ns = ns_of t_setup t_exec;
    serialize_ns = ns_of t_ser0 t_ser1;
    exec_user_s = c1.tms_utime -. c0.tms_utime;
    exec_sys_s = c1.tms_stime -. c0.tms_stime;
    serialize_user_s = cs1.tms_utime -. cs0.tms_utime;
    serialize_sys_s = cs1.tms_stime -. cs0.tms_stime;
    rss_before_kb = rss_before;
    rss_after_kb = rss_after;
    rss_peak_kb = rss_peak;
    gc_allocated_bytes = alloc1 -. alloc0;
    gc_minor_words = gc1.minor_words -. gc0.minor_words;
    gc_promoted_words = gc1.promoted_words -. gc0.promoted_words;
    gc_major_words = gc1.major_words -. gc0.major_words;
    gc_minor_collections = gc1.minor_collections - gc0.minor_collections;
    gc_major_collections = gc1.major_collections - gc0.major_collections;
    gc_forced_major_collections =
      gc1.forced_major_collections - gc0.forced_major_collections;
    gc_compactions = gc1.compactions - gc0.compactions;
    gc_heap_words = gc1.heap_words;
    gc_live_words = gc1.live_words;
    gc_live_blocks = gc1.live_blocks;
    gc_free_words = gc1.free_words;
    gc_fragments = gc1.fragments;
    gc_top_heap_words = gc1.top_heap_words;
    gc_heap_chunks = gc1.heap_chunks;
    gc_stack_size = gc1.stack_size;
    gc_live_stacks_words = gc1.live_stacks_words;
    mapv_young_used = yu;
    mapv_young_total = yt;
    mapv_young_limit = yl;
    mapv_old_used = ou;
    mapv_old_total = ot;
    mapv_n_chunks = nc;
    mapv_alloc_count = ac;
    mapv_gc_minor_ns = heap_ctx.timer.minor_ns;
    mapv_gc_major_ns = heap_ctx.timer.major_ns;
    mapv_gc_minor_count = heap_ctx.timer.minor_count;
    mapv_gc_major_count = heap_ctx.timer.major_count;
    program_bytes;
    metadata_bytes;
    recorder_events;
    trace_file_bytes;
    trace_load_ns = ns_of t_load0 t_load1;
    traced;
    actual = zero_actual;
    seq_head;
    seq_gaps;
    seq_duplicates;
    roundtrip_ok;
    status;
  }

(* ------------------------------------------------------------------ *)
(* Fork isolation *)
(* ------------------------------------------------------------------ *)

type child_msg = Done of result | Failed of string

let describe_status = function
  | Unix.WEXITED n -> Printf.sprintf "exited %d" n
  | Unix.WSIGNALED n -> Printf.sprintf "was killed by signal %d" n
  | Unix.WSTOPPED n -> Printf.sprintf "was stopped by signal %d" n

let isolated f =
  let path = Filename.temp_file "mapv_metrics" ".msg" in
  flush stdout;
  flush stderr;
  match Unix.fork () with
  | 0 ->
      let msg =
        try Done (f ())
        with e ->
          Failed
            (Printf.sprintf "%s\n%s" (Printexc.to_string e)
               (Printexc.get_backtrace ()))
      in
      let oc = open_out_bin path in
      Marshal.to_channel oc msg [];
      close_out oc;
      Unix._exit 0
  | pid -> (
      let _, st = Unix.waitpid [] pid in
      let msg =
        if st = Unix.WEXITED 0 then begin
          let ic = open_in_bin path in
          let m : child_msg = Marshal.from_channel ic in
          close_in ic;
          m
        end
        else Failed (Printf.sprintf "metrics child %s" (describe_status st))
      in
      (try Sys.remove path with _ -> ());
      match msg with Done r -> r | Failed s -> failwith s)
