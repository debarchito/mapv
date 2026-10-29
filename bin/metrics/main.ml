(* Headless tracing-overhead benchmark.

   Emits four raw CSV dumps:

     metrics_runs.csv      one row per measured run (wide)
     metrics_fidelity.csv  one row per workload x sample rate x event kind
     metrics_actual.csv    ground-truth operation counts per workload/rate
     metrics_summary.csv   mean/median/std/min/max/n per run group

   The measured workloads are POST_DUMP style: execute the bytecode, keep the
   recorder in memory, then serialize.  Nothing in this harness starts the live
   visualizer. *)

open Mapv

(* ------------------------------------------------------------------ *)
(* Environment metadata *)
(* ------------------------------------------------------------------ *)

let run_cmd cmd =
  try
    let ic = Unix.open_process_in cmd in
    let line = try input_line ic with End_of_file -> "" in
    ignore (Unix.close_process_in ic);
    Some (String.trim line)
  with _ -> None

let read_line_prefix path prefix =
  try
    let ic = open_in path in
    let plen = String.length prefix in
    let rec go () =
      match input_line ic with
      | line ->
          if String.length line >= plen && String.sub line 0 plen = prefix then begin
            close_in ic;
            Some
              (String.trim
                 (String.sub line plen (String.length line - plen)))
          end
          else go ()
      | exception End_of_file ->
          close_in ic;
          None
    in
    go ()
  with _ -> None

let after_colon s =
  match String.index_opt s ':' with
  | Some i -> String.trim (String.sub s (i + 1) (String.length s - i - 1))
  | None -> String.trim s

let strip_quotes s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2)
  else s

type env = {
  git_commit : string;
  build_profile : string;
  ocaml_version : string;
  word_size : int;
  hostname : string;
  os : string;
  arch : string;
  cpu_model : string;
  nproc : int;
}

let capture_env () =
  let git_commit =
    match run_cmd "git rev-parse --short HEAD" with
    | Some s when s <> "" -> s
    | _ -> "unknown"
  in
  let build_profile =
    Sys.getenv_opt "DUNE_PROFILE" |> Option.value ~default:"unknown"
  in
  let cpu_model =
    match read_line_prefix "/proc/cpuinfo" "model name" with
    | Some s -> after_colon s
    | None -> "unknown"
  in
  let os =
    match read_line_prefix "/etc/os-release" "PRETTY_NAME=" with
    | Some s -> strip_quotes s
    | None -> Sys.os_type
  in
  let nproc =
    match run_cmd "nproc" with
    | Some s -> Option.value (int_of_string_opt s) ~default:0
    | None -> 0
  in
  {
    git_commit;
    build_profile;
    ocaml_version = Sys.ocaml_version;
    word_size = Sys.word_size;
    hostname = Unix.gethostname ();
    os;
    arch =
      (match run_cmd "uname -m" with
      | Some s when s <> "" -> s
      | _ -> "unknown");
    cpu_model;
    nproc;
  }

let timestamp_utc () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

(* ------------------------------------------------------------------ *)
(* Rows *)
(* ------------------------------------------------------------------ *)

type row = {
  run_id : int;
  timestamp_utc : string;
  phase : string;
  tracing : string;
  sample_rate : int;
  rep : int;
  wl : Workload.t;
  result : Measure.result;
  actual : Measure.actual;
  steps : int;
  trace_path : string;
}

type fidelity_row = {
  f_workload : string;
  f_sample_rate : int;
  f_event_kind : string;
  f_is_sampled : bool;
  f_actual_ops : int;
  f_traced_ops : int;
  f_expected_ops : float;
  f_capture_ratio : float;
  f_expected_ratio : float;
  f_fidelity_ratio : float;
  f_missing_ops : float;
  f_extra_ops : float;
  f_abs_error : float;
}

let header_runs =
  [
    "run_id";
    "timestamp_utc";
    "phase";
    "workload";
    "tracing";
    "sample_rate";
    "rep";
    "git_commit";
    "build_profile";
    "ocaml_version";
    "word_size";
    "hostname";
    "os";
    "arch";
    "cpu_model";
    "nproc";
    "outer_iters";
    "inner_iters";
    "alloc_size";
    "program_bytes";
    "heap_chunk_size";
    "heap_young_limit";
    "heap_max_chunks";
    "gc_major_threshold";
    "gc_major_growth_factor";
    "vm_call_depth";
    "vm_exception_depth";
    "tracing_metadata_bytes";
    "setup_ns";
    "exec_wall_ns";
    "exec_user_ns";
    "exec_sys_ns";
    "serialize_wall_ns";
    "serialize_user_ns";
    "serialize_sys_ns";
    "total_wall_ns";
    "vm_steps";
    "instr_per_sec";
    "events_per_sec";
    "rss_before_kb";
    "rss_after_kb";
    "rss_peak_kb";
    "rss_peak_delta_kb";
    "gc_allocated_bytes";
    "gc_minor_words";
    "gc_promoted_words";
    "gc_major_words";
    "gc_minor_collections";
    "gc_major_collections";
    "gc_forced_major_collections";
    "gc_compactions";
    "gc_heap_words";
    "gc_live_words";
    "gc_live_blocks";
    "gc_free_words";
    "gc_fragments";
    "gc_top_heap_words";
    "gc_heap_chunks";
    "gc_stack_size";
    "gc_live_stacks_words";
    "mapv_young_used";
    "mapv_young_total";
    "mapv_young_limit";
    "mapv_old_used";
    "mapv_old_total";
    "mapv_n_chunks";
    "mapv_alloc_count";
    "mapv_gc_minor_ns";
    "mapv_gc_major_ns";
    "mapv_gc_minor_count";
    "mapv_gc_major_count";
    "recorder_events";
    "trace_file_bytes";
    "trace_load_ns";
    "seq_head";
    "seq_gaps";
    "seq_duplicates";
    "roundtrip_ok";
    "ev_total";
    "ev_instr";
    "ev_call";
    "ev_ret";
    "ev_throw";
    "ev_con_new";
    "ev_con_yield";
    "ev_con_resume";
    "ev_reg_write";
    "ev_alloc";
    "ev_free";
    "ev_promote";
    "ev_read";
    "ev_write";
    "ev_gc";
    "ev_gc_minor_start";
    "ev_gc_minor_end";
    "ev_gc_major_mark";
    "ev_gc_major_sweep";
    "ev_gc_major_end";
    "actual_total";
    "actual_instr";
    "actual_call";
    "actual_ret";
    "actual_throw";
    "actual_con_new";
    "actual_con_yield";
    "actual_con_resume";
    "actual_reg_write";
    "actual_alloc";
    "actual_free";
    "actual_promote";
    "actual_read";
    "actual_write";
    "actual_gc";
    "actual_gc_minor_start";
    "actual_gc_minor_end";
    "actual_gc_major_mark";
    "actual_gc_major_sweep";
    "actual_gc_major_end";
    "actual_sampled_read";
    "actual_sampled_write";
    "actual_live_objects";
    "status";
    "trace_path";
  ]

let dim_assoc (r : row) (env : env) : (string * string) list =
  [
    ("run_id", string_of_int r.run_id);
    ("timestamp_utc", r.timestamp_utc);
    ("phase", r.phase);
    ("workload", r.wl.Workload.name);
    ("tracing", r.tracing);
    ("sample_rate", string_of_int r.sample_rate);
    ("rep", string_of_int r.rep);
    ("git_commit", env.git_commit);
    ("build_profile", env.build_profile);
    ("ocaml_version", env.ocaml_version);
    ("word_size", string_of_int env.word_size);
    ("hostname", env.hostname);
    ("os", env.os);
    ("arch", env.arch);
    ("cpu_model", env.cpu_model);
    ("nproc", string_of_int env.nproc);
    ("outer_iters", string_of_int r.wl.Workload.outer_iters);
    ("inner_iters", string_of_int r.wl.Workload.inner_iters);
    ("alloc_size", string_of_int r.wl.Workload.alloc_size);
    ("program_bytes", string_of_int r.result.Measure.program_bytes);
    ("heap_chunk_size", string_of_int r.wl.Workload.config.heap.chunk_size);
    ("heap_young_limit", string_of_int r.wl.Workload.config.heap.young_limit);
    ("heap_max_chunks", string_of_int r.wl.Workload.config.heap.max_chunks);
    ("gc_major_threshold", string_of_int r.wl.Workload.config.gc.major_threshold);
    ("gc_major_growth_factor", Csv.float r.wl.Workload.config.gc.major_growth_factor);
    ("vm_call_depth", string_of_int r.wl.Workload.config.vm.call_depth);
    ("vm_exception_depth", string_of_int r.wl.Workload.config.vm.exception_depth);
    ("tracing_metadata_bytes", string_of_int r.result.Measure.metadata_bytes);
    ("status", r.result.Measure.status);
    ("trace_path", r.trace_path);
  ]

let num_assoc (r : row) : (string * string) list =
  let res = r.result in
  let a = r.actual in
  let t = res.Measure.traced in
  let exec_s = Int64.to_float res.Measure.exec_ns /. 1e9 in
  let instr_per_sec =
    if exec_s > 0. then float r.steps /. exec_s else Float.nan
  in
  let events_per_sec =
    if exec_s > 0. then float t.Measure.total /. exec_s else Float.nan
  in
  [
    ("setup_ns", Int64.to_string res.Measure.setup_ns);
    ("exec_wall_ns", Int64.to_string res.Measure.exec_ns);
    ( "exec_user_ns",
      Int64.to_string (Int64.of_float (res.Measure.exec_user_s *. 1e9)) );
    ( "exec_sys_ns",
      Int64.to_string (Int64.of_float (res.Measure.exec_sys_s *. 1e9)) );
    ("serialize_wall_ns", Int64.to_string res.Measure.serialize_ns);
    ( "serialize_user_ns",
      Int64.to_string (Int64.of_float (res.Measure.serialize_user_s *. 1e9)) );
    ( "serialize_sys_ns",
      Int64.to_string (Int64.of_float (res.Measure.serialize_sys_s *. 1e9)) );
    ( "total_wall_ns",
      Int64.to_string
        (Int64.add res.Measure.setup_ns
           (Int64.add res.Measure.exec_ns res.Measure.serialize_ns)) );
    ("vm_steps", string_of_int r.steps);
    ("instr_per_sec", Csv.float instr_per_sec);
    ("events_per_sec", Csv.float events_per_sec);
    ("rss_before_kb", string_of_int res.Measure.rss_before_kb);
    ("rss_after_kb", string_of_int res.Measure.rss_after_kb);
    ("rss_peak_kb", string_of_int res.Measure.rss_peak_kb);
    ( "rss_peak_delta_kb",
      string_of_int (res.Measure.rss_peak_kb - res.Measure.rss_before_kb) );
    ("gc_allocated_bytes", Csv.float res.Measure.gc_allocated_bytes);
    ("gc_minor_words", Csv.float res.Measure.gc_minor_words);
    ("gc_promoted_words", Csv.float res.Measure.gc_promoted_words);
    ("gc_major_words", Csv.float res.Measure.gc_major_words);
    ("gc_minor_collections", string_of_int res.Measure.gc_minor_collections);
    ("gc_major_collections", string_of_int res.Measure.gc_major_collections);
    ( "gc_forced_major_collections",
      string_of_int res.Measure.gc_forced_major_collections );
    ("gc_compactions", string_of_int res.Measure.gc_compactions);
    ("gc_heap_words", string_of_int res.Measure.gc_heap_words);
    ("gc_live_words", string_of_int res.Measure.gc_live_words);
    ("gc_live_blocks", string_of_int res.Measure.gc_live_blocks);
    ("gc_free_words", string_of_int res.Measure.gc_free_words);
    ("gc_fragments", string_of_int res.Measure.gc_fragments);
    ("gc_top_heap_words", string_of_int res.Measure.gc_top_heap_words);
    ("gc_heap_chunks", string_of_int res.Measure.gc_heap_chunks);
    ("gc_stack_size", string_of_int res.Measure.gc_stack_size);
    ("gc_live_stacks_words", string_of_int res.Measure.gc_live_stacks_words);
    ("mapv_young_used", string_of_int res.Measure.mapv_young_used);
    ("mapv_young_total", string_of_int res.Measure.mapv_young_total);
    ("mapv_young_limit", string_of_int res.Measure.mapv_young_limit);
    ("mapv_old_used", string_of_int res.Measure.mapv_old_used);
    ("mapv_old_total", string_of_int res.Measure.mapv_old_total);
    ("mapv_n_chunks", string_of_int res.Measure.mapv_n_chunks);
    ("mapv_alloc_count", string_of_int res.Measure.mapv_alloc_count);
    ("mapv_gc_minor_ns", Int64.to_string res.Measure.mapv_gc_minor_ns);
    ("mapv_gc_major_ns", Int64.to_string res.Measure.mapv_gc_major_ns);
    ("mapv_gc_minor_count", string_of_int res.Measure.mapv_gc_minor_count);
    ("mapv_gc_major_count", string_of_int res.Measure.mapv_gc_major_count);
    ("recorder_events", string_of_int res.Measure.recorder_events);
    ("trace_file_bytes", string_of_int res.Measure.trace_file_bytes);
    ("trace_load_ns", Int64.to_string res.Measure.trace_load_ns);
    ("seq_head", string_of_int res.Measure.seq_head);
    ("seq_gaps", string_of_int res.Measure.seq_gaps);
    ("seq_duplicates", string_of_int res.Measure.seq_duplicates);
    ("roundtrip_ok", if res.Measure.roundtrip_ok then "true" else "false");
    ("ev_total", string_of_int t.Measure.total);
    ("ev_instr", string_of_int t.Measure.instr);
    ("ev_call", string_of_int t.Measure.call);
    ("ev_ret", string_of_int t.Measure.ret);
    ("ev_throw", string_of_int t.Measure.throw);
    ("ev_con_new", string_of_int t.Measure.con_new);
    ("ev_con_yield", string_of_int t.Measure.con_yield);
    ("ev_con_resume", string_of_int t.Measure.con_resume);
    ("ev_reg_write", string_of_int t.Measure.reg_write);
    ("ev_alloc", string_of_int t.Measure.alloc);
    ("ev_free", string_of_int t.Measure.free);
    ("ev_promote", string_of_int t.Measure.promote);
    ("ev_read", string_of_int t.Measure.read);
    ("ev_write", string_of_int t.Measure.write);
    ("ev_gc", string_of_int t.Measure.gc);
    ("ev_gc_minor_start", string_of_int t.Measure.gc_minor_start);
    ("ev_gc_minor_end", string_of_int t.Measure.gc_minor_end);
    ("ev_gc_major_mark", string_of_int t.Measure.gc_major_mark);
    ("ev_gc_major_sweep", string_of_int t.Measure.gc_major_sweep);
    ("ev_gc_major_end", string_of_int t.Measure.gc_major_end);
    ("actual_total", string_of_int a.Measure.a_total);
    ("actual_instr", string_of_int a.Measure.a_instr);
    ("actual_call", string_of_int a.Measure.a_call);
    ("actual_ret", string_of_int a.Measure.a_ret);
    ("actual_throw", string_of_int a.Measure.a_throw);
    ("actual_con_new", string_of_int a.Measure.a_con_new);
    ("actual_con_yield", string_of_int a.Measure.a_con_yield);
    ("actual_con_resume", string_of_int a.Measure.a_con_resume);
    ("actual_reg_write", string_of_int a.Measure.a_reg_write);
    ("actual_alloc", string_of_int a.Measure.a_alloc);
    ("actual_free", string_of_int a.Measure.a_free);
    ("actual_promote", string_of_int a.Measure.a_promote);
    ("actual_read", string_of_int a.Measure.a_read);
    ("actual_write", string_of_int a.Measure.a_write);
    ("actual_gc", string_of_int a.Measure.a_gc);
    ("actual_gc_minor_start", string_of_int a.Measure.a_gc_minor_start);
    ("actual_gc_minor_end", string_of_int a.Measure.a_gc_minor_end);
    ("actual_gc_major_mark", string_of_int a.Measure.a_gc_major_mark);
    ("actual_gc_major_sweep", string_of_int a.Measure.a_gc_major_sweep);
    ("actual_gc_major_end", string_of_int a.Measure.a_gc_major_end);
    ("actual_sampled_read", string_of_int a.Measure.a_sampled_read);
    ("actual_sampled_write", string_of_int a.Measure.a_sampled_write);
    ("actual_live_objects", string_of_int a.Measure.a_live_objects);
  ]

let row_assoc (r : row) (env : env) = dim_assoc r env @ num_assoc r

let row_values r env =
  let assoc = row_assoc r env in
  List.map (fun k -> List.assoc k assoc) header_runs

(* ------------------------------------------------------------------ *)
(* Fidelity *)
(* ------------------------------------------------------------------ *)

type kind_spec = {
  kind_name : string;
  kind_actual : Measure.actual -> int;
  kind_traced : Measure.counts -> int;
  kind_sampled : bool;
}

let event_kinds =
  [
    {
      kind_name = "instr";
      kind_actual = (fun a -> a.Measure.a_instr);
      kind_traced = (fun t -> t.Measure.instr);
      kind_sampled = false;
    };
    {
      kind_name = "call";
      kind_actual = (fun a -> a.Measure.a_call);
      kind_traced = (fun t -> t.Measure.call);
      kind_sampled = false;
    };
    {
      kind_name = "ret";
      kind_actual = (fun a -> a.Measure.a_ret);
      kind_traced = (fun t -> t.Measure.ret);
      kind_sampled = false;
    };
    {
      kind_name = "throw";
      kind_actual = (fun a -> a.Measure.a_throw);
      kind_traced = (fun t -> t.Measure.throw);
      kind_sampled = false;
    };
    {
      kind_name = "con_new";
      kind_actual = (fun a -> a.Measure.a_con_new);
      kind_traced = (fun t -> t.Measure.con_new);
      kind_sampled = false;
    };
    {
      kind_name = "con_yield";
      kind_actual = (fun a -> a.Measure.a_con_yield);
      kind_traced = (fun t -> t.Measure.con_yield);
      kind_sampled = false;
    };
    {
      kind_name = "con_resume";
      kind_actual = (fun a -> a.Measure.a_con_resume);
      kind_traced = (fun t -> t.Measure.con_resume);
      kind_sampled = false;
    };
    {
      kind_name = "reg_write";
      kind_actual = (fun a -> a.Measure.a_reg_write);
      kind_traced = (fun t -> t.Measure.reg_write);
      kind_sampled = false;
    };
    {
      kind_name = "alloc";
      kind_actual = (fun a -> a.Measure.a_alloc);
      kind_traced = (fun t -> t.Measure.alloc);
      kind_sampled = false;
    };
    {
      kind_name = "free";
      kind_actual = (fun a -> a.Measure.a_free);
      kind_traced = (fun t -> t.Measure.free);
      kind_sampled = false;
    };
    {
      kind_name = "promote";
      kind_actual = (fun a -> a.Measure.a_promote);
      kind_traced = (fun t -> t.Measure.promote);
      kind_sampled = false;
    };
    {
      kind_name = "read";
      kind_actual = (fun a -> a.Measure.a_read);
      kind_traced = (fun t -> t.Measure.read);
      kind_sampled = true;
    };
    {
      kind_name = "write";
      kind_actual = (fun a -> a.Measure.a_write);
      kind_traced = (fun t -> t.Measure.write);
      kind_sampled = true;
    };
    {
      kind_name = "gc";
      kind_actual = (fun a -> a.Measure.a_gc);
      kind_traced = (fun t -> t.Measure.gc);
      kind_sampled = false;
    };
    {
      kind_name = "gc_minor_start";
      kind_actual = (fun a -> a.Measure.a_gc_minor_start);
      kind_traced = (fun t -> t.Measure.gc_minor_start);
      kind_sampled = false;
    };
    {
      kind_name = "gc_minor_end";
      kind_actual = (fun a -> a.Measure.a_gc_minor_end);
      kind_traced = (fun t -> t.Measure.gc_minor_end);
      kind_sampled = false;
    };
    {
      kind_name = "gc_major_mark";
      kind_actual = (fun a -> a.Measure.a_gc_major_mark);
      kind_traced = (fun t -> t.Measure.gc_major_mark);
      kind_sampled = false;
    };
    {
      kind_name = "gc_major_sweep";
      kind_actual = (fun a -> a.Measure.a_gc_major_sweep);
      kind_traced = (fun t -> t.Measure.gc_major_sweep);
      kind_sampled = false;
    };
    {
      kind_name = "gc_major_end";
      kind_actual = (fun a -> a.Measure.a_gc_major_end);
      kind_traced = (fun t -> t.Measure.gc_major_end);
      kind_sampled = false;
    };
  ]

let fidelity_of ~workload ~sample_rate ~(a : Measure.actual)
    ~(t : Measure.counts) =
  let make spec =
    let actual_ops = spec.kind_actual a in
    let traced_ops = spec.kind_traced t in
    let expected_ops =
      if spec.kind_name = "read" then float a.Measure.a_sampled_read
      else if spec.kind_name = "write" then float a.Measure.a_sampled_write
      else float actual_ops
    in
    let traced_f = float traced_ops in
    let fidelity_ratio =
      if expected_ops = 0. then if traced_ops = 0 then 1.0 else Float.nan
      else traced_f /. expected_ops
    in
    {
      f_workload = workload;
      f_sample_rate = sample_rate;
      f_event_kind = spec.kind_name;
      f_is_sampled = spec.kind_sampled;
      f_actual_ops = actual_ops;
      f_traced_ops = traced_ops;
      f_expected_ops = expected_ops;
      f_capture_ratio =
        (if actual_ops = 0 then Float.nan
         else traced_f /. float actual_ops);
      f_expected_ratio =
        (if actual_ops = 0 then Float.nan
         else expected_ops /. float actual_ops);
      f_fidelity_ratio = fidelity_ratio;
      f_missing_ops = expected_ops -. traced_f;
      f_extra_ops = traced_f -. expected_ops;
      f_abs_error = Float.abs (traced_f -. expected_ops);
    }
  in
  let rows = List.map make event_kinds in
  let expected_total =
    float
      (a.Measure.a_total - a.Measure.a_read - a.Measure.a_write
     + a.Measure.a_sampled_read + a.Measure.a_sampled_write)
  in
  let traced_total = float t.Measure.total in
  let fidelity_ratio =
    if expected_total = 0. then
      if t.Measure.total = 0 then 1.0 else Float.nan
    else traced_total /. expected_total
  in
  let total_row =
    {
      f_workload = workload;
      f_sample_rate = sample_rate;
      f_event_kind = "total";
      f_is_sampled = false;
      f_actual_ops = a.Measure.a_total;
      f_traced_ops = t.Measure.total;
      f_expected_ops = expected_total;
      f_capture_ratio =
        (if a.Measure.a_total = 0 then Float.nan
         else traced_total /. float a.Measure.a_total);
      f_expected_ratio =
        (if a.Measure.a_total = 0 then Float.nan
         else expected_total /. float a.Measure.a_total);
      f_fidelity_ratio = fidelity_ratio;
      f_missing_ops = expected_total -. traced_total;
      f_extra_ops = traced_total -. expected_total;
      f_abs_error = Float.abs (traced_total -. expected_total);
    }
  in
  rows @ [ total_row ]

let header_fidelity =
  [
    "workload";
    "sample_rate";
    "event_kind";
    "is_sampled";
    "actual_ops";
    "traced_ops";
    "expected_ops";
    "capture_ratio";
    "expected_ratio";
    "fidelity_ratio";
    "missing_ops";
    "extra_ops";
    "abs_error";
  ]

let fidelity_values (r : fidelity_row) =
  [
    r.f_workload;
    string_of_int r.f_sample_rate;
    r.f_event_kind;
    (if r.f_is_sampled then "true" else "false");
    string_of_int r.f_actual_ops;
    string_of_int r.f_traced_ops;
    Csv.float r.f_expected_ops;
    Csv.float r.f_capture_ratio;
    Csv.float r.f_expected_ratio;
    Csv.float r.f_fidelity_ratio;
    Csv.float r.f_missing_ops;
    Csv.float r.f_extra_ops;
    Csv.float r.f_abs_error;
  ]

let header_actual =
  [
    "workload";
    "sample_rate";
    "total";
    "instr";
    "call";
    "ret";
    "throw";
    "con_new";
    "con_yield";
    "con_resume";
    "reg_write";
    "alloc";
    "free";
    "promote";
    "read";
    "write";
    "gc";
    "gc_minor_start";
    "gc_minor_end";
    "gc_major_mark";
    "gc_major_sweep";
    "gc_major_end";
    "sampled_read";
    "sampled_write";
    "live_objects";
    "count_exec_ns";
    "count_rss_peak_delta_kb";
    "count_gc_allocated_bytes";
  ]

let actual_values ~workload ~sample_rate ~(a : Measure.actual)
    ~(res : Measure.result) =
  [
    workload;
    string_of_int sample_rate;
    string_of_int a.Measure.a_total;
    string_of_int a.Measure.a_instr;
    string_of_int a.Measure.a_call;
    string_of_int a.Measure.a_ret;
    string_of_int a.Measure.a_throw;
    string_of_int a.Measure.a_con_new;
    string_of_int a.Measure.a_con_yield;
    string_of_int a.Measure.a_con_resume;
    string_of_int a.Measure.a_reg_write;
    string_of_int a.Measure.a_alloc;
    string_of_int a.Measure.a_free;
    string_of_int a.Measure.a_promote;
    string_of_int a.Measure.a_read;
    string_of_int a.Measure.a_write;
    string_of_int a.Measure.a_gc;
    string_of_int a.Measure.a_gc_minor_start;
    string_of_int a.Measure.a_gc_minor_end;
    string_of_int a.Measure.a_gc_major_mark;
    string_of_int a.Measure.a_gc_major_sweep;
    string_of_int a.Measure.a_gc_major_end;
    string_of_int a.Measure.a_sampled_read;
    string_of_int a.Measure.a_sampled_write;
    string_of_int a.Measure.a_live_objects;
    Int64.to_string res.Measure.exec_ns;
    string_of_int (res.Measure.rss_peak_kb - res.Measure.rss_before_kb);
    Csv.float res.Measure.gc_allocated_bytes;
  ]

(* ------------------------------------------------------------------ *)
(* Summary *)
(* ------------------------------------------------------------------ *)

let mean xs =
  match xs with
  | [] -> Float.nan
  | _ -> List.fold_left ( +. ) 0. xs /. float (List.length xs)

let median xs =
  let a = Array.of_list xs in
  Array.sort compare a;
  let n = Array.length a in
  if n = 0 then Float.nan
  else if n mod 2 = 1 then a.(n / 2)
  else (a.((n / 2) - 1) +. a.(n / 2)) /. 2.

let std xs =
  let n = List.length xs in
  if n < 2 then 0.
  else
    let m = mean xs in
    let ss =
      List.fold_left (fun acc x -> acc +. ((x -. m) ** 2.)) 0. xs
    in
    sqrt (ss /. float (n - 1))

let header_summary =
  [
    "phase";
    "workload";
    "tracing";
    "sample_rate";
    "metric";
    "n";
    "mean";
    "median";
    "std";
    "min";
    "max";
  ]

let summary_rows (rows : row list) =
  let tbl = Hashtbl.create 64 in
  let order = ref [] in
  List.iter
    (fun (r : row) ->
      if r.phase <> "warmup" then begin
        let key = (r.phase, r.wl.Workload.name, r.tracing, r.sample_rate) in
        if not (Hashtbl.mem tbl key) then order := key :: !order;
        let cur =
          match Hashtbl.find_opt tbl key with Some x -> x | None -> []
        in
        Hashtbl.replace tbl key (r :: cur)
      end)
    rows;
  let keys = List.sort compare !order in
  List.concat_map
    (fun key ->
      let phase, workload, tracing, sample_rate = key in
      let group = List.rev (Hashtbl.find tbl key) in
      let with_metrics =
        List.map (fun r -> (r, num_assoc r)) group
      in
      match with_metrics with
      | [] -> []
      | (_, first) :: _ ->
          let metric_names = List.map fst first in
          List.filter_map
            (fun metric ->
              let vals =
                List.filter_map
                  (fun (_, assoc) ->
                    match List.assoc_opt metric assoc with
                    | Some s -> float_of_string_opt s
                    | None -> None)
                  with_metrics
              in
              match vals with
              | [] -> None
              | _ ->
                  let n = List.length vals in
                  let mn = List.fold_left Float.min infinity vals in
                  let mx = List.fold_left Float.max neg_infinity vals in
                  Some
                    [
                      phase;
                      workload;
                      tracing;
                      string_of_int sample_rate;
                      metric;
                      string_of_int n;
                      Csv.float (mean vals);
                      Csv.float (median vals);
                      Csv.float (std vals);
                      Csv.float mn;
                      Csv.float mx;
                    ])
            metric_names)
    keys

(* ------------------------------------------------------------------ *)
(* Main *)
(* ------------------------------------------------------------------ *)

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" then ()
  else if Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let parse_rates s =
  let xs =
    String.split_on_char ',' s
    |> List.filter_map (fun x -> int_of_string_opt (String.trim x))
    |> List.filter (fun x -> x > 0)
  in
  match xs with [] -> [ 1 ] | _ -> xs

let () =
  let out_dir = ref "metrics" in
  let reps = ref 5 in
  let warmup = ref 1 in
  let no_gc_iters = ref 250_000 in
  let outer = ref 25 in
  let inner = ref 1000 in
  let alloc_size = ref 10 in
  let sample_rates_arg = ref "1,2,4,8" in
  let keep_traces = ref false in
  let args =
    [
      ( "--out-dir",
        Arg.Set_string out_dir,
        "Directory for the CSV dumps (default: metrics)" );
      ( "--reps",
        Arg.Set_int reps,
        "Measured repetitions per mode (default: 5)" );
      ( "--warmup",
        Arg.Set_int warmup,
        "Warmup repetitions per mode (default: 1)" );
      ( "--no-gc-iters",
        Arg.Set_int no_gc_iters,
        "Iterations for the non-allocating workload (default: 250000)" );
      ( "--outer",
        Arg.Set_int outer,
        "Outer iterations for the allocating workload (default: 25)" );
      ( "--inner",
        Arg.Set_int inner,
        "Inner iterations for the allocating workload (default: 1000)" );
      ( "--alloc-size",
        Arg.Set_int alloc_size,
        "Object size in the allocating workload (default: 10)" );
      ( "--sample-rates",
        Arg.Set_string sample_rates_arg,
        "Comma-separated heap read/write sample rates (default: 1,2,4,8)" );
      ( "--keep-traces",
        Arg.Set keep_traces,
        "Keep .mapvt trace files under <out-dir>/traces" );
    ]
  in
  Arg.parse args (fun _ -> ()) "Headless Mapv tracing metrics harness";
  if !reps < 1 then failwith "--reps must be >= 1";
  if !warmup < 0 then failwith "--warmup must be >= 0";
  if !no_gc_iters < 1 then failwith "--no-gc-iters must be >= 1";
  if !outer < 0 || !inner < 0 then failwith "--outer/--inner must be >= 0";
  if !alloc_size < 1 then failwith "--alloc-size must be >= 1";

  let env = capture_env () in
  let workloads =
    [
      Workload.make_no_gc ~iters:!no_gc_iters;
      Workload.make_gc_pressure ~outer:!outer ~inner:!inner
        ~alloc_size:!alloc_size;
    ]
  in
  let rates =
    let rs = parse_rates !sample_rates_arg in
    if List.mem 1 rs then rs else 1 :: rs
  in

  mkdir_p !out_dir;
  let traces_dir = Filename.concat !out_dir "traces" in
  if !keep_traces then mkdir_p traces_dir;

  let runs_csv =
    Csv.create (Filename.concat !out_dir "metrics_runs.csv") header_runs
  in
  let fidelity_csv =
    Csv.create (Filename.concat !out_dir "metrics_fidelity.csv")
      header_fidelity
  in
  let actual_csv =
    Csv.create (Filename.concat !out_dir "metrics_actual.csv") header_actual
  in
  let summary_csv =
    Csv.create (Filename.concat !out_dir "metrics_summary.csv")
      header_summary
  in

  let run_id = ref 0 in
  let rows : row list ref = ref [] in
  let fidelity : fidelity_row list ref = ref [] in
  let gt : (string * int, Measure.actual) Hashtbl.t = Hashtbl.create 16 in
  let gt_results : (Workload.t * int * Measure.result) list ref = ref [] in

  let run_one ~(wl : Workload.t) ~(mode : Measure.mode) ~sample_rate ~phase
      ~rep =
    let trace_path =
      if mode = Measure.On then
        if !keep_traces then
          Filename.concat traces_dir
            (Printf.sprintf "%s_%s_sr%d_rep%d.mapvt" wl.Workload.name phase
               sample_rate rep)
        else Filename.temp_file "mapv_trace" ".mapvt"
      else ""
    in
    let res =
      Measure.isolated (fun () ->
          match mode with
          | Measure.Off -> Measure.execute_off ~workload:wl
          | Measure.Count ->
              Measure.execute_count ~workload:wl ~sample_rate
          | Measure.On ->
              Measure.execute_on ~workload:wl ~sample_rate ~trace_path)
    in
    if mode = Measure.On && not !keep_traces then
      (try Sys.remove trace_path with _ -> ());
    (res, if !keep_traces && mode = Measure.On then trace_path else "")
  in

  let add_row ~phase ~tracing ~rep ~sample_rate ~(wl : Workload.t)
      ~(actual : Measure.actual) ~steps ~(res : Measure.result)
      ~trace_path =
    incr run_id;
    rows :=
      {
        run_id = !run_id;
        timestamp_utc = timestamp_utc ();
        phase;
        tracing;
        sample_rate;
        rep;
        wl;
        result = res;
        actual;
        steps;
        trace_path;
      }
      :: !rows
  in

  (* ---- ground truth: one counting run per workload x sample rate ---- *)
  List.iter
    (fun (wl : Workload.t) ->
      List.iter
        (fun rate ->
          Printf.eprintf "[metrics] ground truth %s sample_rate=%d\n%!"
            wl.Workload.name rate;
          let res, path =
            run_one ~wl ~mode:Measure.Count ~sample_rate:rate
              ~phase:"ground_truth" ~rep:0
          in
          Hashtbl.replace gt (wl.Workload.name, rate) res.Measure.actual;
          gt_results := (wl, rate, res) :: !gt_results;
          add_row ~phase:"ground_truth" ~tracing:"count" ~rep:0
            ~sample_rate:rate ~wl ~actual:res.Measure.actual
            ~steps:res.Measure.actual.Measure.a_instr ~res ~trace_path:path)
        rates)
    workloads;

  (* ---- performance phase: every rep in every mode, sample_rate=1 ---- *)
  List.iter
    (fun (wl : Workload.t) ->
      let gt1 =
        match Hashtbl.find_opt gt (wl.Workload.name, 1) with
        | Some a -> a
        | None ->
            failwith
              (Printf.sprintf "missing ground truth for %s"
                 wl.Workload.name)
      in
      for i = 0 to !warmup + !reps - 1 do
        let is_warmup = i < !warmup in
        let phase = if is_warmup then "warmup" else "measure" in
        let rep = if is_warmup then i else i - !warmup in
        Printf.eprintf "[metrics] perf %s %s rep=%d\n%!" wl.Workload.name
          phase rep;

        let r_off, p_off =
          run_one ~wl ~mode:Measure.Off ~sample_rate:1 ~phase ~rep
        in
        add_row ~phase ~tracing:"off" ~rep ~sample_rate:1 ~wl
          ~actual:gt1 ~steps:gt1.Measure.a_instr ~res:r_off ~trace_path:p_off;

        let r_count, p_count =
          run_one ~wl ~mode:Measure.Count ~sample_rate:1 ~phase ~rep
        in
        add_row ~phase ~tracing:"count" ~rep ~sample_rate:1 ~wl
          ~actual:r_count.Measure.actual
          ~steps:r_count.Measure.actual.Measure.a_instr ~res:r_count
          ~trace_path:p_count;

        let r_on, p_on =
          run_one ~wl ~mode:Measure.On ~sample_rate:1 ~phase ~rep
        in
        add_row ~phase ~tracing:"on" ~rep ~sample_rate:1 ~wl ~actual:gt1
          ~steps:r_on.Measure.traced.Measure.instr ~res:r_on
          ~trace_path:p_on
      done)
    workloads;

  (* ---- fidelity phase: traced run vs exact counting run ---- *)
  List.iter
    (fun (wl : Workload.t) ->
      List.iter
        (fun rate ->
          let gt_actual =
            match Hashtbl.find_opt gt (wl.Workload.name, rate) with
            | Some a -> a
            | None ->
                failwith
                  (Printf.sprintf "missing ground truth for %s rate %d"
                     wl.Workload.name rate)
          in
          Printf.eprintf "[metrics] fidelity %s sample_rate=%d\n%!"
            wl.Workload.name rate;
          let r_on, p_on =
            run_one ~wl ~mode:Measure.On ~sample_rate:rate
              ~phase:"fidelity" ~rep:0
          in
          add_row ~phase:"fidelity" ~tracing:"on" ~rep:0 ~sample_rate:rate
            ~wl ~actual:gt_actual
            ~steps:r_on.Measure.traced.Measure.instr ~res:r_on
            ~trace_path:p_on;
          fidelity :=
            List.rev_append
              (fidelity_of ~workload:wl.Workload.name ~sample_rate:rate
                 ~a:gt_actual ~t:r_on.Measure.traced)
              !fidelity)
        rates)
    workloads;

  let all_rows = List.rev !rows in
  List.iter (fun r -> Csv.write runs_csv (row_values r env)) all_rows;
  Csv.close runs_csv;

  List.iter (fun r -> Csv.write fidelity_csv (fidelity_values r)) (List.rev !fidelity);
  Csv.close fidelity_csv;

  List.iter
    (fun (wl, rate, res) ->
      Csv.write actual_csv
        (actual_values ~workload:wl.Workload.name ~sample_rate:rate
           ~a:res.Measure.actual ~res))
    (List.rev !gt_results);
  Csv.close actual_csv;

  List.iter (fun v -> Csv.write summary_csv v) (summary_rows all_rows);
  Csv.close summary_csv;

  Printf.eprintf
    "[metrics] wrote %s/{metrics_runs,metrics_fidelity,metrics_actual,metrics_summary}.csv\n%!"
    !out_dir
