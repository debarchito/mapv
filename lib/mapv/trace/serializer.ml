open Core

let magic = "MAPVT"
let version = 3
let version_legacy = 2
let sec_events = 0x00
let sec_vm = 0x00
let sec_heap = 0x01
let sec_gc = 0x02

module Ev = Event

module Write = struct
  let u8 buf v = Buffer.add_uint8 buf v
  let u16 buf v = Buffer.add_uint16_le buf v
  let u32 buf v = Buffer.add_int32_le buf (Int32.of_int v)
  let u64 buf v = Buffer.add_int64_le buf v

  let value buf = function
    | Value.Nil -> u8 buf 0
    | Value.Bool b ->
        u8 buf 1;
        u8 buf (if b then 1 else 0)
    | Value.Int n ->
        u8 buf 2;
        u64 buf (Int64.of_int n)
    | Value.Float f ->
        u8 buf 3;
        u64 buf (Int64.bits_of_float f)
    | Value.Ptr p ->
        u8 buf 4;
        u32 buf p
    | Value.NativeFun _ -> u8 buf 5
    | Value.NativePtr _ -> u8 buf 6

  let gc_event buf = function
    | Event.Minor_start -> u8 buf 0
    | Event.Minor_end { promoted } ->
        u8 buf 1;
        u32 buf promoted
    | Event.Major_mark { steps } ->
        u8 buf 2;
        u32 buf steps
    | Event.Major_sweep { steps; freed } ->
        u8 buf 3;
        u32 buf steps;
        u32 buf freed
    | Event.Major_end -> u8 buf 4

  let event buf (ev : Ev.t) =
    u32 buf ev.seq;
    match ev.kind with
    | Ev.Instr { pc; op } ->
        u8 buf 0;
        u32 buf pc;
        u8 buf op
    | Ev.Call { pc; target } ->
        u8 buf 1;
        u32 buf pc;
        u32 buf target
    | Ev.Ret { pc } ->
        u8 buf 2;
        u32 buf pc
    | Ev.Throw { pc } ->
        u8 buf 3;
        u32 buf pc
    | Ev.Con_new { pc; con_id = _ } ->
        u8 buf 4;
        u32 buf pc
    | Ev.Con_yield { con_id; pc } ->
        u8 buf 5;
        u32 buf con_id;
        u32 buf pc
    | Ev.Con_resume { con_id; pc } ->
        u8 buf 6;
        u32 buf con_id;
        u32 buf pc
    | Ev.Reg_write { reg; value = v } ->
        u8 buf 7;
        u32 buf reg;
        value buf v
    | Ev.Alloc { addr; size; tag } ->
        u8 buf 8;
        u32 buf addr;
        u32 buf size;
        u32 buf tag
    | Ev.Free { addr } ->
        u8 buf 9;
        u32 buf addr
    | Ev.Promote { addr } ->
        u8 buf 10;
        u32 buf addr
    | Ev.Read { addr; field } ->
        u8 buf 11;
        u32 buf addr;
        u32 buf field
    | Ev.Write { addr; field; value = v } ->
        u8 buf 12;
        u32 buf addr;
        u32 buf field;
        value buf v
    | Ev.Gc { event = ev } ->
        u8 buf 13;
        gc_event buf ev

  let events_section events =
    let buf = Buffer.create 4096 in
    u32 buf (List.length events);
    List.iter (event buf) events;
    buf

  let container secs =
    let n_secs = Array.length secs in
    let header_size = 5 + 2 + 1 in
    let table_size = 4 + (n_secs * 12) in
    let body_start = header_size + table_size in
    let offsets = Array.make n_secs 0 in
    let pos = ref body_start in
    Array.iteri
      (fun i (_, b) ->
        let padding = (8 - (!pos mod 8)) mod 8 in
        offsets.(i) <- !pos + padding;
        pos := offsets.(i) + Buffer.length b)
      secs;
    let out = Buffer.create 4096 in
    Buffer.add_string out magic;
    u16 out version;
    u8 out 0;
    u32 out n_secs;
    Array.iteri
      (fun i (id, b) ->
        u32 out id;
        u32 out offsets.(i);
        u32 out (Buffer.length b))
      secs;
    Array.iteri
      (fun i (_, b) ->
        let current_pos = Buffer.length out in
        let padding = (8 - (current_pos mod 8)) mod 8 in
        assert (Buffer.length out + padding = offsets.(i));
        for _ = 1 to padding do
          Buffer.add_char out '\x00'
        done;
        Buffer.add_buffer out b)
      secs;
    ignore offsets;
    out

  let program events = container [| (sec_events, events_section events) |]
end

module Read = struct
  type cursor = { data : bytes; mutable pos : int }

  let make data = { data; pos = 0 }
  let seek cur p = cur.pos <- p

  let u8 cur =
    let v = Bytes.get_uint8 cur.data cur.pos in
    cur.pos <- cur.pos + 1;
    v

  let u16 cur =
    let v = Bytes.get_uint16_le cur.data cur.pos in
    cur.pos <- cur.pos + 2;
    v

  let u32 cur =
    let v = Int32.to_int (Bytes.get_int32_le cur.data cur.pos) in
    cur.pos <- cur.pos + 4;
    v

  let u64 cur =
    let v = Bytes.get_int64_le cur.data cur.pos in
    cur.pos <- cur.pos + 8;
    v

  let value cur =
    match u8 cur with
    | 0 -> Value.Nil
    | 1 -> Value.Bool (u8 cur <> 0)
    | 2 -> Value.Int (Int64.to_int (u64 cur))
    | 3 -> Value.Float (Int64.float_of_bits (u64 cur))
    | 4 -> Value.Ptr (u32 cur)
    | 5 -> Value.Nil
    | 6 -> Value.Nil
    | _ -> Value.Nil

  type vm_trace = {
    instrs : (int * int * int) array;
    calls : (int * int * int) array;
    rets : (int * int) array;
    throws : (int * int) array;
    con_news : (int * int) array;
    con_yields : (int * int * int) array;
    con_resumes : (int * int * int) array;
    reg_writes : (int * int * Value.t) array;
  }

  type heap_trace = {
    allocs : (int * int * int * int) array;
    frees : (int * int) array;
    promotes : (int * int) array;
    reads : (int * int * int) array;
    writes : (int * int * int * Value.t) array;
  }

  type gc_trace = { events : (int * Event.gc) array }
  type t = { vm : vm_trace; heap : heap_trace; gc : gc_trace; head : int }

  let gc_event cur =
    match u8 cur with
    | 0 -> Event.Minor_start
    | 1 -> Event.Minor_end { promoted = u32 cur }
    | 2 -> Event.Major_mark { steps = u32 cur }
    | 3 ->
        let steps = u32 cur in
        let freed = u32 cur in
        Event.Major_sweep { steps; freed }
    | 4 -> Event.Major_end
    | _ -> Event.Major_end

  (* ---- v3: single event stream ---- *)

  let events_section cur =
    let n = u32 cur in
    let instrs = ref [] in
    let calls = ref [] in
    let rets = ref [] in
    let throws = ref [] in
    let con_news = ref [] in
    let con_yields = ref [] in
    let con_resumes = ref [] in
    let reg_writes = ref [] in
    let allocs = ref [] in
    let frees = ref [] in
    let promotes = ref [] in
    let reads = ref [] in
    let writes = ref [] in
    let events = ref [] in
    let head = ref (-1) in
    for _ = 1 to n do
      let seq = u32 cur in
      if seq > !head then head := seq;
      match u8 cur with
      | 0 ->
          let pc = u32 cur in
          let op = u8 cur in
          instrs := (seq, pc, op) :: !instrs
      | 1 ->
          let pc = u32 cur in
          let target = u32 cur in
          calls := (seq, pc, target) :: !calls
      | 2 -> rets := (seq, u32 cur) :: !rets
      | 3 -> throws := (seq, u32 cur) :: !throws
      | 4 -> con_news := (seq, u32 cur) :: !con_news
      | 5 ->
          let con_id = u32 cur in
          let pc = u32 cur in
          con_yields := (seq, con_id, pc) :: !con_yields
      | 6 ->
          let con_id = u32 cur in
          let pc = u32 cur in
          con_resumes := (seq, con_id, pc) :: !con_resumes
      | 7 ->
          let reg = u32 cur in
          let v = value cur in
          reg_writes := (seq, reg, v) :: !reg_writes
      | 8 ->
          let addr = u32 cur in
          let size = u32 cur in
          let tag = u32 cur in
          allocs := (seq, addr, size, tag) :: !allocs
      | 9 -> frees := (seq, u32 cur) :: !frees
      | 10 -> promotes := (seq, u32 cur) :: !promotes
      | 11 ->
          let addr = u32 cur in
          let field = u32 cur in
          reads := (seq, addr, field) :: !reads
      | 12 ->
          let addr = u32 cur in
          let field = u32 cur in
          let v = value cur in
          writes := (seq, addr, field, v) :: !writes
      | 13 -> events := (seq, gc_event cur) :: !events
      | tag ->
          raise
            (Exception.Panic
               (Exception.Alloc_error
                  (Format.asprintf "trace: unknown event tag %d" tag)))
    done;
    let arr l = Array.of_list (List.rev l) in
    let vm =
      {
        instrs = arr !instrs;
        calls = arr !calls;
        rets = arr !rets;
        throws = arr !throws;
        con_news = arr !con_news;
        con_yields = arr !con_yields;
        con_resumes = arr !con_resumes;
        reg_writes = arr !reg_writes;
      }
    in
    let heap =
      {
        allocs = arr !allocs;
        frees = arr !frees;
        promotes = arr !promotes;
        reads = arr !reads;
        writes = arr !writes;
      }
    in
    let gc = { events = arr !events } in
    { vm; heap; gc; head = !head }

  (* ---- v2: legacy multi-section ---- *)

  let vm_section cur =
    let n_instrs = u32 cur in
    let instrs =
      Array.init n_instrs (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          let op = u8 cur in
          cur.pos <- cur.pos + 3;
          (tick, pc, op))
    in
    let n_calls = u32 cur in
    let calls =
      Array.init n_calls (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          let target = u32 cur in
          (tick, pc, target))
    in
    let n_rets = u32 cur in
    let rets =
      Array.init n_rets (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          (tick, pc))
    in
    let n_throws = u32 cur in
    let throws =
      Array.init n_throws (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          (tick, pc))
    in
    let n_con_news = u32 cur in
    let con_news =
      Array.init n_con_news (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          (tick, pc))
    in
    let n_con_yields = u32 cur in
    let con_yields =
      Array.init n_con_yields (fun _ ->
          let tick = u32 cur in
          let con_id = u32 cur in
          let pc = u32 cur in
          (tick, con_id, pc))
    in
    let n_con_resumes = u32 cur in
    let con_resumes =
      Array.init n_con_resumes (fun _ ->
          let tick = u32 cur in
          let con_id = u32 cur in
          let pc = u32 cur in
          (tick, con_id, pc))
    in
    let n_reg_writes = u32 cur in
    let reg_writes =
      Array.init n_reg_writes (fun _ ->
          let tick = u32 cur in
          let reg = u32 cur in
          let v = value cur in
          (tick, reg, v))
    in
    {
      instrs;
      calls;
      rets;
      throws;
      con_news;
      con_yields;
      con_resumes;
      reg_writes;
    }

  let heap_section cur =
    let n_allocs = u32 cur in
    let allocs =
      Array.init n_allocs (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          let size = u32 cur in
          let tag = u32 cur in
          (tick, addr, size, tag))
    in
    let n_frees = u32 cur in
    let frees =
      Array.init n_frees (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          (tick, addr))
    in
    let n_promotes = u32 cur in
    let promotes =
      Array.init n_promotes (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          (tick, addr))
    in
    let n_reads = u32 cur in
    let reads =
      Array.init n_reads (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          let field = u32 cur in
          (tick, addr, field))
    in
    let n_writes = u32 cur in
    let writes =
      Array.init n_writes (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          let field = u32 cur in
          let v = value cur in
          (tick, addr, field, v))
    in
    { allocs; frees; promotes; reads; writes }

  let gc_section cur =
    let n = u32 cur in
    let events =
      Array.init n (fun _ ->
          let tick = u32 cur in
          let ev = gc_event cur in
          (tick, ev))
    in
    { events }

  let read_table cur =
    let n_sec = u32 cur in
    Array.init n_sec (fun _ ->
        let id = u32 cur in
        let off = u32 cur in
        let sz = u32 cur in
        (id, off, sz))

  let find_sec secs id =
    List.find_map
      (fun (i, o, _) -> if i = id then Some o else None)
      (Array.to_list secs)

  let require secs id name =
    match find_sec secs id with
    | Some o -> o
    | None ->
        raise
          (Exception.Panic
             (Exception.Alloc_error
                (Format.asprintf "trace: missing section '%s'" name)))

  let load_legacy cur secs =
    let vm_off = require secs sec_vm "vm" in
    seek cur vm_off;
    let vm = vm_section cur in
    let heap_off = require secs sec_heap "heap" in
    seek cur heap_off;
    let heap = heap_section cur in
    let gc_off = require secs sec_gc "gc" in
    seek cur gc_off;
    let gc = gc_section cur in
    let head = ref (-1) in
    let bump t = if t > !head then head := t in
    Array.iter (fun (t, _, _) -> bump t) vm.instrs;
    Array.iter (fun (t, _, _) -> bump t) vm.calls;
    Array.iter (fun (t, _) -> bump t) vm.rets;
    Array.iter (fun (t, _) -> bump t) vm.throws;
    Array.iter (fun (t, _) -> bump t) vm.con_news;
    Array.iter (fun (t, _, _) -> bump t) vm.con_yields;
    Array.iter (fun (t, _, _) -> bump t) vm.con_resumes;
    Array.iter (fun (t, _, _) -> bump t) vm.reg_writes;
    Array.iter (fun (t, _, _, _) -> bump t) heap.allocs;
    Array.iter (fun (t, _) -> bump t) heap.frees;
    Array.iter (fun (t, _) -> bump t) heap.promotes;
    Array.iter (fun (t, _, _) -> bump t) heap.reads;
    Array.iter (fun (t, _, _, _) -> bump t) heap.writes;
    Array.iter (fun (t, _) -> bump t) gc.events;
    { vm; heap; gc; head = !head }

  let load cur =
    let m = Bytes.sub_string cur.data cur.pos 5 in
    if m <> magic then
      raise
        (Exception.Panic
           (Exception.Alloc_error (Format.asprintf "trace: invalid magic %S" m)));
    cur.pos <- 5;
    let ver = u16 cur in
    cur.pos <- 8;
    let secs = read_table cur in
    match ver with
    | v when v = version_legacy -> load_legacy cur secs
    | v when v = version ->
        let off = require secs sec_events "events" in
        seek cur off;
        events_section cur
    | _ ->
        raise
          (Exception.Panic
             (Exception.Alloc_error
                (Format.asprintf "trace: unknown version %d" ver)))
end

let serialize_events events =
  let buf = Write.program events in
  Buffer.to_bytes buf

let serialize recorder = serialize_events (Recorder.stream recorder)

let serialize_to_file path recorder =
  let b = serialize recorder in
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_bytes oc b)

let deserialize data = Read.load (Read.make data)

let deserialize_from_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let b = Bytes.create n in
      really_input ic b 0 n;
      deserialize b)
