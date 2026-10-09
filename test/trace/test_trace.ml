let () =
  let open Mapv.Trace.Event in
  let bus = Mapv.Trace.Bus.create () in
  let rec_ = Mapv.Trace.Recorder.attach bus in
  Mapv.Trace.Bus.publish bus (Instr { pc = 0; op = 7 });
  Mapv.Trace.Bus.publish bus
    (Reg_write { reg = 3; value = Mapv.Core.Value.Int 42 });
  Mapv.Trace.Bus.publish bus (Alloc { addr = 100; size = 4; tag = 1 });
  Mapv.Trace.Bus.publish bus (Gc { event = Minor_start });
  Mapv.Trace.Bus.publish bus (Gc { event = Minor_end { promoted = 2 } });
  Mapv.Trace.Bus.publish bus (Instr { pc = 4; op = 9 });
  let bytes = Mapv.Trace.Serializer.serialize rec_ in
  let t = Mapv.Trace.Serializer.deserialize bytes in
  let module R = Mapv.Trace.Serializer.Read in
  assert (t.R.head = 5);
  assert (Array.length t.R.vm.instrs = 2);
  assert (t.R.vm.instrs.(0) = (0, 0, 7));
  assert (t.R.vm.instrs.(1) = (5, 4, 9));
  assert (Array.length t.R.vm.reg_writes = 1);
  assert (t.R.vm.reg_writes.(0) = (1, 3, Mapv.Core.Value.Int 42));
  assert (Array.length t.R.heap.allocs = 1);
  assert (t.R.heap.allocs.(0) = (2, 100, 4, 1));
  assert (Array.length t.R.gc.events = 2);
  assert (t.R.gc.events.(0) = (3, Mapv.Heap.Minor_start));
  assert (t.R.gc.events.(1) = (4, Mapv.Heap.Minor_end { promoted = 2 }));
  print_endline "trace round-trip ok"

let le_u32 buf v = Buffer.add_int32_le buf (Int32.of_int v)
let le_u16 buf v = Buffer.add_uint16_le buf v

let container ~version secs =
  let n = Array.length secs in
  let body_start = 8 + 4 + (n * 12) in
  let offs = Array.make n 0 in
  let pos = ref body_start in
  Array.iteri
    (fun i (_, b) ->
      let p = (8 - (!pos mod 8)) mod 8 in
      offs.(i) <- !pos + p;
      pos := offs.(i) + Bytes.length b)
    secs;
  let out = Buffer.create 256 in
  Buffer.add_string out "MAPVT";
  le_u16 out version;
  Buffer.add_uint8 out 0;
  le_u32 out n;
  Array.iteri
    (fun i (id, b) ->
      le_u32 out id;
      le_u32 out offs.(i);
      le_u32 out (Bytes.length b))
    secs;
  Array.iteri
    (fun i (_, b) ->
      let p = (8 - (Buffer.length out mod 8)) mod 8 in
      for _ = 1 to p do
        Buffer.add_char out '\x00'
      done;
      Buffer.add_bytes out b)
    secs;
  Buffer.to_bytes out

let v2_fixture () =
  let vm = Buffer.create 64 in
  le_u32 vm 1;
  le_u32 vm 0;
  le_u32 vm 5;
  Buffer.add_uint8 vm 7;
  Buffer.add_string vm "\x00\x00\x00";
  le_u32 vm 0;
  le_u32 vm 0;
  le_u32 vm 0;
  le_u32 vm 0;
  le_u32 vm 0;
  le_u32 vm 0;
  le_u32 vm 1;
  le_u32 vm 1;
  le_u32 vm 3;
  Buffer.add_uint8 vm 2;
  Buffer.add_int64_le vm (Int64.of_int 42);
  let heap = Buffer.create 48 in
  le_u32 heap 1;
  le_u32 heap 2;
  le_u32 heap 100;
  le_u32 heap 4;
  le_u32 heap 1;
  le_u32 heap 0;
  le_u32 heap 0;
  le_u32 heap 0;
  le_u32 heap 0;
  let gc = Buffer.create 16 in
  le_u32 gc 1;
  le_u32 gc 3;
  Buffer.add_uint8 gc 0;
  container ~version:2
    [|
      (0, Buffer.to_bytes vm); (1, Buffer.to_bytes heap); (2, Buffer.to_bytes gc);
    |]

let () =
  let t = Mapv.Trace.Serializer.deserialize (v2_fixture ()) in
  let module R = Mapv.Trace.Serializer.Read in
  assert (t.R.head = 3);
  assert (t.R.vm.instrs = [| (0, 5, 7) |]);
  assert (t.R.vm.reg_writes = [| (1, 3, Mapv.Core.Value.Int 42) |]);
  assert (t.R.heap.allocs = [| (2, 100, 4, 1) |]);
  assert (t.R.gc.events = [| (3, Mapv.Heap.Minor_start) |]);
  print_endline "v2 legacy read ok"

let () =
  let open Mapv.Trace.Event in
  let m = Mapv.Viz.Trace_model.create ~window:4 () in
  let ev seq kind = Mapv.Viz.Trace_model.ingest m { seq; kind } in
  ev 0 (Reg_write { reg = 1; value = Mapv.Core.Value.Int 7 });
  ev 1 (Instr { pc = 0; op = 1 });
  ev 2 (Call { pc = 1; target = 2 });
  ev 3 (Reg_write { reg = 1; value = Mapv.Core.Value.Int 9 });
  ev 4 (Instr { pc = 1; op = 1 });
  ev 5 (Instr { pc = 2; op = 1 });
  assert (Mapv.Viz.Trace_model.count m = 4);
  assert (Mapv.Viz.Trace_model.head m = 5);
  assert (Mapv.Viz.Trace_model.lo m = 2);
  assert (Mapv.Viz.Trace_model.reg_value_at m 5 1 = Some (Mapv.Core.Value.Int 9));
  assert (Mapv.Viz.Trace_model.reg_value_at m 2 1 = Some (Mapv.Core.Value.Int 7));
  assert (Mapv.Viz.Trace_model.call_depth m 5 = 1);
  print_endline "window eviction + snapshot ok"

let () =
  let open Mapv.Trace.Event in
  let m = Mapv.Viz.Trace_model.create ~window:4 () in
  let ev seq kind = Mapv.Viz.Trace_model.ingest m { seq; kind } in
  ev 0 (Call { pc = 0; target = 1 });
  for i = 1 to 5 do
    ev i (Instr { pc = i; op = 1 })
  done;
  assert (Mapv.Viz.Trace_model.call_depth m 5 = 1);
  print_endline "cross-window call depth ok"

let () =
  let open Mapv.Trace.Event in
  let m = Mapv.Viz.Trace_model.create ~bucket_size:2 ~max_buckets:2 () in
  for i = 0 to 19 do
    Mapv.Viz.Trace_model.ingest m { seq = i; kind = Instr { pc = i; op = 1 } }
  done;
  assert (List.length (Mapv.Viz.Trace_model.bucket_list m) <= 3);
  let tot = Mapv.Viz.Trace_model.totals m in
  assert (tot.Mapv.Viz.Trace_model.instr = 20);
  print_endline "archive aging ok"
