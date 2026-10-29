open Mapv
open Mapv.Core
open Mapv.Asm
open Mapv.Bytecode

let make_program () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "start";
  emit a (Load (5, 40)) |> ignore;
  emit a (LoadNil 0) |> ignore;
  emit a (LoadNil 1) |> ignore;
  emit a (LoadNil 2) |> ignore;
  emit a (LoadNil 3) |> ignore;
  label a "alloc_loop";
  emit a (Mov (3, 2)) |> ignore;
  emit a (Mov (2, 1)) |> ignore;
  emit a (Mov (1, 0)) |> ignore;
  emit a (Alloc (0, 1, 4)) |> ignore;
  emit a (SubI (5, 5, 1)) |> ignore;
  jnz a 5 "alloc_loop";
  emit a Halt |> ignore;
  link a

let () =
  let module H = Heap.Make (Heap.Tracing) in
  let module VM = Vm.Make (H) (Vm.Tracing) in
  let config =
    {
      Config.default with
      heap = { chunk_size = 128; young_limit = 256; max_chunks = 128 };
      gc = { major_threshold = 128; major_growth_factor = 1.5 };
    }
  in
  let program = make_program () in
  let flat, offsets =
    Loader.link
      [|
        {
          Serializer.name = "dump_gc_pressure";
          arity = 0;
          code = program.Asm.program;
        };
      |]
  in
  let entry = Loader.func_slice flat offsets 0 in
  let session =
    Session.create (Session.Post_dump { path = "_dump_gc_pressure.mapvt" })
  in
  let heap_ctx =
    Heap.Tracing.make ~max_chunks:config.heap.max_chunks
      ~chunk_size:config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Vm.Tracing.make () in
  Heap.Tracing.set_bus heap_ctx (Session.bus session);
  Vm.Tracing.set_bus vm_ctx (Session.bus session);
  let vm = VM.create config entry heap_ctx vm_ctx in
  Session.run session (module VM) vm
