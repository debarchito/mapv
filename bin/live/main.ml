open Mapv
open Mapv.Core
open Mapv.Asm
open Mapv.Bytecode

let make_program () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "start";
  emit a (Load (4, 0)) |> ignore;
  label a "loop";
  emit a (AddI (4, 4, 2)) |> ignore;
  jmp a "loop";
  link a

let () =
  let module H = Heap.Make (Heap.Tracing) in
  let module VM = Vm.Make (H) (Vm.Tracing) in
  let config =
    {
      Config.default with
      heap = { chunk_size = 128; young_limit = 256; max_chunks = 128 };
      gc = { major_threshold = 128; major_growth_factor = 1.1 };
    }
  in
  let program = make_program () in
  let flat, offsets =
    Loader.link
      [| { Serializer.name = "live"; arity = 0; code = program.Asm.program } |]
  in
  let entry = Loader.func_slice flat offsets 0 in
  let session =
    Session.create (Session.Live { window = 200_000; bucket_size = 1000 })
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
