open Mapv
open Mapv.Core
open Mapv.Asm
open Mapv.Bytecode

let list_len = 24
let concat_tag = Tag.user

let make_program () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (Load (1, 0)) |> ignore;
  emit a (Load (2, 0)) |> ignore;
  label a "loop";
  emit a (Load (10, list_len)) |> ignore;
  call a "build" 10 10 11;
  emit a (Mov (3, 11)) |> ignore;
  emit a (Mov (4, 3)) |> ignore;
  emit a (Load (5, 0)) |> ignore;
  emit a (Load (6, list_len)) |> ignore;
  label a "walk";
  jz a 6 "walk_done";
  emit a (GetField (7, 4, 0)) |> ignore;
  emit a (Add (5, 5, 7)) |> ignore;
  emit a (GetField (4, 4, 1)) |> ignore;
  emit a (SubI (6, 6, 1)) |> ignore;
  jmp a "walk";
  label a "walk_done";
  emit a (Load (8, 3)) |> ignore;
  emit a (And (9, 1, 8)) |> ignore;
  jnz a 9 "skip_keep";
  emit a (Mov (2, 3)) |> ignore;
  label a "skip_keep";
  emit a (Alloc (12, concat_tag, 6)) |> ignore;
  emit a (SetField (12, 0, 2)) |> ignore;
  emit a (SetField (12, 1, 5)) |> ignore;
  emit a (LoadNil 12) |> ignore;
  emit a (LoadNil 3) |> ignore;
  emit a (LoadNil 4) |> ignore;
  emit a (LoadNil 7) |> ignore;
  emit a (AddI (1, 1, 1)) |> ignore;
  jmp a "loop";
  label a "exit";
  emit a Halt |> ignore;
  label a "build";
  jz a 0 "build_zero";
  emit a (Alloc (1, concat_tag, 2)) |> ignore;
  emit a (SetField (1, 0, 0)) |> ignore;
  emit a (SubI (4, 0, 1)) |> ignore;
  call a "build" 4 4 5;
  emit a (SetField (1, 1, 5)) |> ignore;
  emit a (Ret 1) |> ignore;
  label a "build_zero";
  emit a (LoadNil 1) |> ignore;
  emit a (Ret 1) |> ignore;
  link a

let () =
  let module H = Heap.Make (Heap.Tracing) in
  let module VM = Vm.Make (H) (Vm.Tracing) in
  let config =
    {
      Config.default with
      heap = { chunk_size = 256; young_limit = 64; max_chunks = 1024 };
      gc = { major_threshold = 64; major_growth_factor = 1.5 };
    }
  in
  let program = make_program () in
  let flat, offsets =
    Loader.link
      [|
        { Serializer.name = "live_gc"; arity = 0; code = program.Asm.program };
      |]
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
