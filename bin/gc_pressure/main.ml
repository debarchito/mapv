open Mapv
open Mapv.Core
open Mapv.Asm
open Mapv.Bytecode

let outer_iters = 25
let inner_iters = 1000

let make_gc_pressure () =
  let a = Asm.create () in
  let open Instr in
  (* A finite benchmark: each outer round churns through a batch of
     short-lived objects, then allocates one object that survives into the
     next round. This keeps both the minor and the major collector busy. *)
  Asm.label a "outer";
  Asm.emit a (Load (1, outer_iters)) |> ignore;
  Asm.label a "outer_loop";
  Asm.emit a (Load (0, 0)) |> ignore;
  Asm.emit a (Lte (3, 1, 0)) |> ignore;
  Asm.jnz a 3 "exit";
  Asm.emit a (Load (2, inner_iters)) |> ignore;
  Asm.label a "inner_loop";
  Asm.emit a (Lte (3, 2, 0)) |> ignore;
  Asm.jnz a 3 "inner_done";
  Asm.emit a (Alloc (4, Tag.user, 10)) |> ignore;
  Asm.emit a (Alloc (5, Tag.user, 10)) |> ignore;
  Asm.emit a (Alloc (6, Tag.user, 10)) |> ignore;
  Asm.emit a (SubI (2, 2, 1)) |> ignore;
  Asm.jmp a "inner_loop";
  Asm.label a "inner_done";
  Asm.emit a (Alloc (10, Tag.user, 5)) |> ignore;
  Asm.emit a (SetField (10, 0, 4)) |> ignore;
  Asm.emit a (Mov (4, 10)) |> ignore;
  Asm.emit a (SubI (1, 1, 1)) |> ignore;
  Asm.jmp a "outer_loop";
  Asm.label a "exit";
  Asm.emit a Halt |> ignore;
  Asm.link a

let () =
  let module H = Heap.Make (Heap.Tracing) in
  let module VM = Vm.Make (H) (Vm.Tracing) in
  let config =
    {
      Config.default with
      heap = { chunk_size = 1024; young_limit = 512; max_chunks = 2048 };
      gc = { major_threshold = 512; major_growth_factor = 1.5 };
    }
  in

  let gc_pressure = make_gc_pressure () in
  let flat, offsets =
    Loader.link
      [|
        {
          Serializer.name = "gc_pressure";
          arity = 0;
          code = gc_pressure.Asm.program;
        };
      |]
  in
  let entry = Loader.func_slice flat offsets 0 in
  Printf.printf "GC pressure bytecode: %d bytes\n%!" (Bytes.length entry);

  let session =
    Session.create (Session.Post_dump { path = "_gc_crush.mapvt" })
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
