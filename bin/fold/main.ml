open Mapv
open Mapv.Core
open Mapv.Asm
open Mapv.Bytecode

let make_gc_pressure () =
  let a = Asm.create () in
  let open Instr in
  Asm.label a "loop";
  Asm.emit a (Load (1, 1000)) |> ignore;
  Asm.emit a (Load (11, 0)) |> ignore;
  Asm.emit a (Lt (3, 11, 1)) |> ignore;
  Asm.jz a 3 "exit";
  Asm.emit a (Alloc (4, 0, 10)) |> ignore;
  Asm.emit a (Alloc (5, 0, 10)) |> ignore;
  Asm.emit a (Alloc (6, 0, 10)) |> ignore;
  Asm.emit a (Load (2, 100000)) |> ignore;
  Asm.emit a (Alloc (12, 0, 10)) |> ignore;
  Asm.emit a (SetField (12, 0, 2)) |> ignore;
  Asm.emit a (SubI (2, 2, 1)) |> ignore;
  Asm.emit a (Lt (13, 11, 2)) |> ignore;
  Asm.jnz a 13 "loop";
  Asm.emit a (Alloc (10, 1, 5)) |> ignore;
  Asm.emit a (SetField (10, 0, 4)) |> ignore;
  Asm.emit a (Mov (4, 10)) |> ignore;
  Asm.emit a (Load (12, 2)) |> ignore;
  Asm.emit a (Mod (13, 1, 12)) |> ignore;
  Asm.emit a (Eq (13, 13, 11)) |> ignore;
  Asm.jz a 13 "exit";
  Asm.emit a (Alloc (14, 2, 500)) |> ignore;
  Asm.emit a (SubI (1, 1, 1)) |> ignore;
  Asm.jmp a "loop";
  Asm.label a "exit";
  Asm.emit a Halt |> ignore;
  Asm.link a

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

  let heap_ctx =
    Heap.Tracing.make ~max_chunks:config.heap.max_chunks
      ~chunk_size:config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Vm.Tracing.make () in
  let vm = VM.create config entry heap_ctx vm_ctx in

  (match VM.run vm with
  | () -> ()
  | exception e ->
      Printf.printf "VM Panic: %s\n" (Printexc.to_string e);
      exit 1);

  Trace.Serializer.serialize_to_file "_gc_crush.mapvt" vm_ctx heap_ctx;
  Viz.Renderer.run "_gc_crush.mapvt"
