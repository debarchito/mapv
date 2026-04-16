open Mapv
open Mapv.Core
open Mapv.Bytecode
open Mapv.Asm
module H = Heap.Make (Heap.Tracing)
module VM = Vm.Make (H) (Vm.Tracing)
module L = Loader.Make (H) (VM)
module Fold_buffer = Fold.Std.Fold_buffer.Make (H)
module Fold_string = Fold.Std.Fold_string.Make (H)
module Fold_vector = Fold.Std.Fold_vector.Make (H)
module Fold_record = Fold.Std.Fold_record.Make (H)
module Fold_raylib = Fold.Std.Fold_raylib.Make (H)

let register_stdlib heap reg =
  Fold_buffer.register heap reg;
  Fold_string.register heap reg;
  Fold_vector.register heap reg;
  Fold_record.register heap reg;
  Fold_raylib.register heap reg

let make_program () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (LoadS (1, 0L)) |> ignore;
  emit a (Load (2, 800)) |> ignore;
  emit a (Load (3, 450)) |> ignore;
  emit a (LoadS (4, 9L)) |> ignore;
  emit a (DynCall (1, 2, 4, 0)) |> ignore;
  label a "loop";
  emit a (LoadS (20, 10L)) |> ignore;
  emit a (DynCall (20, 21, 20, 0)) |> ignore;
  emit a (LoadS (20, 3L)) |> ignore;
  emit a (DynCall (20, 21, 20, 0)) |> ignore;
  emit a (LoadS (1, 1L)) |> ignore;
  emit a (Load (2, 30)) |> ignore;
  emit a (Load (3, 30)) |> ignore;
  emit a (Load (4, 30)) |> ignore;
  emit a (Load (5, 255)) |> ignore;
  emit a (DynCall (1, 2, 5, 20)) |> ignore;
  emit a (LoadS (1, 6L)) |> ignore;
  emit a (Mov (2, 20)) |> ignore;
  emit a (DynCall (1, 2, 2, 0)) |> ignore;
  emit a (LoadS (1, 1L)) |> ignore;
  emit a (Load (2, 255)) |> ignore;
  emit a (Load (3, 0)) |> ignore;
  emit a (Load (4, 0)) |> ignore;
  emit a (Load (5, 255)) |> ignore;
  emit a (DynCall (1, 2, 5, 21)) |> ignore;
  emit a (LoadS (1, 7L)) |> ignore;
  emit a (Load (2, 400)) |> ignore;
  emit a (Load (3, 225)) |> ignore;
  emit a (LoadF (4, 80.0)) |> ignore;
  emit a (Mov (5, 21)) |> ignore;
  emit a (DynCall (1, 2, 5, 0)) |> ignore;
  emit a (LoadS (1, 1L)) |> ignore;
  emit a (Load (2, 0)) |> ignore;
  emit a (Load (3, 0)) |> ignore;
  emit a (Load (4, 255)) |> ignore;
  emit a (Load (5, 255)) |> ignore;
  emit a (DynCall (1, 2, 5, 21)) |> ignore;
  emit a (LoadS (1, 8L)) |> ignore;
  emit a (Load (2, 50)) |> ignore;
  emit a (Load (3, 50)) |> ignore;
  emit a (Load (4, 200)) |> ignore;
  emit a (Load (5, 200)) |> ignore;
  emit a (Mov (6, 21)) |> ignore;
  emit a (DynCall (1, 2, 6, 0)) |> ignore;
  emit a (LoadS (20, 4L)) |> ignore;
  emit a (DynCall (20, 21, 20, 0)) |> ignore;
  jmp a "loop";
  label a "exit";
  emit a (LoadS (20, 5L)) |> ignore;
  emit a (DynCall (20, 21, 20, 0)) |> ignore;
  emit a Halt |> ignore;
  link a

let () =
  let { Asm.program = code; constants } = make_program () in
  let config =
    {
      Config.default with
      heap = { chunk_size = 4096; young_limit = 1024 * 1024; max_chunks = 256 };
    }
  in
  let heap_ctx =
    Mapv.Heap.Tracing.make ~max_chunks:config.heap.max_chunks
      ~chunk_size:config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Mapv.Vm.Tracing.make () in
  try
    let flat, offsets =
      Mapv.Bytecode.Loader.link
        [| { Serializer.name = "main"; arity = 0; code } |]
    in
    let entry = Mapv.Bytecode.Loader.func_slice flat offsets 0 in
    let vm = VM.create config entry heap_ctx vm_ctx in
    let heap = VM.heap vm in
    let reg = VM.symbols vm in
    register_stdlib heap reg;
    let sym name =
      match Symbol.resolve reg (Symbol.hash name) with
      | Some v -> v
      | None -> failwith ("Could not resolve symbol: " ^ name)
    in
    VM.set_sym vm 0 (sym "std/raylib/init_window");
    VM.set_sym vm 1 (sym "std/raylib/color/make");
    VM.set_sym vm 2 (sym "std/raylib/window_should_close");
    VM.set_sym vm 3 (sym "std/raylib/begin_drawing");
    VM.set_sym vm 4 (sym "std/raylib/end_drawing");
    VM.set_sym vm 5 (sym "std/raylib/close_window");
    VM.set_sym vm 6 (sym "std/raylib/clear_background");
    VM.set_sym vm 7 (sym "std/raylib/shapes/draw_circle");
    VM.set_sym vm 8 (sym "std/raylib/shapes/draw_rect");
    VM.set_sym vm 9 (Fold_string.of_ocaml heap "Shapes Demo");
    VM.set_sym vm 10 (sym "std/raylib/poll_input_events");
    VM.set_sym vm 11 (sym "std/raylib/draw_text");
    VM.set_sym vm 12 (Fold_string.of_ocaml heap "wsc=");
    VM.run vm
  with
  | Exception.Panic p ->
      Printf.eprintf "VM PANIC: %s\n" (Exception.panic_to_string p);
      exit 1
  | Exception.Signal s ->
      let code, msg = Exception.signal_to_pair s in
      let name =
        Exception.Registry.name_of Exception.Registry.empty code
        |> Option.value ~default:"UnknownSignal"
      in
      Printf.eprintf "VM SIGNAL [%s (code %d)]: %s\n" name code msg;
      exit 1
  | e ->
      Printf.eprintf "Native OCaml Exception: %s\n%s\n" (Printexc.to_string e)
        (Printexc.get_backtrace ());
      exit 1
