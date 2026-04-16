open Mapv
open Mapv.Core
open Mapv.Bytecode
open Mapv.Asm
module H = Heap.Make (Heap.Tracing)
module VM = Vm.Make (H) (Vm.Tracing)
module Fold_buffer = Fold.Std.Fold_buffer.Make (H)
module Fold_string = Fold.Std.Fold_string.Make (H)
module Fold_vector = Fold.Std.Fold_vector.Make (H)
module Fold_record = Fold.Std.Fold_record.Make (H)
module Fold_math = Fold.Std.Fold_math.Make (H)
module Fold_random = Fold.Std.Fold_random.Make (H)
module Fold_raylib = Fold.Std.Fold_raylib.Make (H)

let register_stdlib heap reg =
  Fold_buffer.register heap reg;
  Fold_string.register heap reg;
  Fold_vector.register heap reg;
  Fold_record.register heap reg;
  Fold_math.register heap reg;
  Fold_random.register heap reg;
  Fold_raylib.register heap reg

let make_program ~samples_str ~inside_str ~pi_str () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (LoadS (31, 0L)) |> ignore;
  emit a (Load (1, 800)) |> ignore;
  emit a (Load (2, 600)) |> ignore;
  emit a (LoadS (3, 9L)) |> ignore;
  emit a (DynCall (31, 1, 3, 30)) |> ignore;
  emit a (LoadS (31, 1L)) |> ignore;
  emit a (Load (1, 18)) |> ignore;
  emit a (Load (2, 18)) |> ignore;
  emit a (Load (3, 18)) |> ignore;
  emit a (Load (4, 255)) |> ignore;
  emit a (DynCall (31, 1, 4, 50)) |> ignore;
  emit a (LoadS (31, 1L)) |> ignore;
  emit a (Load (1, 40)) |> ignore;
  emit a (Load (2, 40)) |> ignore;
  emit a (Load (3, 60)) |> ignore;
  emit a (Load (4, 255)) |> ignore;
  emit a (DynCall (31, 1, 4, 51)) |> ignore;
  emit a (LoadS (31, 1L)) |> ignore;
  emit a (Load (1, 255)) |> ignore;
  emit a (Load (2, 210)) |> ignore;
  emit a (Load (3, 0)) |> ignore;
  emit a (Load (4, 255)) |> ignore;
  emit a (DynCall (31, 1, 4, 52)) |> ignore;
  emit a (LoadS (31, 1L)) |> ignore;
  emit a (Load (1, 210)) |> ignore;
  emit a (Load (2, 210)) |> ignore;
  emit a (Load (3, 210)) |> ignore;
  emit a (Load (4, 255)) |> ignore;
  emit a (DynCall (31, 1, 4, 53)) |> ignore;
  emit a (LoadS (31, 1L)) |> ignore;
  emit a (Load (1, 255)) |> ignore;
  emit a (Load (2, 210)) |> ignore;
  emit a (Load (3, 0)) |> ignore;
  emit a (Load (4, 255)) |> ignore;
  emit a (DynCall (31, 1, 4, 54)) |> ignore;
  label a "loop";
  emit a (LoadS (31, 10L)) |> ignore;
  emit a (DynCall (31, 31, 31, 30)) |> ignore;
  emit a (LoadS (31, 2L)) |> ignore;
  emit a (DynCall (31, 31, 31, 30)) |> ignore;
  jnz a 30 "exit";
  emit a (LoadS (31, 20L)) |> ignore;
  emit a (DynCall (31, 31, 31, 30)) |> ignore;
  emit a (LoadS (31, 6L)) |> ignore;
  emit a (DynCall (31, 50, 50, 30)) |> ignore;
  emit a (LoadS (31, 8L)) |> ignore;
  emit a (Load (1, 100)) |> ignore;
  emit a (Load (2, 50)) |> ignore;
  emit a (Load (3, 500)) |> ignore;
  emit a (Load (4, 500)) |> ignore;
  emit a (Mov (5, 51)) |> ignore;
  emit a (DynCall (31, 1, 5, 30)) |> ignore;
  emit a (LoadS (31, 7L)) |> ignore;
  emit a (Load (1, 100)) |> ignore;
  emit a (Load (2, 550)) |> ignore;
  emit a (LoadF (3, 500.0)) |> ignore;
  emit a (Mov (4, 52)) |> ignore;
  emit a (DynCall (31, 1, 4, 30)) |> ignore;
  emit a (LoadS (31, 11L)) |> ignore;
  emit a (LoadS (1, 12L)) |> ignore;
  emit a (Load (2, 620)) |> ignore;
  emit a (Load (3, 50)) |> ignore;
  emit a (Load (4, 20)) |> ignore;
  emit a (Mov (5, 53)) |> ignore;
  emit a (DynCall (31, 1, 5, 30)) |> ignore;
  emit a (LoadS (31, 11L)) |> ignore;
  emit a (LoadS (1, 13L)) |> ignore;
  emit a (Load (2, 620)) |> ignore;
  emit a (Load (3, 80)) |> ignore;
  emit a (Load (4, 20)) |> ignore;
  emit a (Mov (5, 53)) |> ignore;
  emit a (DynCall (31, 1, 5, 30)) |> ignore;
  emit a (LoadS (31, 11L)) |> ignore;
  emit a (LoadS (1, 16L)) |> ignore;
  emit a (Load (2, 620)) |> ignore;
  emit a (Load (3, 110)) |> ignore;
  emit a (Load (4, 20)) |> ignore;
  emit a (Mov (5, 54)) |> ignore;
  emit a (DynCall (31, 1, 5, 30)) |> ignore;
  emit a (LoadS (31, 21L)) |> ignore;
  emit a (DynCall (31, 31, 31, 30)) |> ignore;
  jmp a "loop";
  label a "exit";
  emit a (LoadS (31, 5L)) |> ignore;
  emit a (DynCall (31, 31, 31, 30)) |> ignore;
  emit a Halt |> ignore;
  link a

let () =
  Random.self_init ();
  let samples = 100000 in
  let inside = ref 0 in
  for _ = 0 to samples - 1 do
    let x = Random.float 1.0 in
    let y = Random.float 1.0 in
    if (x *. x) +. (y *. y) <= 1.0 then incr inside
  done;
  let pi_est = 4.0 *. float_of_int !inside /. float_of_int samples in
  let pi = 4.0 *. Stdlib.atan 1.0 in
  Printf.printf "Samples: %d, Inside: %d\n" samples !inside;
  Printf.printf "Est: %.6f, Real: %.6f\n" pi_est pi;
  let samples_str = Printf.sprintf "Samples: %d" samples in
  let inside_str = Printf.sprintf "Inside: %d" !inside in
  let pi_str = Printf.sprintf "Pi ~ %.6f" pi_est in
  let { Asm.program = code; _ } =
    make_program ~samples_str ~inside_str ~pi_str ()
  in
  let config =
    {
      Config.default with
      heap =
        {
          chunk_size = 65536;
          young_limit = 16 * 1024 * 1024;
          max_chunks = 1024;
        };
    }
  in
  let heap_ctx =
    Mapv.Heap.Tracing.make ~max_chunks:config.heap.max_chunks
      ~chunk_size:config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Mapv.Vm.Tracing.make () in
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
  VM.set_sym vm 3 (sym "std/raylib/poll_input_events");
  VM.set_sym vm 4 (sym "std/raylib/end_drawing");
  VM.set_sym vm 5 (sym "std/raylib/close_window");
  VM.set_sym vm 6 (sym "std/raylib/clear_background");
  VM.set_sym vm 7 (sym "std/raylib/shapes/draw_circle_lines");
  VM.set_sym vm 8 (sym "std/raylib/shapes/draw_rect");
  VM.set_sym vm 9 (Fold_string.of_ocaml heap "Monte Carlo Pi");
  VM.set_sym vm 10 (sym "std/raylib/poll_input_events");
  VM.set_sym vm 11 (sym "std/raylib/draw_text");
  VM.set_sym vm 12 (Fold_string.of_ocaml heap samples_str);
  VM.set_sym vm 13 (Fold_string.of_ocaml heap inside_str);
  VM.set_sym vm 16 (Fold_string.of_ocaml heap pi_str);
  VM.set_sym vm 20 (sym "std/raylib/begin_drawing");
  VM.set_sym vm 21 (sym "std/raylib/end_drawing");
  try VM.run vm with
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
