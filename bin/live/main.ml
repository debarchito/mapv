open Mapv
open Mapv.Core
open Mapv.Bytecode
open Mapv.Asm
(**)
module H = Heap.Make (Heap.Tracing)
module VM = Vm.Make (H) (Vm.Tracing)
module Fold_math = Fold.Std.Fold_math.Make (H)
module Fold_random = Fold.Std.Fold_random.Make (H)

let make_program () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (LoadS (30, 30L)) |> ignore;
  emit a (LoadS (31, 33L)) |> ignore;
  emit a (Load  (1, 8)) |> ignore;
  emit a (DynCall (30, 1, 0, 0)) |> ignore;
  emit a (DynCall (31, 1, 1, 2)) |> ignore;
  emit a Halt |> ignore;
  link a

let () =
  let { Asm.program = code; _ } = make_program () in
  (* prepare the heap and vm context *)
  let config = Config.default in
  let heap_ctx =
    Mapv.Heap.Tracing.make ~max_chunks:config.heap.max_chunks
      ~chunk_size:config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Mapv.Vm.Tracing.make () in
  (* prepare the bytecode *)
  let flat, offsets =
    Mapv.Bytecode.Loader.link
      [| { Serializer.name = "main"; arity = 0; code } |]
  in
  (* load the main function *)
  let entry = Mapv.Bytecode.Loader.func_slice flat offsets 0 in
  let vm = VM.create config entry heap_ctx vm_ctx in
  let heap = VM.heap vm in
  let reg = VM.symbols vm in
  (* register the standard library *)
  Fold_math.register heap reg;
  Fold_random.register heap reg;
  (* set symbols *)
  VM.set_sym vm 30 (Symbol.sym reg "std/random/init");
  VM.set_sym vm 31 (Symbol.sym reg "std/random/int");
  VM.set_sym vm 32 (Symbol.sym reg "std/random/float");
  VM.set_sym vm 33 (Symbol.sym reg "std/random/gaussian");
  VM.set_sym vm 34 (Symbol.sym reg "std/random/exp");
  (* manage exceptions *)
  (try VM.run vm with
  | Exception.Panic p ->
      Printf.eprintf "PANIC: %s\n" (Exception.panic_to_string p);
      exit 1
  | Exception.Signal s ->
      let code, msg = Exception.signal_to_pair s in
      let name =
        Exception.Registry.name_of Exception.Registry.empty code
        |> Option.value ~default:"UnknownSignal"
      in
      Printf.eprintf "SIGNAL [%s (%d)]: %s\n" name code msg;
      exit 1
  | e ->
      Printf.eprintf "Exception: %s\n%s\n" (Printexc.to_string e)
        (Printexc.get_backtrace ());
      exit 1);
  (* trace and visualize *)
  (* Trace.Serializer.serialize_to_file "_live.mapvt" vm_ctx heap_ctx; *)
  (* Viz.Renderer.run "_live.mapvt"; *)
  (* show non-nil registers *)
  Printf.printf "Registers:\n";
  for i = 0 to 255 do
    match VM.get_reg vm i with
    | Value.Nil -> ()
    | v -> Printf.printf "  r%-3d %s\n" i (Value.to_string v)
  done;
