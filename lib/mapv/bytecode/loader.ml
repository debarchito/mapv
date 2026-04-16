open Core

exception Load_error of string

let load_error fmt = Format.kasprintf (fun s -> raise (Load_error s)) fmt

let encode_func (f : Serializer.func) : bytes =
  let buf = Buffer.create 64 in
  let offsets = Serializer.Write.compute_byte_offsets f.code in
  Array.iter (fun i -> Serializer.Write.instr buf offsets i) f.code;
  Buffer.to_bytes buf

let link (funcs : Serializer.func array) : bytes * int array =
  let chunks = Array.map encode_func funcs in
  let offsets = Array.make (Array.length funcs) 0 in
  let total = Array.fold_left (fun acc b -> acc + Bytes.length b) 0 chunks in
  let flat = Bytes.make total '\x00' in
  let pos = ref 0 in
  Array.iteri
    (fun i chunk ->
      offsets.(i) <- !pos;
      Bytes.blit chunk 0 flat !pos (Bytes.length chunk);
      pos := !pos + Bytes.length chunk)
    chunks;
  (flat, offsets)

let find_entry (prog : Serializer.program) (name : string option) : int =
  match name with
  | None ->
      if Array.length prog.funcs = 0 then load_error "program has no functions";
      0
  | Some n -> (
      match
        Array.find_index (fun (f : Serializer.func) -> f.name = n) prog.funcs
      with
      | Some i -> i
      | None -> load_error "entry function '%s' not found" n)

let func_slice (flat : bytes) (offsets : int array) (i : int) : bytes =
  let n = Array.length offsets in
  let off = offsets.(i) in
  let len =
    if i + 1 < n then offsets.(i + 1) - off else Bytes.length flat - off
  in
  Bytes.sub flat off len

module Make
    (H : Heap.S)
    (VM : Vm.S with type heap_t = H.t and type heap_tracer_ctx = H.tracer_ctx) =
struct
  let build_closures (prog : Serializer.program) (flat : bytes)
      (offsets : int array) (heap : H.t) (vm : VM.t) =
    let n_consts = Array.length prog.constants in
    Array.iteri (fun i v -> VM.set_const vm i v) prog.constants;
    Array.iteri
      (fun i _ ->
        let slice = func_slice flat offsets i in
        let code_native =
          Value.make_native ~tag:Value.bytecode_id ~finalizer:None slice
        in
        let addr = H.alloc heap ~size:1 ~tag:Tag.closure in
        H.write heap addr 0 code_native;
        VM.set_const vm (n_consts + i) (Value.Ptr addr))
      prog.funcs

  let load ?(entry : string option = None) (prog : Serializer.program)
      (cfg : Config.t) (heap_ctx : H.tracer_ctx) (tracer_ctx : VM.tracer_ctx) :
      VM.t =
    let flat, offsets = link prog.funcs in
    let entry_idx = find_entry prog entry in
    let entry_bytes = func_slice flat offsets entry_idx in
    let vm = VM.create cfg entry_bytes heap_ctx tracer_ctx in
    let heap = VM.heap vm in

    build_closures prog flat offsets heap vm;
    vm

  let load_file ?(entry = None) path cfg heap_ctx tracer_ctx =
    let prog = Serializer.deserialize_from_file path in
    load ~entry prog cfg heap_ctx tracer_ctx
end
