open Mapv.Core
open Mapv.Namespace

module Make (H : Mapv.Heap.S) = struct
  let bytes_per_slot = 7
  let slots_for n = (n + bytes_per_slot - 1) / bytes_per_slot

  let alloc heap capacity =
    let slots = slots_for capacity in
    let addr = H.alloc heap ~size:(2 + slots) ~tag:Core.Tag.buffer in
    H.write heap addr 0 (Value.Int capacity);
    H.write heap addr 1 (Value.Int 0);
    for i = 0 to slots - 1 do
      H.write heap addr (2 + i) (Value.Int 0)
    done;
    addr

  let capacity heap addr =
    let v = H.read heap addr 0 in
    match v with
    | Value.Int n -> n
    | _ ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf "std/buffer/capacity: expected Int but got %s"
                   (Value.to_string v))))

  let length heap addr =
    let v = H.read heap addr 1 in
    match v with
    | Value.Int n -> n
    | _ ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf "std/buffer/length: expected Int but got %s"
                   (Value.to_string v))))

  let set_length heap addr n =
    let cap = capacity heap addr in
    if n < 0 || n > cap then
      raise
        (Exception.Signal
           (Exception.Bounds_error
              (Printf.sprintf
                 "std/buffer/set_length: out of bounds for range 0 <= length \
                  (got %d) <= capacity (has %d)"
                 n cap)));
    H.write heap addr 1 (Value.Int n)

  let read_byte heap addr i =
    let cap = capacity heap addr in
    if i < 0 || i >= cap then
      raise
        (Exception.Signal
           (Exception.Bounds_error
              (Printf.sprintf
                 "std/buffer/read_byte: out of bounds for range 0 <= index \
                  (got %d) < capacity (has %d)"
                 i cap)));
    let slot = i / bytes_per_slot in
    let off = i mod bytes_per_slot in
    let v = H.read heap addr (2 + slot) in
    match v with
    | Value.Int v -> (v lsr (off * 8)) land 0xFF
    | _ ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf
                   "std/buffer/read_byte: expected slot to hold Int but got %s)"
                   (Value.to_string v))))

  let write_byte heap addr i byte =
    let cap = capacity heap addr in
    if i < 0 || i >= cap then
      raise
        (Exception.Signal
           (Exception.Bounds_error
              (Printf.sprintf
                 "std/buffer/write_byte: out of bounds for range 0 <= index \
                  (got %d) < capacity (has %d)"
                 i cap)));
    let slot = i / bytes_per_slot in
    let off = i mod bytes_per_slot in
    let v = H.read heap addr (2 + slot) in
    let old =
      match v with
      | Value.Int v -> v
      | _ ->
          raise
            (Exception.Signal
               (Exception.Type_error
                  (Printf.sprintf
                     "std/buffer/read_byte: expected slot to hold Int but got \
                      %s)"
                     (Value.to_string v))))
    in
    let mask = lnot (0xFF lsl (off * 8)) in
    let nv = old land mask lor ((byte land 0xFF) lsl (off * 8)) in
    H.write heap addr (2 + slot) (Value.Int nv)

  let blit_from_bytes heap addr offset (src : bytes) src_off len =
    for i = 0 to len - 1 do
      write_byte heap addr (offset + i)
        (Char.code (Bytes.get src (src_off + i)))
    done

  let blit_to_bytes heap addr offset len =
    let dst = Bytes.make len '\000' in
    for i = 0 to len - 1 do
      Bytes.set dst i (Char.chr (read_byte heap addr (offset + i)))
    done;
    dst

  let copy heap src_addr src_off dst_addr dst_off len =
    for i = 0 to len - 1 do
      write_byte heap dst_addr (dst_off + i)
        (read_byte heap src_addr (src_off + i))
    done

  let fill heap addr offset len byte =
    for i = 0 to len - 1 do
      write_byte heap addr (offset + i) (byte land 0xFF)
    done

  let compare heap a_addr a_off b_addr b_off len =
    let result = ref 0 in
    let i = ref 0 in
    while !result = 0 && !i < len do
      let a = read_byte heap a_addr (a_off + !i) in
      let b = read_byte heap b_addr (b_off + !i) in
      result := Int.compare a b;
      incr i
    done;
    !result

  module NS = Mapv.Namespace.Make (H)

  let register heap (reg : Mapv.Symbol.registry) =
    let nif_checked, _, type_err = NS.ns_builder () in
    NS.register heap reg
      (ns "std/buffer"
         [
           nif_checked "make" 1 (function
             | [| Value.Int cap |] -> Value.Ptr (alloc heap cap)
             | _ -> type_err "make" "Int");
           nif_checked "capacity" 1 (function
             | [| Value.Ptr addr |] -> Value.Int (capacity heap addr)
             | _ -> type_err "capacity" "Ptr");
           nif_checked "length" 1 (function
             | [| Value.Ptr addr |] -> Value.Int (length heap addr)
             | _ -> type_err "length" "Ptr");
           nif_checked "set_length" 2 (function
             | [| Value.Ptr addr; Value.Int n |] ->
                 set_length heap addr n;
                 Value.Nil
             | _ -> type_err "set_length" "Ptr, Int");
           nif_checked "read_byte" 2 (function
             | [| Value.Ptr addr; Value.Int i |] ->
                 Value.Int (read_byte heap addr i)
             | _ -> type_err "read_byte" "Ptr, Int");
           nif_checked "write_byte" 3 (function
             | [| Value.Ptr addr; Value.Int i; Value.Int byte |] ->
                 write_byte heap addr i byte;
                 Value.Nil
             | _ -> type_err "write_byte" "Ptr, Int, Int");
           nif_checked "copy" 5 (function
             | [|
                 Value.Ptr s;
                 Value.Int so;
                 Value.Ptr d;
                 Value.Int doff;
                 Value.Int l;
               |] ->
                 copy heap s so d doff l;
                 Value.Nil
             | _ -> type_err "copy" "Ptr, Int, Ptr, Int, Int");
           nif_checked "fill" 4 (function
             | [|
                 Value.Ptr addr; Value.Int offset; Value.Int len; Value.Int byte;
               |] ->
                 fill heap addr offset len byte;
                 Value.Nil
             | _ -> type_err "fill" "Ptr, Int, Int, Int");
           nif_checked "compare" 5 (function
             | [|
                 Value.Ptr a;
                 Value.Int ao;
                 Value.Ptr b;
                 Value.Int bo;
                 Value.Int l;
               |] ->
                 Value.Int (compare heap a ao b bo l)
             | _ -> type_err "compare" "Ptr, Int, Ptr, Int, Int");
         ])
end
