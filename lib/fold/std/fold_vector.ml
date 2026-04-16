open Mapv.Core
open Mapv.Namespace

module Make (H : Mapv.Heap.S) = struct
  let alloc heap n init =
    let addr = H.alloc heap ~size:(1 + n) ~tag:Core.Tag.vector in
    H.write heap addr 0 (Value.Int n);
    for i = 0 to n - 1 do
      H.write heap addr (1 + i) init
    done;
    addr

  let length heap addr =
    let v = H.read heap addr 0 in
    match v with
    | Value.Int n -> n
    | _ ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf "std/vector/length: expected Int but got %s"
                   (Value.to_string v))))

  let get heap addr i =
    let n = length heap addr in
    if i < 0 || i >= n then
      raise
        (Exception.Signal
           (Exception.Bounds_error
              (Printf.sprintf "std/vector/get: index %d out of bounds (len %d)"
                 i n)));
    H.read heap addr (1 + i)

  let set heap addr i v =
    let n = length heap addr in
    if i < 0 || i >= n then
      raise
        (Exception.Signal
           (Exception.Bounds_error
              (Printf.sprintf "std/vector/set: index %d out of bounds (len %d)"
                 i n)));
    H.write heap addr (1 + i) v

  let is_vector heap addr = H.get_tag heap addr = Core.Tag.vector

  module NS = Mapv.Namespace.Make (H)

  let register heap (reg : Mapv.Symbol.registry) =
    let nif_checked, _, type_err = NS.ns_builder () in
    NS.register heap reg
      (ns "std/vector"
         [
           nif_checked "make" 1 (function
             | [| Value.Int n |] -> Value.Ptr (alloc heap n Value.Nil)
             | _ -> type_err "make" "Int");
           nif_checked "make" 2 (function
             | [| Value.Int n; init |] -> Value.Ptr (alloc heap n init)
             | _ -> type_err "make" "Int, Value");
           nif_checked "length" 1 (function
             | [| Value.Ptr addr |] -> Value.Int (length heap addr)
             | _ -> type_err "length" "Ptr");
           nif_checked "get" 2 (function
             | [| Value.Ptr addr; Value.Int i |] -> get heap addr i
             | _ -> type_err "get" "Ptr, Int");
           nif_checked "set" 3 (function
             | [| Value.Ptr addr; Value.Int i; v |] ->
                 set heap addr i v;
                 Value.Nil
             | _ -> type_err "set" "Ptr, Int, Value");
           nif_checked "is_vector" 1 (function
             | [| Value.Ptr addr |] ->
                 Value.Int (if is_vector heap addr then 1 else 0)
             | [| _ |] -> Value.Int 0
             | _ -> type_err "is_vector" "Ptr");
         ])
end
