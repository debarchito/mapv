open Mapv.Core
open Mapv.Namespace

module Make (H : Mapv.Heap.S) = struct
  let alloc heap schema_id nfields =
    let addr = H.alloc heap ~size:(2 + nfields) ~tag:Core.Tag.record in
    H.write heap addr 0 (Value.Int schema_id);
    H.write heap addr 1 (Value.Int nfields);
    for i = 0 to nfields - 1 do
      H.write heap addr (2 + i) Value.Nil
    done;
    addr

  let schema heap addr =
    let v = H.read heap addr 0 in
    match v with
    | Value.Int s -> s
    | _ ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf "std/record/schema: expected Int but got %s"
                   (Value.to_string v))))

  let nfields heap addr =
    let v = H.read heap addr 1 in
    match v with
    | Value.Int n -> n
    | _ ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf "std/record/nfields: expected Int but got %s"
                   (Value.to_string v))))

  let get heap addr i =
    let n = nfields heap addr in
    if i < 0 || i >= n then
      raise
        (Exception.Signal
           (Exception.Bounds_error
              (Printf.sprintf "std/record/get: field %d out of bounds (n %d)" i
                 n)));
    H.read heap addr (2 + i)

  let set heap addr i v =
    let n = nfields heap addr in
    if i < 0 || i >= n then
      raise
        (Exception.Signal
           (Exception.Bounds_error
              (Printf.sprintf "std/record/set: field %d out of bounds (n %d)" i
                 n)));
    H.write heap addr (2 + i) v

  let is_record heap addr = H.get_tag heap addr = Core.Tag.record
  let is_schema heap addr s = schema heap addr = s

  module NS = Mapv.Namespace.Make (H)

  let register heap (reg : Mapv.Symbol.registry) =
    let nif_checked, _, type_err = NS.ns_builder () in
    NS.register heap reg
      (ns "std/record"
         [
           nif_checked "make" 2 (function
             | [| Value.Int schema_id; Value.Int nfields |] ->
                 Value.Ptr (alloc heap schema_id nfields)
             | _ -> type_err "make" "Int, Int");
           nif_checked "schema" 1 (function
             | [| Value.Ptr addr |] -> Value.Int (schema heap addr)
             | _ -> type_err "schema" "Ptr");
           nif_checked "nfields" 1 (function
             | [| Value.Ptr addr |] -> Value.Int (nfields heap addr)
             | _ -> type_err "nfields" "Ptr");
           nif_checked "get" 2 (function
             | [| Value.Ptr addr; Value.Int i |] -> get heap addr i
             | _ -> type_err "get" "Ptr, Int");
           nif_checked "set" 3 (function
             | [| Value.Ptr addr; Value.Int i; v |] ->
                 set heap addr i v;
                 Value.Nil
             | _ -> type_err "set" "Ptr, Int, Value");
           nif_checked "is_record" 1 (function
             | [| Value.Ptr addr |] ->
                 Value.Int (if is_record heap addr then 1 else 0)
             | [| _ |] -> Value.Int 0
             | _ -> type_err "is_record" "Ptr");
           nif_checked "is_schema" 2 (function
             | [| Value.Ptr addr; Value.Int s |] ->
                 Value.Int (if is_schema heap addr s then 1 else 0)
             | _ -> type_err "is_schema" "Ptr, Int");
         ])
end
