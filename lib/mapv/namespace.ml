open Core

type fn = Native of Value.t | Closure of bytes * Value.t array
type t = Leaf of string * fn | Node of string * t list

let name_of = function Leaf (n, _) -> n | Node (n, _) -> n
let ns name children = Node (name, children)
let native name v = Leaf (name, Native v)
let nif name f = Leaf (name, Native (Value.NativeFun f))
let closure name code consts = Leaf (name, Closure (code, consts))

let register_pure (reg : Symbol.registry) root =
  let rec walk path = function
    | Leaf (name, Native v) ->
        let full = if path = "" then name else path ^ "/" ^ name in
        ignore (Symbol.register reg ~name:full v)
    | Leaf (name, Closure _) ->
        let full = if path = "" then name else path ^ "/" ^ name in
        failwith
          (Printf.sprintf
             "Namespace.register_pure: '%s' is a closure, use Make(H).register"
             full)
    | Node (name, children) ->
        let full = if path = "" then name else path ^ "/" ^ name in
        List.iter (walk full) children
  in
  walk "" root

module Make (H : Heap.S) = struct
  let load_value heap = function
    | Native v -> v
    | Closure (code, _) ->
        let code_ptr =
          Value.make_native ~tag:Value.bytecode_id ~finalizer:None code
        in
        let addr = H.alloc heap ~size:2 ~tag:Tag.closure in
        H.write heap addr 0 code_ptr;
        H.write heap addr 1 Value.Nil;
        Value.Ptr addr

  let nif_checked name arity body =
    nif name (fun args ->
        let actual = Array.length args in
        if actual <> arity then
          raise
            (Exception.Signal
               (Exception.Arity_error
                  (Printf.sprintf "%s: expected %d arguments but got %d" name
                     arity actual)));
        body args)

  let nif_ name body = nif name body

  let ns_builder () =
    let def name arity body = nif_checked name arity body in
    let def_ name body = nif_ name body in
    let type_err name expected =
      raise
        (Exception.Signal
           (Exception.Type_error
              (Printf.sprintf "%s: expected %s" name expected)))
    in
    (def, def_, type_err)

  let register heap (reg : Symbol.registry) root =
    let rec walk path = function
      | Leaf (name, fn) ->
          let full = if path = "" then name else path ^ "/" ^ name in
          let v = load_value heap fn in
          ignore (Symbol.register reg ~name:full v)
      | Node (name, children) ->
          let full = if path = "" then name else path ^ "/" ^ name in
          List.iter (walk full) children
    in
    walk "" root

  let pp root =
    let rec walk indent = function
      | Leaf (name, Native (Value.NativeFun _)) ->
          Printf.printf "%s%s  [nif]\n" indent name
      | Leaf (name, Native v) ->
          Printf.printf "%s%s  [%s]\n" indent name (Value.to_string v)
      | Leaf (name, Closure _) -> Printf.printf "%s%s  [closure]\n" indent name
      | Node (name, children) ->
          Printf.printf "%s%s/\n" indent name;
          List.iter (walk (indent ^ "  ")) children
    in
    walk "" root
end
