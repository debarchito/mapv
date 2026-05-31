open Core

type symbol_id = int64

let hash_basis = 0xcbf29ce484222325L
let hash_prime = 0x100000001b3L

let hash s =
  let h = ref hash_basis in
  String.iter
    (fun c ->
      h := Int64.logxor !h (Int64.of_int (Char.code c));
      h := Int64.mul !h hash_prime)
    s;
  !h

type entry = Resolved of Value.t | Unresolved of string

type registry = {
  by_id : (symbol_id, entry) Hashtbl.t;
  by_name : (string, symbol_id) Hashtbl.t;
}

let create () = { by_id = Hashtbl.create 256; by_name = Hashtbl.create 256 }

let register reg ~name v =
  let id = hash name in
  Hashtbl.replace reg.by_id id (Resolved v);
  Hashtbl.replace reg.by_name name id;
  id

let resolve reg id =
  match Hashtbl.find_opt reg.by_id id with
  | Some (Resolved v) -> Some v
  | Some (Unresolved n) ->
      failwith (Printf.sprintf "Symbol.resolve: unresolved '%s'" n)
  | None -> None

let sym reg name =
  match Hashtbl.find_opt reg.by_name name with
  | Some id -> id
  | None -> failwith (Printf.sprintf "Symbol.sym: '%s' not found" name)

let pp reg =
  Hashtbl.iter
    (fun id entry ->
      match entry with
      | Resolved v -> Printf.printf "  0x%Lx  %s\n" id (Value.to_string v)
      | Unresolved name -> Printf.printf "  0x%Lx  [unresolved: %s]\n" id name)
    reg.by_id

let sym reg name =
  match resolve reg (hash name) with
  | Some v -> v
  | None -> failwith ("Could not resolve symbol: " ^ name)
