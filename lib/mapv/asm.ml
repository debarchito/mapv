open Core

type label = string

type patch_kind =
  | Jmp_target
  | Jz_target
  | Jnz_target
  | Call_target
  | TailCall_target
  | Try_target

type patch = {
  kind : patch_kind;
  pc : int;
  name : string;
  scope_stack : string list;
}

type t = {
  mutable instrs : Instr.t array;
  mutable size : int;
  mutable scope : string list;
  mutable scope_id : int;
  labels : (string, int) Hashtbl.t;
  patches : patch list ref;
  const_tbl : (Value.t, int) Hashtbl.t;
  mutable const_vec : Value.t list;
  mutable const_len : int;
}

type result = { program : Instr.t array; constants : Value.t array }

let create () =
  {
    instrs = Array.make 64 Instr.Nop;
    size = 0;
    scope = [ "0" ];
    scope_id = 0;
    labels = Hashtbl.create 16;
    patches = ref [];
    const_tbl = Hashtbl.create 16;
    const_vec = [];
    const_len = 0;
  }

let grow t =
  let n = Array.length t.instrs * 2 in
  let a = Array.make n Instr.Nop in
  Array.blit t.instrs 0 a 0 t.size;
  t.instrs <- a

let pc t = t.size

let emit t instr =
  if t.size >= Array.length t.instrs then grow t;
  t.instrs.(t.size) <- instr;
  let p = t.size in
  t.size <- t.size + 1;
  p

let push_scope t =
  t.scope_id <- t.scope_id + 1;
  t.scope <- string_of_int t.scope_id :: t.scope;
  t.scope

let pop_scope t =
  match t.scope with
  | [] -> failwith "Asm.pop_scope: scope stack underflow"
  | _ :: rest -> t.scope <- rest

let label t name =
  let key = String.concat "." (List.rev t.scope) ^ "." ^ name in
  if Hashtbl.mem t.labels key then
    failwith (Printf.sprintf "Asm.label: duplicate label '%s'" key);
  Hashtbl.add t.labels key (pc t)

let add_patch t name kind =
  t.patches :=
    { kind; pc = t.size; name; scope_stack = t.scope } :: !(t.patches)

let resolve_label t name scope_stack =
  let rec walk = function
    | [] -> failwith (Printf.sprintf "Asm.link: unresolved label '%s'" name)
    | scope -> (
        let key = String.concat "." (List.rev scope) ^ "." ^ name in
        match Hashtbl.find_opt t.labels key with
        | Some target -> target
        | None -> walk (List.tl scope))
  in
  walk scope_stack

let const t v =
  match Hashtbl.find_opt t.const_tbl v with
  | Some idx -> idx
  | None ->
      let idx = t.const_len in
      Hashtbl.add t.const_tbl v idx;
      t.const_vec <- v :: t.const_vec;
      t.const_len <- t.const_len + 1;
      idx

let const_int t n = const t (Value.Int n)
let const_float t f = const t (Value.Float f)
let const_value t v = const t v

let jmp t name =
  add_patch t name Jmp_target;
  emit t (Instr.Jmp 0) |> ignore

let jz t reg name =
  add_patch t name Jz_target;
  emit t (Instr.Jz (reg, 0)) |> ignore

let jnz t reg name =
  add_patch t name Jnz_target;
  emit t (Instr.Jnz (reg, 0)) |> ignore

let call t name arg_start arg_end ret_dst =
  add_patch t name Call_target;
  emit t (Instr.Call (0, arg_start, arg_end, ret_dst)) |> ignore

let tcall t name arg_start arg_end =
  add_patch t name TailCall_target;
  emit t (Instr.TailCall (0, arg_start, arg_end)) |> ignore

let try_ t name catch_reg =
  add_patch t name Try_target;
  emit t (Instr.Try (0, catch_reg)) |> ignore

let patch_instr instr kind target =
  match (kind, instr) with
  | Jmp_target, Instr.Jmp _ -> Instr.Jmp target
  | Jz_target, Instr.Jz (r, _) -> Instr.Jz (r, target)
  | Jnz_target, Instr.Jnz (r, _) -> Instr.Jnz (r, target)
  | Call_target, Instr.Call (_, s, e, r) -> Instr.Call (target, s, e, r)
  | TailCall_target, Instr.TailCall (_, s, e) -> Instr.TailCall (target, s, e)
  | Try_target, Instr.Try (_, r) -> Instr.Try (target, r)
  | k, _ ->
      let ks =
        match k with
        | Jmp_target -> "Jmp"
        | Jz_target -> "Jz"
        | Jnz_target -> "Jnz"
        | Call_target -> "Call"
        | TailCall_target -> "TailCall"
        | Try_target -> "Try"
      in
      failwith
        (Printf.sprintf "Asm.link: patch kind %s mismatches instruction at pc"
           ks)

let link t =
  List.iter
    (fun { kind; pc; name; scope_stack } ->
      let target = resolve_label t name scope_stack in
      t.instrs.(pc) <- patch_instr t.instrs.(pc) kind target)
    !(t.patches);
  {
    program = Array.sub t.instrs 0 t.size;
    constants = Array.of_list (List.rev t.const_vec);
  }
