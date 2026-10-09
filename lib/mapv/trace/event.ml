open Core

type gc =
  | Minor_start
  | Minor_end of { promoted : int }
  | Major_mark of { steps : int }
  | Major_sweep of { steps : int; freed : int }
  | Major_end

type kind =
  | Instr of { pc : int; op : int }
  | Call of { pc : int; target : int }
  | Ret of { pc : int }
  | Throw of { pc : int }
  | Con_new of { pc : int; con_id : int }
  | Con_yield of { con_id : int; pc : int }
  | Con_resume of { con_id : int; pc : int }
  | Reg_write of { reg : int; value : Value.t }
  | Alloc of { addr : int; size : int; tag : int }
  | Free of { addr : int }
  | Promote of { addr : int }
  | Read of { addr : int; field : int }
  | Write of { addr : int; field : int; value : Value.t }
  | Gc of { event : gc }

type t = { seq : int; kind : kind }
