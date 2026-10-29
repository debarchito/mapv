open Mapv
open Mapv.Core
open Mapv.Asm
open Mapv.Bytecode

(* Two post-dump workloads.

   [no_gc] is a pure register loop: it never allocates and therefore never
   touches the Mapv young/old heap.  [gc_pressure] allocates three objects per
   inner iteration and writes one field per outer iteration, so it exercises
   allocation, promotion, write barriers, minor GCs and major GCs. *)

type t = {
  name : string;
  program : Instr.t array;
  config : Config.t;
  outer_iters : int;
  inner_iters : int;
  alloc_size : int;
}

let no_gc_config =
  {
    Config.default with
    heap = { chunk_size = 128; young_limit = 256; max_chunks = 128 };
    gc = { major_threshold = 128; major_growth_factor = 1.5 };
  }

let gc_pressure_config =
  {
    Config.default with
    heap = { chunk_size = 1024; young_limit = 512; max_chunks = 2048 };
    gc = { major_threshold = 512; major_growth_factor = 1.5 };
  }

let make_no_gc ~iters =
  let a = Asm.create () in
  let open Instr in
  Asm.label a "start";
  Asm.emit a (Load (4, 0)) |> ignore;
  Asm.emit a (Load (5, iters)) |> ignore;
  Asm.label a "loop";
  Asm.emit a (AddI (4, 4, 2)) |> ignore;
  Asm.emit a (SubI (5, 5, 1)) |> ignore;
  Asm.jnz a 5 "loop";
  Asm.emit a Halt |> ignore;
  let r = Asm.link a in
  {
    name = "no_gc";
    program = r.program;
    config = no_gc_config;
    outer_iters = iters;
    inner_iters = 0;
    alloc_size = 0;
  }

let make_gc_pressure ~outer ~inner ~alloc_size =
  let a = Asm.create () in
  let open Instr in
  Asm.label a "outer";
  Asm.emit a (Load (1, outer)) |> ignore;
  Asm.label a "outer_loop";
  Asm.emit a (Load (0, 0)) |> ignore;
  Asm.emit a (Lte (3, 1, 0)) |> ignore;
  Asm.jnz a 3 "exit";
  Asm.emit a (Load (2, inner)) |> ignore;
  Asm.label a "inner_loop";
  Asm.emit a (Lte (3, 2, 0)) |> ignore;
  Asm.jnz a 3 "inner_done";
  Asm.emit a (Alloc (4, Tag.user, alloc_size)) |> ignore;
  Asm.emit a (Alloc (5, Tag.user, alloc_size)) |> ignore;
  Asm.emit a (Alloc (6, Tag.user, alloc_size)) |> ignore;
  Asm.emit a (SubI (2, 2, 1)) |> ignore;
  Asm.jmp a "inner_loop";
  Asm.label a "inner_done";
  Asm.emit a (Alloc (10, Tag.user, 5)) |> ignore;
  Asm.emit a (SetField (10, 0, 4)) |> ignore;
  Asm.emit a (Mov (4, 10)) |> ignore;
  Asm.emit a (SubI (1, 1, 1)) |> ignore;
  Asm.jmp a "outer_loop";
  Asm.label a "exit";
  Asm.emit a Halt |> ignore;
  let r = Asm.link a in
  {
    name = "gc_pressure";
    program = r.program;
    config = gc_pressure_config;
    outer_iters = outer;
    inner_iters = inner;
    alloc_size = alloc_size;
  }

let entry t =
  let flat, offsets =
    Loader.link
      [| { Serializer.name = t.name; arity = 0; code = t.program } |]
  in
  Loader.func_slice flat offsets 0
