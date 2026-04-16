open Mapv.Core
open Mapv.Namespace

module Make (H : Mapv.Heap.S) = struct
  module Fs = Fold_string.Make (H)
  module NS = Mapv.Namespace.Make (H)

  let register heap (reg : Mapv.Symbol.registry) =
    let nif_checked, nif_, type_err = NS.ns_builder () in
    let fof = float_of_int in
    NS.register heap reg
      (ns "std/random"
         [
           nif_checked "int" 1 (function
             | [| Value.Int bound |] ->
                 if bound <= 0 then type_err "int" "positive Int"
                 else Value.Int (Random.int bound)
             | _ -> type_err "int" "Int");
           nif_checked "float" 1 (function
             | [| Value.Float scale |] -> Value.Float (Random.float scale)
             | [| Value.Int scale |] -> Value.Float (Random.float (fof scale))
             | _ -> type_err "float" "Int|Float");
           nif_checked "float_unit" 0 (function
             | [||] -> Value.Float (Random.float 1.0)
             | _ -> type_err "float_unit" "");
           nif_checked "bool" 0 (function
             | [||] -> Value.Int (if Random.bool () then 1 else 0)
             | _ -> type_err "bool" "");
           nif_checked "bits" 0 (function
             | [||] -> Value.Int (Random.bits ())
             | _ -> type_err "bits" "");
           nif_ "init" (function args ->
               (match args with
               | [| Value.Int seed |] ->
                   Random.init seed;
                   Value.Nil
               | [||] ->
                   Random.self_init ();
                   Value.Nil
               | _ -> type_err "init" "Int?"));
           nif_checked "gaussian" 1 (function
             | [| Value.Float stddev |] ->
                 let u = Random.float 1.0 in
                 let v = Random.float 1.0 in
                 let two_pi = 2.0 *. (4.0 *. Stdlib.atan 1.0) in
                 let box_muller =
                   stddev *. sqrt (-2.0 *. log u) *. cos (two_pi *. v)
                 in
                 Value.Float box_muller
             | [| Value.Int stddev |] ->
                 let stddev = fof stddev in
                 let u = Random.float 1.0 in
                 let v = Random.float 1.0 in
                 let two_pi = 2.0 *. (4.0 *. Stdlib.atan 1.0) in
                 let box_muller =
                   stddev *. sqrt (-2.0 *. log u) *. cos (two_pi *. v)
                 in
                 Value.Float box_muller
             | _ -> type_err "gaussian" "Int|Float");
           nif_checked "exp" 1 (function
             | [| Value.Float lambda |] ->
                 let u = Random.float 1.0 in
                 Value.Float (-.log (1.0 -. u) /. lambda)
             | [| Value.Int lambda |] ->
                 let u = Random.float 1.0 in
                 Value.Float (-.log (1.0 -. u) /. fof lambda)
             | _ -> type_err "exp" "Int|Float");
         ])
end
