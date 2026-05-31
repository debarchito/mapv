open Mapv.Core
open Mapv.Namespace

module Make (H : Mapv.Heap.S) = struct
  module Fs = Fold_string.Make (H)
  module NS = Mapv.Namespace.Make (H)

  let register heap (reg : Mapv.Symbol.registry) =
    let nif_checked, nif_, type_err = NS.ns_builder () in
    (* sample from N(0, stddev^2) using Box-Muller transform *)
    let box_muller stddev =
      let two_pi = 2.0 *. (4.0 *. Stdlib.atan 1.0) in
      let u = 1.0 -. Random.float 1.0 in
      let v = Random.float 1.0 in
      Value.Float (stddev *. sqrt (-2.0 *. log u) *. cos (two_pi *. v))
    in
    (* sample from Exp(lambda) via inverse CDF *)
    let inv_cdf_exp lambda =
      let u = 1.0 -. Random.float 1.0 in
      Value.Float (-.log u /. lambda)
    in
    NS.register heap reg
      (ns "std/random"
         [
           (* uniform random int in [0, bound). requires bound > 0 *)
           nif_checked "int" 1 (function
             | [| Value.Int bound |] ->
                 if bound <= 0 then type_err "int" "positive Int"
                 else Value.Int (Random.int bound)
             | _ -> type_err "int" "Int");
           (* uniform random float in [0, scale) *)
           nif_checked "float" 1 (function
             | [| Value.Float scale |] -> Value.Float (Random.float scale)
             | [| Value.Int scale |] ->
                 Value.Float (Random.float (float_of_int scale))
             | _ -> type_err "float" "Int|Float");
           (* uniform random float in [0, 1) *)
           nif_checked "float_unit" 0 (function
             | [||] -> Value.Float (Random.float 1.0)
             | _ -> type_err "float_unit" "");
           (* random boolean as Int 0 or 1 *)
           nif_checked "bool" 0 (function
             | [||] -> Value.Int (if Random.bool () then 1 else 0)
             | _ -> type_err "bool" "");
           (* random 30-bit non-negative int *)
           nif_checked "bits" 0 (function
             | [||] -> Value.Int (Random.bits ())
             | _ -> type_err "bits" "");
           (* seed the RNG. pass an Int for deterministic output,
               or no args for self-init from system entropy *)
           nif_ "init" (function
             | [| Value.Int seed |] ->
                 Random.init seed;
                 Value.Nil
             | [||] ->
                 Random.self_init ();
                 Value.Nil
             | _ -> type_err "init" "Int?");
           (* sample from N(0, stddev^2) *)
           nif_checked "gaussian" 1 (function
             | [| Value.Float stddev |] -> box_muller stddev
             | [| Value.Int stddev |] -> box_muller (float_of_int stddev)
             | _ -> type_err "gaussian" "Int|Float");
           (* sample from Exp(lambda). mean = 1/lambda *)
           nif_checked "exp" 1 (function
             | [| Value.Float lambda |] -> inv_cdf_exp lambda
             | [| Value.Int lambda |] -> inv_cdf_exp (float_of_int lambda)
             | _ -> type_err "exp" "Int|Float");
         ])
end
