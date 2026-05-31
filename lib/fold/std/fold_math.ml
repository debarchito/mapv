open Mapv.Core
open Mapv.Namespace

module Make (H : Mapv.Heap.S) = struct
  module Fs = Fold_string.Make (H)
  module NS = Mapv.Namespace.Make (H)

  let register heap (reg : Mapv.Symbol.registry) =
    let nif_checked, nif_, type_err = NS.ns_builder () in
    let fof = float_of_int in
    NS.register heap reg
      (ns "std/math"
         [
           nif_checked "abs" 1 (function
             | [| Value.Int n |] -> Value.Int (abs n)
             | [| Value.Float f |] -> Value.Float (abs_float f)
             | _ -> type_err "abs" "Int|Float");
           nif_checked "acos" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.acos f)
             | [| Value.Int n |] -> Value.Float (Stdlib.acos (fof n))
             | _ -> type_err "acos" "Int|Float");
           nif_checked "asin" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.asin f)
             | [| Value.Int n |] -> Value.Float (Stdlib.asin (fof n))
             | _ -> type_err "asin" "Int|Float");
           nif_checked "atan" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.atan f)
             | [| Value.Int n |] -> Value.Float (Stdlib.atan (fof n))
             | _ -> type_err "atan" "Int|Float");
           nif_checked "atan2" 2 (function
             | [| Value.Float y; Value.Float x |] ->
                 Value.Float (Stdlib.atan2 y x)
             | [| Value.Int y; Value.Int x |] ->
                 Value.Float (Stdlib.atan2 (fof y) (fof x))
             | [| Value.Float y; Value.Int x |] ->
                 Value.Float (Stdlib.atan2 y (fof x))
             | [| Value.Int y; Value.Float x |] ->
                 Value.Float (Stdlib.atan2 (fof y) x)
             | _ -> type_err "atan2" "Int|Float, Int|Float");
           nif_checked "ceil" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.ceil f)
             | [| Value.Int n |] -> Value.Int n
             | _ -> type_err "ceil" "Int|Float");
           nif_checked "cos" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.cos f)
             | [| Value.Int n |] -> Value.Float (Stdlib.cos (fof n))
             | _ -> type_err "cos" "Int|Float");
           nif_checked "cosh" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.cosh f)
             | [| Value.Int n |] -> Value.Float (Stdlib.cosh (fof n))
             | _ -> type_err "cosh" "Int|Float");
           nif_checked "exp" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.exp f)
             | [| Value.Int n |] -> Value.Float (Stdlib.exp (fof n))
             | _ -> type_err "exp" "Int|Float");
           nif_checked "expm1" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.expm1 f)
             | [| Value.Int n |] -> Value.Float (Stdlib.expm1 (fof n))
             | _ -> type_err "expm1" "Int|Float");
           nif_checked "floor" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.floor f)
             | [| Value.Int n |] -> Value.Int n
             | _ -> type_err "floor" "Int|Float");
           nif_checked "fmod" 2 (function
             | [| Value.Float x; Value.Float y |] ->
                 Value.Float (Stdlib.mod_float x y)
             | [| Value.Int x; Value.Int y |] ->
                 Value.Float (Stdlib.mod_float (fof x) (fof y))
             | [| Value.Float x; Value.Int y |] ->
                 Value.Float (Stdlib.mod_float x (fof y))
             | [| Value.Int x; Value.Float y |] ->
                 Value.Float (Stdlib.mod_float (fof x) y)
             | _ -> type_err "fmod" "Int|Float, Int|Float");
           nif_checked "hypot" 2 (function
             | [| Value.Float x; Value.Float y |] ->
                 Value.Float (Stdlib.hypot x y)
             | [| Value.Int x; Value.Int y |] ->
                 Value.Float (Stdlib.hypot (fof x) (fof y))
             | [| Value.Float x; Value.Int y |] ->
                 Value.Float (Stdlib.hypot x (fof y))
             | [| Value.Int x; Value.Float y |] ->
                 Value.Float (Stdlib.hypot (fof x) y)
             | _ -> type_err "hypot" "Int|Float, Int|Float");
           nif_checked "log" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.log f)
             | [| Value.Int n |] -> Value.Float (Stdlib.log (fof n))
             | _ -> type_err "log" "Int|Float");
           nif_checked "log10" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.log10 f)
             | [| Value.Int n |] -> Value.Float (Stdlib.log10 (fof n))
             | _ -> type_err "log10" "Int|Float");
           nif_checked "log1p" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.log1p f)
             | [| Value.Int n |] -> Value.Float (Stdlib.log1p (fof n))
             | _ -> type_err "log1p" "Int|Float");
           nif_checked "max" 2 (function
             | [| Value.Int a; Value.Int b |] -> Value.Int (Stdlib.max a b)
             | [| Value.Float a; Value.Float b |] ->
                 Value.Float (Stdlib.max a b)
             | [| Value.Int a; Value.Float b |] ->
                 Value.Float (Stdlib.max (fof a) b)
             | [| Value.Float a; Value.Int b |] ->
                 Value.Float (Stdlib.max a (fof b))
             | _ -> type_err "max" "Int|Float, Int|Float");
           nif_checked "min" 2 (function
             | [| Value.Int a; Value.Int b |] -> Value.Int (Stdlib.min a b)
             | [| Value.Float a; Value.Float b |] ->
                 Value.Float (Stdlib.min a b)
             | [| Value.Int a; Value.Float b |] ->
                 Value.Float (Stdlib.min (fof a) b)
             | [| Value.Float a; Value.Int b |] ->
                 Value.Float (Stdlib.min a (fof b))
             | _ -> type_err "min" "Int|Float, Int|Float");
           nif_checked "pow" 2 (function
             | [| Value.Float x; Value.Float y |] ->
                 Value.Float (Stdlib.( ** ) x y)
             | [| Value.Int x; Value.Int y |] ->
                 Value.Float (Stdlib.( ** ) (fof x) (fof y))
             | [| Value.Float x; Value.Int y |] ->
                 Value.Float (Stdlib.( ** ) x (fof y))
             | [| Value.Int x; Value.Float y |] ->
                 Value.Float (Stdlib.( ** ) (fof x) y)
             | _ -> type_err "pow" "Int|Float, Int|Float");
           nif_checked "sin" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.sin f)
             | [| Value.Int n |] -> Value.Float (Stdlib.sin (fof n))
             | _ -> type_err "sin" "Int|Float");
           nif_checked "sinh" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.sinh f)
             | [| Value.Int n |] -> Value.Float (Stdlib.sinh (fof n))
             | _ -> type_err "sinh" "Int|Float");
           nif_checked "sqrt" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.sqrt f)
             | [| Value.Int n |] -> Value.Float (Stdlib.sqrt (fof n))
             | _ -> type_err "sqrt" "Int|Float");
           nif_checked "tan" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.tan f)
             | [| Value.Int n |] -> Value.Float (Stdlib.tan (fof n))
             | _ -> type_err "tan" "Int|Float");
           nif_checked "tanh" 1 (function
             | [| Value.Float f |] -> Value.Float (Stdlib.tanh f)
             | [| Value.Int n |] -> Value.Float (Stdlib.tanh (fof n))
             | _ -> type_err "tanh" "Int|Float");
           nif_checked "trunc" 1 (function
             | [| Value.Float f |] -> Value.Int (Stdlib.truncate f)
             | [| Value.Int n |] -> Value.Int n
             | _ -> type_err "trunc" "Int|Float");
           nif_checked "neg" 1 (function
             | [| Value.Float f |] -> Value.Float ~-.f
             | [| Value.Int n |] -> Value.Int ~-n
             | _ -> type_err "neg" "Int|Float");
           nif_checked "pi" 0 (function
             | [||] -> Value.Float (4.0 *. Stdlib.atan 1.0)
             | _ -> type_err "pi" "");
           nif_checked "e" 0 (function
             | [||] -> Value.Float (Stdlib.exp 1.0)
             | _ -> type_err "e" "");
           nif_checked "monte_carlo" 1 (function
             | [| Value.Int samples |] ->
                 let samples = max 1 samples in
                 let inside = ref 0 in
                 for _ = 0 to samples - 1 do
                   let x = Random.float 1.0 in
                   let y = Random.float 1.0 in
                   if (x *. x) +. (y *. y) <= 1.0 then incr inside
                 done;
                 let pi_est =
                   4.0 *. float_of_int !inside /. float_of_int samples
                 in
                 Printf.printf "Monte Carlo: %d samples, %d inside\n" samples
                   !inside;
                 Printf.printf "Estimated Pi: %.6f\n" pi_est;
                 Value.Float pi_est
             | _ -> type_err "monte_carlo" "Int");
         ])
end
