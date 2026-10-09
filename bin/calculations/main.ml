open Mapv
open Mapv.Core
open Mapv.Bytecode
open Mapv.Asm
module H = Heap.Make (Heap.Tracing)
module VM = Vm.Make (H) (Vm.Tracing)

let run prog n config =
  let heap_ctx =
    Mapv.Heap.Tracing.make ~max_chunks:config.Config.heap.max_chunks
      ~chunk_size:config.Config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Mapv.Vm.Tracing.make () in
  let flat, offsets =
    Mapv.Bytecode.Loader.link
      [| { Serializer.name = "main"; arity = 0; code = prog } |]
  in
  let entry = Mapv.Bytecode.Loader.func_slice flat offsets 0 in
  let vm = VM.create config entry heap_ctx vm_ctx in
  VM.set_reg vm 0 (Value.Int n);
  (try VM.run vm with
  | Exception.Panic p ->
      Printf.eprintf "PANIC: %s\n" (Exception.panic_to_string p);
      exit 1
  | Exception.Signal s ->
      let code, msg = Exception.signal_to_pair s in
      Printf.eprintf "SIGNAL [%d]: %s\n" code msg;
      exit 1);
  VM.get_reg vm 1

(* factorial: r0=n -> r1=n! *)
let make_factorial () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (Load (1, 1)) |> ignore;
  label a "loop";
  jz a 0 "done";
  emit a (Mul (1, 1, 0)) |> ignore;
  emit a (SubI (0, 0, 1)) |> ignore;
  jmp a "loop";
  label a "done";
  emit a Halt |> ignore;
  (link a).program

(* fibonacci: r0=n -> r1=fib(n) *)
let make_fibonacci () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (Load (1, 0)) |> ignore;
  emit a (Load (2, 1)) |> ignore;
  label a "loop";
  jz a 0 "done";
  emit a (Add (3, 1, 2)) |> ignore;
  emit a (Mov (1, 2)) |> ignore;
  emit a (Mov (2, 3)) |> ignore;
  emit a (SubI (0, 0, 1)) |> ignore;
  jmp a "loop";
  label a "done";
  emit a Halt |> ignore;
  (link a).program

(* triangular: r0=n -> r1=sum 1..n *)
let make_triangular () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (Load (1, 0)) |> ignore;
  label a "loop";
  jz a 0 "done";
  emit a (Add (1, 1, 0)) |> ignore;
  emit a (SubI (0, 0, 1)) |> ignore;
  jmp a "loop";
  label a "done";
  emit a Halt |> ignore;
  (link a).program

(* pow2: r0=n -> r1=2^n *)
let make_pow2 () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (Load (1, 1)) |> ignore;
  label a "loop";
  jz a 0 "done";
  emit a (ShlI (1, 1, 1)) |> ignore;
  emit a (SubI (0, 0, 1)) |> ignore;
  jmp a "loop";
  label a "done";
  emit a Halt |> ignore;
  (link a).program

(* sum of squares: r0=n -> r1=1^2+..+n^2, r2=counter *)
let make_sum_of_squares () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (Load (1, 0)) |> ignore;
  emit a (Load (2, 1)) |> ignore;
  (* r0=n, r1=acc, r2=i from 1..n *)
  label a "loop";
  (* if r2 > n: done. use r0 as countdown instead *)
  jz a 0 "done";
  emit a (Mul (3, 2, 2)) |> ignore;
  emit a (Add (1, 1, 3)) |> ignore;
  emit a (AddI (2, 2, 1)) |> ignore;
  emit a (SubI (0, 0, 1)) |> ignore;
  jmp a "loop";
  label a "done";
  emit a Halt |> ignore;
  (link a).program

(* collatz: r0=n -> r1=steps *)
let make_collatz () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (Load (1, 0)) |> ignore;
  label a "loop";
  (* done if n == 1, check by subtracting 1 and testing zero *)
  emit a (SubI (29, 0, 1)) |> ignore;
  jz a 29 "done";
  (* even check: r29 = n & ~1, if r29 == n then even *)
  emit a (ShrI (29, 0, 1)) |> ignore;
  emit a (ShlI (29, 29, 1)) |> ignore;
  emit a (Sub (28, 0, 29)) |> ignore;
  (* r28 = 0 if even, 1 if odd *)
  jnz a 28 "odd";
  emit a (ShrI (0, 0, 1)) |> ignore;
  jmp a "next";
  label a "odd";
  emit a (MulI (0, 0, 3)) |> ignore;
  emit a (AddI (0, 0, 1)) |> ignore;
  label a "next";
  emit a (AddI (1, 1, 1)) |> ignore;
  jmp a "loop";
  label a "done";
  emit a Halt |> ignore;
  (link a).program

(* isqrt: r0=n -> r1=floor(sqrt(n)) *)
let make_isqrt () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  emit a (Load (1, 0)) |> ignore;
  label a "loop";
  emit a (AddI (29, 1, 1)) |> ignore;
  emit a (Mul (28, 29, 29)) |> ignore;
  (* r28 = (r1+1)^2, need r28 > r0 to stop *)
  (* r28 - r0, if <= 0 continue *)
  emit a (Sub (27, 28, 0)) |> ignore;
  (* if r27 <= 0 keep going: jz handles ==0, need to also catch negative *)
  (* use: if r28 - r0 - 1 < 0 i.e. r28 <= r0, continue *)
  emit a (SubI (27, 27, 1)) |> ignore;
  (* r27 < 0 means (r1+1)^2 <= n, so increment *)
  (* check sign bit via ShrI by 62 to get sign *)
  emit a (ShrI (27, 27, 30)) |> ignore;
  jnz a 27 "inc";
  (* also check zero: (r1+1)^2 == n means r1+1 is exact sqrt, still increment *)
  emit a (Sub (27, 28, 0)) |> ignore;
  jz a 27 "inc";
  jmp a "done";
  label a "inc";
  emit a (AddI (1, 1, 1)) |> ignore;
  jmp a "loop";
  label a "done";
  emit a Halt |> ignore;
  (link a).program

(* gcd: r0=a, r1=b -> r1=gcd(a,b) *)
let make_gcd () =
  let a = Asm.create () in
  let open Asm in
  let open Instr in
  label a "main";
  label a "loop";
  jz a 1 "done";
  emit a (Mod (2, 0, 1)) |> ignore;
  emit a (Mov (0, 1)) |> ignore;
  emit a (Mov (1, 2)) |> ignore;
  jmp a "loop";
  label a "done";
  emit a Halt |> ignore;
  (link a).program

let run prog n config =
  let heap_ctx =
    Mapv.Heap.Tracing.make ~max_chunks:config.Config.heap.max_chunks
      ~chunk_size:config.Config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Mapv.Vm.Tracing.make () in
  let flat, offsets =
    Mapv.Bytecode.Loader.link
      [| { Serializer.name = "main"; arity = 0; code = prog } |]
  in
  let entry = Mapv.Bytecode.Loader.func_slice flat offsets 0 in
  let vm = VM.create config entry heap_ctx vm_ctx in
  VM.set_reg vm 0 (Value.Int n);
  (try VM.run vm with
  | Exception.Panic p ->
      Printf.eprintf "PANIC: %s\n" (Exception.panic_to_string p);
      exit 1
  | Exception.Signal s ->
      let code, msg = Exception.signal_to_pair s in
      Printf.eprintf "SIGNAL [%d]: %s\n" code msg;
      exit 1);
  VM.get_reg vm 1

let run2 prog a b config =
  let heap_ctx =
    Mapv.Heap.Tracing.make ~max_chunks:config.Config.heap.max_chunks
      ~chunk_size:config.Config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Mapv.Vm.Tracing.make () in
  let flat, offsets =
    Mapv.Bytecode.Loader.link
      [| { Serializer.name = "main"; arity = 0; code = prog } |]
  in
  let entry = Mapv.Bytecode.Loader.func_slice flat offsets 0 in
  let vm = VM.create config entry heap_ctx vm_ctx in
  VM.set_reg vm 0 (Value.Int a);
  VM.set_reg vm 1 (Value.Int b);
  (try VM.run vm with
  | Exception.Panic p ->
      Printf.eprintf "PANIC: %s\n" (Exception.panic_to_string p);
      exit 1
  | Exception.Signal s ->
      let code, msg = Exception.signal_to_pair s in
      Printf.eprintf "SIGNAL [%d]: %s\n" code msg;
      exit 1);
  VM.get_reg vm 1

let () =
  let config = Config.default in
  let fact = make_factorial () in
  let fib = make_fibonacci () in
  let tri = make_triangular () in
  let pow2 = make_pow2 () in
  let sos = make_sum_of_squares () in
  let col = make_collatz () in
  let isqrt = make_isqrt () in
  let gcd = make_gcd () in

  Printf.printf "Factorial\n";
  for i = 0 to 12 do
    Printf.printf "  %2d! = %s\n" i (Value.to_string (run fact i config))
  done;

  Printf.printf "\nFibonacci\n";
  for i = 0 to 20 do
    Printf.printf "  fib(%2d) = %s\n" i (Value.to_string (run fib i config))
  done;

  Printf.printf "\nTriangular Numbers\n";
  for i = 1 to 15 do
    Printf.printf "  T(%2d) = %s\n" i (Value.to_string (run tri i config))
  done;

  Printf.printf "\nPowers of 2\n";
  for i = 0 to 20 do
    Printf.printf "  2^%2d = %s\n" i (Value.to_string (run pow2 i config))
  done;

  Printf.printf "\nSum of Squares\n";
  for i = 1 to 15 do
    Printf.printf "  sos(%2d) = %s\n" i (Value.to_string (run sos i config))
  done;

  Printf.printf "\nCollatz Steps\n";
  List.iter
    (fun n ->
      Printf.printf "  collatz(%4d) = %s steps\n" n
        (Value.to_string (run col n config)))
    [ 1; 2; 3; 6; 7; 27; 97; 871; 6171 ];

  Printf.printf "\nInteger Square Root\n";
  List.iter
    (fun n ->
      Printf.printf "  isqrt(%4d) = %s\n" n
        (Value.to_string (run isqrt n config)))
    [ 0; 1; 4; 9; 15; 16; 17; 100; 144; 1000 ];

  Printf.printf "\nGCD\n";
  List.iter
    (fun (a, b) ->
      Printf.printf "  gcd(%d, %d) = %s\n" a b
        (Value.to_string (run2 gcd a b config)))
    [ (12, 8); (100, 75); (17, 13); (270, 192); (1071, 462); (48, 18) ]
