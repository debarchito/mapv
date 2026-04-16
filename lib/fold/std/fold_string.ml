open Mapv.Core
open Mapv.Namespace

module Make (H : Mapv.Heap.S) = struct
  module Buf = Fold_buffer.Make (H)
  module NS = Mapv.Namespace.Make (H)

  let char_length_utf8 (b : bytes) off len =
    let count = ref 0 in
    let i = ref off in
    while !i < off + len do
      let c = Char.code (Bytes.get b !i) in
      let skip =
        if c land 0x80 = 0 then 1
        else if c land 0xE0 = 0xC0 then 2
        else if c land 0xF0 = 0xE0 then 3
        else 4
      in
      incr count;
      i := !i + skip
    done;
    !count

  let alloc_string heap ~offset ~byte_length ~char_length ~buf_addr =
    let addr = H.alloc heap ~size:4 ~tag:Core.Tag.string in
    H.write heap addr 0 (Value.Int offset);
    H.write heap addr 1 (Value.Int byte_length);
    H.write heap addr 2 (Value.Int char_length);
    H.write heap addr 3 (Value.Ptr buf_addr);
    Value.Ptr addr

  let of_ocaml heap s =
    let src = Bytes.of_string s in
    let byte_len = Bytes.length src in
    let char_len = char_length_utf8 src 0 byte_len in
    let buf_addr = Buf.alloc heap byte_len in
    Buf.blit_from_bytes heap buf_addr 0 src 0 byte_len;
    Buf.set_length heap buf_addr byte_len;
    alloc_string heap ~offset:0 ~byte_length:byte_len ~char_length:char_len
      ~buf_addr

  let to_ocaml heap addr =
    match (H.read heap addr 0, H.read heap addr 1, H.read heap addr 3) with
    | Value.Int offset, Value.Int byte_len, Value.Ptr buf_addr ->
        Bytes.to_string (Buf.blit_to_bytes heap buf_addr offset byte_len)
    | _ ->
        raise
          (Exception.Signal
             (Exception.Type_error "std/string/to_ocaml: bad string object"))

  let expect_str heap v =
    match v with
    | Value.Ptr addr when H.get_tag heap addr = Core.Tag.string ->
        to_ocaml heap addr
    | _ ->
        raise
          (Exception.Signal
             (Exception.Type_error "std/string: expected string Ptr"))

  let register heap (reg : Mapv.Symbol.registry) =
    let nif, _, type_err = NS.ns_builder () in
    let str = of_ocaml heap in
    let estr = expect_str heap in
    NS.register heap reg
      (ns "std/string"
         [
           nif "len" 1 (function
             | [| Value.Ptr addr |] -> (
                 match H.read heap addr 2 with
                 | Value.Int n -> Value.Int n
                 | _ -> type_err "len" "string Ptr")
             | _ -> type_err "len" "Ptr");
           nif "byte_len" 1 (function
             | [| Value.Ptr addr |] -> (
                 match H.read heap addr 1 with
                 | Value.Int n -> Value.Int n
                 | _ -> type_err "byte_len" "string Ptr")
             | _ -> type_err "byte_len" "Ptr");
           nif "concat" 2 (function
             | [| a; b |] ->
                 let sa = Bytes.of_string (estr a) in
                 let sb = Bytes.of_string (estr b) in
                 let combined = Bytes.cat sa sb in
                 let total = Bytes.length combined in
                 let buf_addr = Buf.alloc heap total in
                 Buf.blit_from_bytes heap buf_addr 0 combined 0 total;
                 Buf.set_length heap buf_addr total;
                 let char_len = char_length_utf8 combined 0 total in
                 alloc_string heap ~offset:0 ~byte_length:total
                   ~char_length:char_len ~buf_addr
             | _ -> type_err "concat" "Ptr, Ptr");
           nif "slice" 3 (function
             | [| Value.Ptr addr; Value.Int lo; Value.Int hi |] -> (
                 match
                   (H.read heap addr 0, H.read heap addr 1, H.read heap addr 3)
                 with
                 | Value.Int offset, Value.Int byte_len, Value.Ptr buf_addr ->
                     let lo = max 0 lo and hi = min byte_len hi in
                     let len = hi - lo in
                     if len < 0 then
                       raise
                         (Exception.Signal
                            (Exception.Bounds_error
                               "std/string/slice: bad range"));
                     let raw =
                       Buf.blit_to_bytes heap buf_addr (offset + lo) len
                     in
                     let char_len = char_length_utf8 raw 0 len in
                     alloc_string heap ~offset:(offset + lo) ~byte_length:len
                       ~char_length:char_len ~buf_addr
                 | _ -> type_err "slice" "string Ptr")
             | _ -> type_err "slice" "Ptr, Int, Int");
           nif "get" 2 (function
             | [| s; Value.Int i |] ->
                 let buf = estr s in
                 if i < 0 || i >= String.length buf then
                   raise
                     (Exception.Signal
                        (Exception.Bounds_error "std/string/get: out of bounds"));
                 let b0 = Char.code buf.[i] in
                 let cp =
                   if b0 land 0x80 = 0 then b0
                   else if b0 land 0xE0 = 0xC0 then
                     ((b0 land 0x1F) lsl 6) lor (Char.code buf.[i + 1] land 0x3F)
                   else if b0 land 0xF0 = 0xE0 then
                     ((b0 land 0x0F) lsl 12)
                     lor ((Char.code buf.[i + 1] land 0x3F) lsl 6)
                     lor (Char.code buf.[i + 2] land 0x3F)
                   else
                     ((b0 land 0x07) lsl 18)
                     lor ((Char.code buf.[i + 1] land 0x3F) lsl 12)
                     lor ((Char.code buf.[i + 2] land 0x3F) lsl 6)
                     lor (Char.code buf.[i + 3] land 0x3F)
                 in
                 Value.Int cp
             | _ -> type_err "get" "Ptr, Int");
           nif "byte_set" 3 (function
             | [| Value.Ptr addr; Value.Int i; Value.Int byte |] -> (
                 match
                   (H.read heap addr 0, H.read heap addr 1, H.read heap addr 3)
                 with
                 | Value.Int offset, Value.Int byte_len, Value.Ptr buf_addr ->
                     if i < 0 || i >= byte_len then
                       raise
                         (Exception.Signal
                            (Exception.Bounds_error
                               "std/string/byte_set: out of bounds"));
                     Buf.write_byte heap buf_addr (offset + i) (byte land 0xFF);
                     Value.Nil
                 | _ -> type_err "byte_set" "string Ptr")
             | _ -> type_err "byte_set" "Ptr, Int, Int");
           nif "eq" 2 (function
             | [| a; b |] -> Value.Int (if estr a = estr b then 1 else 0)
             | _ -> type_err "eq" "Ptr, Ptr");
           nif "compare" 2 (function
             | [| a; b |] -> Value.Int (String.compare (estr a) (estr b))
             | _ -> type_err "compare" "Ptr, Ptr");
           nif "from_int" 1 (function
             | [| Value.Int n |] -> str (string_of_int n)
             | _ -> type_err "from_int" "Int");
           nif "from_float" 1 (function
             | [| Value.Float f |] -> str (string_of_float f)
             | _ -> type_err "from_float" "Float");
           nif "from_float_fmt" 1 (function
             | [| Value.Float f |] -> str (Printf.sprintf "%g" f)
             | _ -> type_err "from_float_fmt" "Float");
           nif "to_int" 1 (function
             | [| s |] -> (
                 match int_of_string_opt (estr s) with
                 | Some n -> Value.Int n
                 | None ->
                     raise
                       (Exception.Signal
                          (Exception.Type_error "std/string/to_int: not an int"))
                 )
             | _ -> type_err "to_int" "Ptr");
           nif "to_float" 1 (function
             | [| s |] -> (
                 match float_of_string_opt (estr s) with
                 | Some f -> Value.Float f
                 | None ->
                     raise
                       (Exception.Signal
                          (Exception.Type_error
                             "std/string/to_float: not a float")))
             | _ -> type_err "to_float" "Ptr");
           nif "contains" 2 (function
             | [| s; sub |] ->
                 let s = estr s and sub = estr sub in
                 let ls = String.length s and lsub = String.length sub in
                 let rec check i =
                   if i + lsub > ls then 0
                   else if String.sub s i lsub = sub then 1
                   else check (i + 1)
                 in
                 Value.Int (if lsub = 0 then 1 else check 0)
             | _ -> type_err "contains" "Ptr, Ptr");
           nif "split" 2 (function
             | [| s; sep |] ->
                 let parts = String.split_on_char (estr sep).[0] (estr s) in
                 let n = List.length parts in
                 let addr = H.alloc heap ~size:(n + 1) ~tag:Core.Tag.vector in
                 H.write heap addr 0 (Value.Int n);
                 List.iteri
                   (fun i p -> H.write heap addr (1 + i) (of_ocaml heap p))
                   parts;
                 Value.Ptr addr
             | _ -> type_err "split" "Ptr, Ptr");
           nif "join" 2 (function
             | [| Value.Ptr arr; sep |] ->
                 let sep = estr sep in
                 let n =
                   match H.read heap arr 0 with
                   | Value.Int n -> n
                   | _ -> type_err "join" "vector Ptr"
                 in
                 let parts =
                   List.init n (fun i -> estr (H.read heap arr (1 + i)))
                 in
                 str (String.concat sep parts)
             | _ -> type_err "join" "Ptr, Ptr");
           nif "format" 1 (fun args ->
               if Array.length args < 1 then type_err "format" "fmt + args";
               let fmt_s = estr args.(0) in
               let nargs = Array.length args - 1 in
               let buf = Buffer.create 64 in
               let len = String.length fmt_s in
               let ai = ref 0 in
               let i = ref 0 in
               while !i < len do
                 if fmt_s.[!i] = '%' && !i + 1 < len then begin
                   let spec = fmt_s.[!i + 1] in
                   let arg =
                     if !ai < nargs then args.(!ai + 1) else Value.Nil
                   in
                   (match spec with
                   | 's' ->
                       Buffer.add_string buf (estr arg);
                       incr ai
                   | 'd' ->
                       (match arg with
                       | Value.Int n -> Buffer.add_string buf (string_of_int n)
                       | Value.Float f ->
                           Buffer.add_string buf
                             (string_of_int (int_of_float f))
                       | _ -> Buffer.add_string buf "?");
                       incr ai
                   | 'f' ->
                       (match arg with
                       | Value.Float f ->
                           Buffer.add_string buf (string_of_float f)
                       | Value.Int n ->
                           Buffer.add_string buf
                             (string_of_float (float_of_int n))
                       | _ -> Buffer.add_string buf "?");
                       incr ai
                   | 'g' ->
                       (match arg with
                       | Value.Float f ->
                           Buffer.add_string buf (Printf.sprintf "%g" f)
                       | Value.Int n -> Buffer.add_string buf (string_of_int n)
                       | _ -> Buffer.add_string buf "?");
                       incr ai
                   | '%' -> Buffer.add_char buf '%'
                   | c ->
                       Buffer.add_char buf '%';
                       Buffer.add_char buf c);
                   i := !i + 2
                 end
                 else begin
                   Buffer.add_char buf fmt_s.[!i];
                   incr i
                 end
               done;
               str (Buffer.contents buf));
         ])
end
