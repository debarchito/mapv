open Core

let magic = "MAPV"
let edition = 2026
let sec_imports = 0x00
let sec_constants = 0x01
let sec_functions = 0x02
let sec_debug = 0x03

type func = { name : string; arity : int; code : Instr.t array }
type debug_entry = { instr_offset : int; line : int; col : int; file_idx : int }
type debug_info = { func_idx : int; entries : debug_entry array }
type import = { symbol_id : int64; name : string }

type program = {
  edition : int;
  imports : import array;
  constants : Value.t array;
  funcs : func array;
  debug : debug_info array;
}

let check_constants constants =
  Array.iteri
    (fun i v ->
      match v with
      | Value.NativeFun _ ->
          raise
            (Exception.Panic
               (Exception.Invalid_registry
                  (Format.asprintf
                     "serializer: constant[%d] is NativeFun — not serializable"
                     i)))
      | Value.NativePtr _ ->
          raise
            (Exception.Panic
               (Exception.Invalid_registry
                  (Format.asprintf
                     "serializer: constant[%d] is NativePtr — not serializable"
                     i)))
      | _ -> ())
    constants

let validate_imports imports registry =
  Array.iter
    (fun imp ->
      let id = Int64.to_int imp.symbol_id in
      match Exception.Registry.name_of registry id with
      | None ->
          raise
            (Exception.Panic
               (Exception.Invalid_registry
                  (Format.asprintf
                     "serializer: import '%s' (sym=0x%Lx) not found in registry"
                     imp.name imp.symbol_id)))
      | Some _ -> ())
    imports

module Write = struct
  let align buf n =
    let remainder = Buffer.length buf mod n in
    if remainder <> 0 then
      for _ = 1 to n - remainder do
        Buffer.add_char buf '\x00'
      done

  let u8 buf v = Buffer.add_uint8 buf v
  let u16 buf v = Buffer.add_uint16_le buf v
  let u32 buf v = Buffer.add_int32_le buf (Int32.of_int v)
  let u64 buf v = Buffer.add_int64_le buf v

  let f64 buf v =
    align buf 8;
    u64 buf (Int64.bits_of_float v)

  let u64a buf v =
    align buf 8;
    u64 buf v

  let string buf s =
    u32 buf (String.length s);
    Buffer.add_string buf s

  let i32 buf v = Buffer.add_int32_le buf (Int32.of_int v)
  let i64v buf v = u64 buf v

  let value buf = function
    | Value.Nil -> u8 buf 0
    | Value.Bool b ->
        u8 buf 1;
        u8 buf (if b then 1 else 0)
    | Value.Int n ->
        u8 buf 2;
        u64a buf (Int64.of_int n)
    | Value.Float f ->
        u8 buf 3;
        f64 buf f
    | Value.Ptr p ->
        u8 buf 4;
        u32 buf p
    | Value.NativeFun _ | Value.NativePtr _ ->
        raise
          (Exception.Panic
             (Exception.Invalid_registry
                "serializer: attempted to serialize non-serializable value"))

  let instr_size = function
    | Instr.Nop | Instr.Halt -> 4
    | Instr.Mov _ -> 4
    | Instr.Load _ -> 8
    | Instr.LoadF _ -> 12
    | Instr.LoadB _ -> 4
    | Instr.LoadNil _ -> 4
    | Instr.LoadK _ -> 8
    | Instr.LoadS _ -> 12
    | Instr.Add _ | Instr.Sub _ | Instr.Mul _ | Instr.Div _ | Instr.Mod _ -> 4
    | Instr.AddI _ | Instr.SubI _ | Instr.MulI _ -> 8
    | Instr.AddF _ | Instr.SubF _ | Instr.MulF _ | Instr.DivF _ -> 4
    | Instr.And _ | Instr.Or _ | Instr.Xor _ -> 4
    | Instr.Shl _ | Instr.Shr _ | Instr.ShrU _ -> 4
    | Instr.ShlI _ | Instr.ShrI _ | Instr.ShrUI _ -> 8
    | Instr.Eq _ | Instr.Ne _ | Instr.Lt _ | Instr.LtU _ | Instr.Lte _
    | Instr.LteU _ | Instr.EqF _ | Instr.NeF _ | Instr.LtF _ | Instr.LteF _ ->
        4
    | Instr.I2F _ | Instr.F2I _ | Instr.TypeOf _ -> 4
    | Instr.Alloc _ -> 12
    | Instr.GetField _ -> 8
    | Instr.SetField _ -> 8
    | Instr.GetTag _ | Instr.Len _ -> 4
    | Instr.Jmp _ -> 8
    | Instr.Jz _ | Instr.Jnz _ -> 8
    | Instr.Call _ -> 12
    | Instr.TailCall _ -> 12
    | Instr.DynCall _ -> 8
    | Instr.TailDynCall _ -> 4
    | Instr.Ret _ -> 4
    | Instr.Try _ -> 8
    | Instr.Throw _ -> 4
    | Instr.EndTry -> 4
    | Instr.ConNew _ | Instr.ConYield _ -> 4
    | Instr.ConResume _ | Instr.ConStatus _ -> 4

  let compute_byte_offsets code =
    let offsets = Array.make (Array.length code) 0 in
    let pos = ref 0 in
    for i = 0 to Array.length code - 1 do
      offsets.(i) <- !pos;
      pos := !pos + instr_size code.(i)
    done;
    offsets

  let instr buf (offsets : int array) i =
    let header op a b c =
      u8 buf op;
      u8 buf a;
      u8 buf b;
      u8 buf c
    in
    match i with
    | Instr.Nop -> header 0x00 0 0 0
    | Instr.Halt -> header 0x01 0 0 0
    | Instr.Mov (d, s) -> header 0x02 d s 0
    | Instr.Load (d, n) ->
        header 0x03 d 0 0;
        i32 buf n
    | Instr.LoadF (d, f) ->
        header 0x04 d 0 0;
        f64 buf f
    | Instr.LoadB (d, b) -> header 0x05 d (if b then 1 else 0) 0
    | Instr.LoadNil d -> header 0x06 d 0 0
    | Instr.LoadK (d, idx) ->
        header 0x07 d 0 0;
        i32 buf idx
    | Instr.LoadS (d, id) ->
        header 0x08 d 0 0;
        i64v buf id
    | Instr.Add (d, a, b) -> header 0x10 d a b
    | Instr.Sub (d, a, b) -> header 0x11 d a b
    | Instr.Mul (d, a, b) -> header 0x12 d a b
    | Instr.Div (d, a, b) -> header 0x13 d a b
    | Instr.Mod (d, a, b) -> header 0x14 d a b
    | Instr.AddI (d, a, i) ->
        header 0x15 d a 0;
        i32 buf i
    | Instr.SubI (d, a, i) ->
        header 0x16 d a 0;
        i32 buf i
    | Instr.MulI (d, a, i) ->
        header 0x17 d a 0;
        i32 buf i
    | Instr.AddF (d, a, b) -> header 0x18 d a b
    | Instr.SubF (d, a, b) -> header 0x19 d a b
    | Instr.MulF (d, a, b) -> header 0x1A d a b
    | Instr.DivF (d, a, b) -> header 0x1B d a b
    | Instr.And (d, a, b) -> header 0x20 d a b
    | Instr.Or (d, a, b) -> header 0x21 d a b
    | Instr.Xor (d, a, b) -> header 0x22 d a b
    | Instr.Shl (d, a, b) -> header 0x23 d a b
    | Instr.Shr (d, a, b) -> header 0x24 d a b
    | Instr.ShrU (d, a, b) -> header 0x25 d a b
    | Instr.ShlI (d, a, i) ->
        header 0x26 d a 0;
        i32 buf i
    | Instr.ShrI (d, a, i) ->
        header 0x27 d a 0;
        i32 buf i
    | Instr.ShrUI (d, a, i) ->
        header 0x28 d a 0;
        i32 buf i
    | Instr.Eq (d, a, b) -> header 0x30 d a b
    | Instr.Ne (d, a, b) -> header 0x31 d a b
    | Instr.Lt (d, a, b) -> header 0x32 d a b
    | Instr.LtU (d, a, b) -> header 0x33 d a b
    | Instr.Lte (d, a, b) -> header 0x34 d a b
    | Instr.LteU (d, a, b) -> header 0x35 d a b
    | Instr.EqF (d, a, b) -> header 0x36 d a b
    | Instr.NeF (d, a, b) -> header 0x37 d a b
    | Instr.LtF (d, a, b) -> header 0x38 d a b
    | Instr.LteF (d, a, b) -> header 0x39 d a b
    | Instr.I2F (d, s) -> header 0x40 d s 0
    | Instr.F2I (d, s) -> header 0x41 d s 0
    | Instr.TypeOf (d, s) -> header 0x42 d s 0
    | Instr.Alloc (d, t, s) ->
        header 0x50 d 0 0;
        i32 buf t;
        i32 buf s
    | Instr.GetField (d, o, f) ->
        header 0x51 d o 0;
        i32 buf f
    | Instr.SetField (o, f, s) ->
        header 0x52 o s 0;
        i32 buf f
    | Instr.GetTag (d, o) -> header 0x53 d o 0
    | Instr.Len (d, o) -> header 0x54 d o 0
    | Instr.Jmp target ->
        header 0x60 0 0 0;
        i32 buf offsets.(target)
    | Instr.Jz (r, target) ->
        header 0x61 r 0 0;
        i32 buf offsets.(target)
    | Instr.Jnz (r, target) ->
        header 0x62 r 0 0;
        i32 buf offsets.(target)
    | Instr.Call (target, s, e, r) ->
        header 0x70 s e r;
        i32 buf offsets.(target)
    | Instr.TailCall (target, s, e) ->
        header 0x71 s e 0;
        i32 buf offsets.(target)
    | Instr.DynCall (f, s, e, r) ->
        header 0x72 f s e;
        u8 buf r;
        u8 buf 0;
        u8 buf 0;
        u8 buf 0
    | Instr.TailDynCall (f, s, e) -> header 0x73 f s e
    | Instr.Ret r -> header 0x74 r 0 0
    | Instr.Try (handler, c) ->
        header 0x80 c 0 0;
        i32 buf offsets.(handler)
    | Instr.Throw r -> header 0x81 r 0 0
    | Instr.EndTry -> header 0x82 0 0 0
    | Instr.ConNew (d, s) -> header 0x90 d s 0
    | Instr.ConYield (d, v) -> header 0x91 d v 0
    | Instr.ConResume (d, f, v) -> header 0x92 d f v
    | Instr.ConStatus (d, f) -> header 0x93 d f 0

  let func buf (f : func) =
    string buf f.name;
    let offsets = compute_byte_offsets f.code in
    u8 buf f.arity;
    align buf 4;
    let n = Array.length f.code in
    u32 buf n;
    for i = 0 to n - 1 do
      instr buf offsets f.code.(i)
    done

  let import buf i =
    i64v buf i.symbol_id;
    string buf i.name

  let debug_entry buf e =
    u32 buf e.instr_offset;
    u32 buf e.line;
    u16 buf e.col;
    u16 buf e.file_idx

  let debug_info buf d =
    u32 buf d.func_idx;
    u32 buf (Array.length d.entries);
    Array.iter (debug_entry buf) d.entries

  let program buf prog =
    check_constants prog.constants;
    Buffer.add_string buf magic;
    u16 buf prog.edition;
    u16 buf 0;
    let compile_sec id data writer =
      let b = Buffer.create 128 in
      u32 b (Array.length data);
      Array.iter (writer b) data;
      (id, b)
    in
    let compiled = [] in
    let compiled =
      if Array.length prog.imports > 0 then
        compile_sec sec_imports prog.imports import :: compiled
      else compiled
    in
    let compiled = compile_sec sec_constants prog.constants value :: compiled in
    let compiled = compile_sec sec_functions prog.funcs func :: compiled in
    let compiled =
      if Array.length prog.debug > 0 then
        compile_sec sec_debug prog.debug debug_info :: compiled
      else compiled
    in
    let compiled = List.rev compiled in
    let sec_count = List.length compiled in
    u32 buf sec_count;
    let header_off = 8 + 4 + (sec_count * 12) in
    let current_off = ref header_off in
    List.iter
      (fun (id, b) ->
        let padding = (8 - (!current_off mod 8)) mod 8 in
        let start = !current_off + padding in
        u32 buf id;
        u32 buf start;
        u32 buf (Buffer.length b);
        current_off := start + Buffer.length b)
      compiled;
    List.iter
      (fun (_, b) ->
        align buf 8;
        Buffer.add_buffer buf b)
      compiled
end

module Read = struct
  type cursor = { data : bytes; mutable pos : int }

  let make data = { data; pos = 0 }
  let seek cur p = cur.pos <- p
  let align cur n = cur.pos <- (cur.pos + (n - 1)) land lnot (n - 1)

  let u8 cur =
    let v = Bytes.get_uint8 cur.data cur.pos in
    cur.pos <- cur.pos + 1;
    v

  let u16 cur =
    let v = Bytes.get_uint16_le cur.data cur.pos in
    cur.pos <- cur.pos + 2;
    v

  let u32 cur =
    let v = Int32.to_int (Bytes.get_int32_le cur.data cur.pos) in
    cur.pos <- cur.pos + 4;
    v

  let u64 cur =
    let v = Bytes.get_int64_le cur.data cur.pos in
    cur.pos <- cur.pos + 8;
    v

  let f64 cur =
    align cur 8;
    Int64.float_of_bits (u64 cur)

  let i32 cur = u32 cur
  let i64v cur = u64 cur

  let string cur =
    let len = u32 cur in
    let s = Bytes.sub_string cur.data cur.pos len in
    cur.pos <- cur.pos + len;
    s

  let value cur =
    match u8 cur with
    | 0 -> Value.Nil
    | 1 -> Value.Bool (u8 cur <> 0)
    | 2 ->
        align cur 8;
        Value.Int (Int64.to_int (u64 cur))
    | 3 -> Value.Float (f64 cur)
    | 4 -> Value.Ptr (u32 cur)
    | t ->
        raise
          (Exception.Panic
             (Exception.Alloc_error
                (Format.asprintf "serializer: unknown constant type tag %d" t)))

  let instr cur =
    let op = u8 cur in
    let a = u8 cur in
    let b = u8 cur in
    let c = u8 cur in
    match op with
    | 0x00 -> Instr.Nop
    | 0x01 -> Instr.Halt
    | 0x02 -> Instr.Mov (a, b)
    | 0x03 -> Instr.Load (a, i32 cur)
    | 0x04 -> Instr.LoadF (a, f64 cur)
    | 0x05 -> Instr.LoadB (a, b <> 0)
    | 0x06 -> Instr.LoadNil a
    | 0x07 -> Instr.LoadK (a, i32 cur)
    | 0x08 -> Instr.LoadS (a, i64v cur)
    | 0x10 -> Instr.Add (a, b, c)
    | 0x11 -> Instr.Sub (a, b, c)
    | 0x12 -> Instr.Mul (a, b, c)
    | 0x13 -> Instr.Div (a, b, c)
    | 0x14 -> Instr.Mod (a, b, c)
    | 0x15 -> Instr.AddI (a, b, i32 cur)
    | 0x16 -> Instr.SubI (a, b, i32 cur)
    | 0x17 -> Instr.MulI (a, b, i32 cur)
    | 0x18 -> Instr.AddF (a, b, c)
    | 0x19 -> Instr.SubF (a, b, c)
    | 0x1A -> Instr.MulF (a, b, c)
    | 0x1B -> Instr.DivF (a, b, c)
    | 0x20 -> Instr.And (a, b, c)
    | 0x21 -> Instr.Or (a, b, c)
    | 0x22 -> Instr.Xor (a, b, c)
    | 0x23 -> Instr.Shl (a, b, c)
    | 0x24 -> Instr.Shr (a, b, c)
    | 0x25 -> Instr.ShrU (a, b, c)
    | 0x26 -> Instr.ShlI (a, b, i32 cur)
    | 0x27 -> Instr.ShrI (a, b, i32 cur)
    | 0x28 -> Instr.ShrUI (a, b, i32 cur)
    | 0x30 -> Instr.Eq (a, b, c)
    | 0x31 -> Instr.Ne (a, b, c)
    | 0x32 -> Instr.Lt (a, b, c)
    | 0x33 -> Instr.LtU (a, b, c)
    | 0x34 -> Instr.Lte (a, b, c)
    | 0x35 -> Instr.LteU (a, b, c)
    | 0x36 -> Instr.EqF (a, b, c)
    | 0x37 -> Instr.NeF (a, b, c)
    | 0x38 -> Instr.LtF (a, b, c)
    | 0x39 -> Instr.LteF (a, b, c)
    | 0x40 -> Instr.I2F (a, b)
    | 0x41 -> Instr.F2I (a, b)
    | 0x42 -> Instr.TypeOf (a, b)
    | 0x50 ->
        let t = i32 cur in
        Instr.Alloc (a, t, i32 cur)
    | 0x51 -> Instr.GetField (a, b, i32 cur)
    | 0x52 -> Instr.SetField (a, i32 cur, b)
    | 0x53 -> Instr.GetTag (a, b)
    | 0x54 -> Instr.Len (a, b)
    | 0x60 -> Instr.Jmp (i32 cur)
    | 0x61 -> Instr.Jz (a, i32 cur)
    | 0x62 -> Instr.Jnz (a, i32 cur)
    | 0x70 -> Instr.Call (i32 cur, a, b, c)
    | 0x71 -> Instr.TailCall (i32 cur, a, b)
    | 0x72 ->
        let r = u8 cur in
        cur.pos <- cur.pos + 3;
        Instr.DynCall (a, b, c, r)
    | 0x73 -> Instr.TailDynCall (a, b, c)
    | 0x74 -> Instr.Ret a
    | 0x80 -> Instr.Try (i32 cur, a)
    | 0x81 -> Instr.Throw a
    | 0x82 -> Instr.EndTry
    | 0x90 -> Instr.ConNew (a, b)
    | 0x91 -> Instr.ConYield (a, b)
    | 0x92 -> Instr.ConResume (a, b, c)
    | 0x93 -> Instr.ConStatus (a, b)
    | op ->
        raise
          (Exception.Panic
             (Exception.Alloc_error
                (Format.asprintf "serializer: unknown opcode 0x%02X" op)))

  let func cur =
    let name = string cur in
    let arity = u8 cur in
    align cur 4;
    let n = u32 cur in
    { name; arity; code = Array.init n (fun _ -> instr cur) }

  let import cur =
    let symbol_id = i64v cur in
    let name = string cur in
    { symbol_id; name }

  let debug_entry cur =
    let instr_offset = u32 cur in
    let line = u32 cur in
    let col = u16 cur in
    let file_idx = u16 cur in
    { instr_offset; line; col; file_idx }

  let debug_info cur =
    let func_idx = u32 cur in
    let n = u32 cur in
    { func_idx; entries = Array.init n (fun _ -> debug_entry cur) }

  let program cur =
    let m = Bytes.sub_string cur.data cur.pos 4 in
    if m <> magic then
      raise
        (Exception.Panic
           (Exception.Alloc_error
              (Format.asprintf "serializer: invalid magic %S (expected %S)" m
                 magic)));
    cur.pos <- 4;
    let ed = u16 cur in
    if ed <> edition then
      raise
        (Exception.Panic
           (Exception.Alloc_error
              (Format.asprintf "serializer: unknown edition %d (expected %d)" ed
                 edition)));
    cur.pos <- 8;
    let n_sec = u32 cur in
    let secs =
      Array.init n_sec (fun _ ->
          let id = u32 cur in
          let off = u32 cur in
          let sz = u32 cur in
          (id, off, sz))
    in
    let get_sec id =
      Array.find_map (fun (i, o, _) -> if i = id then Some o else None) secs
    in
    let require_sec id name =
      match get_sec id with
      | Some o -> o
      | None ->
          raise
            (Exception.Panic
               (Exception.Alloc_error
                  (Format.asprintf "serializer: missing required section '%s'"
                     name)))
    in
    let load_array id loader =
      match get_sec id with
      | None -> [||]
      | Some o ->
          seek cur o;
          let n = u32 cur in
          Array.init n (fun _ -> loader cur)
    in
    let load_required_array id name loader =
      let o = require_sec id name in
      seek cur o;
      let n = u32 cur in
      Array.init n (fun _ -> loader cur)
    in
    {
      edition = ed;
      imports = load_array sec_imports import;
      constants = load_required_array sec_constants "constants" value;
      funcs = load_required_array sec_functions "functions" func;
      debug = load_array sec_debug debug_info;
    }
end

let serialize prog =
  let buf = Buffer.create 1024 in
  Write.program buf prog;
  Buffer.to_bytes buf

let deserialize data = Read.program (Read.make data)

let serialize_to_file path prog =
  let b = serialize prog in
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_bytes oc b);
  b

let deserialize_from_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let b = Bytes.create n in
      really_input ic b 0 n;
      deserialize b)
