open Core

type symbol_registry = Symbol.registry

type frame = {
  return_pc : int;
  ret_dst : int;
  reg_base : int;
  program : bytes;
  handler_base : int;
  is_continuation_frame : bool;
}

type handler = { handler_pc : int; handler_frame : int; handler_reg : int }

type con_state =
  | Con_suspended of {
      mutable resume_pc : int;
      saved_regs : Value.t array;
      caller_regs : Value.t array;
    }
  | Con_running of { caller_regs : Value.t array }
  | Con_dead

type continuation = {
  id : int;
  mutable state : con_state;
  program : bytes;
  constants : Value.t array;
}

module type TRACER = sig
  type ctx

  val on_instr : ctx -> pc:int -> op:int -> unit
  val on_call : ctx -> pc:int -> target:int -> unit
  val on_ret : ctx -> pc:int -> unit
  val on_throw : ctx -> pc:int -> unit
  val on_con_new : ctx -> pc:int -> int
  val on_con_yield : ctx -> con_id:int -> pc:int -> unit
  val on_con_resume : ctx -> con_id:int -> pc:int -> unit
  val on_reg_write : ctx -> reg:int -> value:Value.t -> unit
end

module No_tracing = struct
  type ctx = unit

  let on_instr () ~pc:_ ~op:_ = ()
  let on_call () ~pc:_ ~target:_ = ()
  let on_ret () ~pc:_ = ()
  let on_throw () ~pc:_ = ()
  let on_con_new () ~pc:_ = 0
  let on_con_yield () ~con_id:_ ~pc:_ = ()
  let on_con_resume () ~con_id:_ ~pc:_ = ()
  let on_reg_write () ~reg:_ ~value:_ = ()
end

module Tracing = struct
  type ctx = {
    mutable instrs : (int * int * int) list;
    mutable calls : (int * int * int) list;
    mutable rets : (int * int) list;
    mutable throws : (int * int) list;
    mutable con_news : (int * int) list;
    mutable con_yields : (int * int * int) list;
    mutable con_resumes : (int * int * int) list;
    mutable con_count : int;
    mutable tick : int;
    mutable reg_writes : (int * int * Value.t) list;
  }

  let make () =
    {
      instrs = [];
      calls = [];
      rets = [];
      throws = [];
      con_news = [];
      con_yields = [];
      con_resumes = [];
      con_count = 0;
      tick = 0;
      reg_writes = [];
    }

  let on_instr ctx ~pc ~op =
    ctx.instrs <- (ctx.tick, pc, op) :: ctx.instrs;
    ctx.tick <- ctx.tick + 1

  let on_call ctx ~pc ~target = ctx.calls <- (ctx.tick, pc, target) :: ctx.calls
  let on_ret ctx ~pc = ctx.rets <- (ctx.tick, pc) :: ctx.rets
  let on_throw ctx ~pc = ctx.throws <- (ctx.tick, pc) :: ctx.throws

  let on_con_new ctx ~pc =
    let id = ctx.con_count in
    ctx.con_news <- (ctx.tick, pc) :: ctx.con_news;
    ctx.con_count <- ctx.con_count + 1;
    id

  let on_con_yield ctx ~con_id ~pc =
    ctx.con_yields <- (ctx.tick, con_id, pc) :: ctx.con_yields

  let on_con_resume ctx ~con_id ~pc =
    ctx.con_resumes <- (ctx.tick, con_id, pc) :: ctx.con_resumes

  let on_reg_write ctx ~reg ~value =
    ctx.reg_writes <- (ctx.tick, reg, value) :: ctx.reg_writes
end

module type S = sig
  type tracer_ctx
  type heap_tracer_ctx
  type heap_t
  type t

  val create : Config.t -> bytes -> heap_tracer_ctx -> tracer_ctx -> t
  val run : t -> unit
  val step : t -> unit
  val reset : t -> bytes -> unit
  val is_done : t -> bool
  val get_reg : t -> int -> Value.t
  val set_reg : t -> int -> Value.t -> unit
  val set_const : t -> int -> Value.t -> unit
  val get_status : t -> string
  val heap : t -> heap_t
  val symbols : t -> symbol_registry
  val set_sym : t -> int -> Value.t -> unit
end

module Make (H : Heap.S) (T : TRACER) :
  S
    with type tracer_ctx = T.ctx
     and type heap_tracer_ctx = H.tracer_ctx
     and type heap_t = H.t = struct
  type tracer_ctx = T.ctx
  type heap_tracer_ctx = H.tracer_ctx
  type heap_t = H.t

  module G = Gc.Make (H)

  let continuation_id : continuation Type.Id.t = Type.Id.make ()

  type status = Running | Halted | Fault of string

  type t = {
    heap : H.t;
    gc : Config.Gc.t;
    constants : Value.t array;
    mutable program : bytes;
    regs : Value.t array;
    frames : frame array;
    mutable frame_sp : int;
    handlers : handler array;
    mutable handler_sp : int;
    mutable pc : int;
    mutable status : status;
    tracer_ctx : T.ctx;
    symbols : symbol_registry;
    mutable current_continuation : continuation option;
    mutable live_continuations : continuation list;
  }

  let create (cfg : Config.t) program heap_ctx tracer_ctx =
    {
      heap = H.create cfg.heap heap_ctx;
      gc = cfg.gc;
      constants = Array.make 4096 Value.Nil;
      program;
      regs = Array.make 256 Value.Nil;
      frames =
        Array.init cfg.vm.call_depth (fun _ ->
            {
              return_pc = 0;
              ret_dst = 0;
              reg_base = 0;
              program;
              handler_base = 0;
              is_continuation_frame = false;
            });
      frame_sp = 0;
      handlers =
        Array.init cfg.vm.exception_depth (fun _ ->
            { handler_pc = 0; handler_frame = 0; handler_reg = 0 });
      handler_sp = 0;
      pc = 0;
      status = Running;
      tracer_ctx;
      symbols = Symbol.create ();
      current_continuation = None;
      live_continuations = [];
    }

  let collect_continuation_roots con =
    match con.state with
    | Con_suspended { saved_regs; caller_regs; _ } ->
        [| saved_regs; caller_regs |]
    | Con_running { caller_regs } -> [| caller_regs |]
    | Con_dead -> [||]

  let roots vm =
    let con_roots =
      Array.concat (List.map collect_continuation_roots vm.live_continuations)
    in
    Array.append [| vm.regs; vm.constants |] con_roots

  let fault vm msg = vm.status <- Fault msg

  let current_program vm =
    if vm.frame_sp = 0 then vm.program else vm.frames.(vm.frame_sp - 1).program

  let current_base vm =
    if vm.frame_sp = 0 then 0 else vm.frames.(vm.frame_sp - 1).reg_base

  let current_handler_base vm =
    if vm.frame_sp = 0 then 0 else vm.frames.(vm.frame_sp - 1).handler_base

  let maybe_gc vm =
    if H.needs_minor_gc vm.heap then
      match G.run vm.heap vm.gc ~roots:(roots vm) Minor with
      | Some Gc.Major -> G.run vm.heap vm.gc ~roots:(roots vm) Major |> ignore
      | _ -> ()

  let rd_u8 prog pc = Bytes.get_uint8 prog pc

  let rd_i32 prog pc =
    let b0 = Bytes.get_uint8 prog pc in
    let b1 = Bytes.get_uint8 prog (pc + 1) in
    let b2 = Bytes.get_uint8 prog (pc + 2) in
    let b3 = Bytes.get_uint8 prog (pc + 3) in
    b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

  let rd_i64 prog pc =
    let lo = rd_i32 prog pc in
    let hi = rd_i32 prog (pc + 4) in
    Int64.logor (Int64.of_int lo) (Int64.shift_left (Int64.of_int hi) 32)

  let rd_f64 prog pc = Int64.float_of_bits (rd_i64 prog pc)

  let throw_signal vm (s : Exception.signal) =
    T.on_throw vm.tracer_ctx ~pc:vm.pc;
    let code = Exception.signal_code s in
    if vm.handler_sp = 0 then
      fault vm
        (Format.asprintf "uncaught signal: %s" (Exception.signal_message s))
    else begin
      let h = vm.handlers.(vm.handler_sp - 1) in
      vm.handler_sp <- vm.handler_sp - 1;
      while vm.frame_sp > h.handler_frame do
        vm.frame_sp <- vm.frame_sp - 1
      done;
      vm.regs.(h.handler_reg) <- Value.Int code;
      vm.pc <- h.handler_pc;
      vm.status <- Running
    end

  let do_call vm ~target ~arg_start ~ret_dst ~prog ~is_continuation_frame =
    if vm.frame_sp >= Array.length vm.frames then fault vm "call stack overflow"
    else begin
      T.on_call vm.tracer_ctx ~pc:vm.pc ~target;
      let base = current_base vm in
      vm.frames.(vm.frame_sp) <-
        {
          return_pc = vm.pc;
          ret_dst = base + ret_dst;
          reg_base = base + arg_start;
          program = prog;
          handler_base = vm.handler_sp;
          is_continuation_frame;
        };
      vm.frame_sp <- vm.frame_sp + 1;
      vm.pc <- target
    end

  let do_tcall vm ~target ~arg_start ~prog =
    if vm.frame_sp = 0 then fault vm "tail call from top level"
    else begin
      T.on_call vm.tracer_ctx ~pc:vm.pc ~target;
      let base = current_base vm in
      let frame = vm.frames.(vm.frame_sp - 1) in
      vm.handler_sp <- frame.handler_base;
      vm.frames.(vm.frame_sp - 1) <-
        { frame with reg_base = base + arg_start; program = prog };
      vm.pc <- target
    end

  let resolve_callable vm fn_reg =
    let base = current_base vm in
    match vm.regs.(base + fn_reg) with
    | Value.NativeFun _ as v -> v
    | Value.Ptr addr when H.get_tag vm.heap addr = Tag.closure -> Value.Ptr addr
    | v -> v

  let do_dcall vm ~fn_reg ~arg_start ~arg_end ~ret_dst =
    let base = current_base vm in
    let len = if arg_end >= arg_start then arg_end - arg_start + 1 else 0 in
    let args = Array.sub vm.regs (base + arg_start) len in
    match resolve_callable vm fn_reg with
    | Value.NativeFun fn -> (
        match fn args with
        | v -> vm.regs.(base + ret_dst) <- v
        | exception Exception.Signal s -> throw_signal vm s
        | exception Exception.Panic p -> fault vm (Exception.panic_to_string p))
    | Value.Ptr addr -> (
        let code_ptr = H.read vm.heap addr 0 in
        match Value.get_native Value.bytecode_id code_ptr with
        | Some code ->
            do_call vm ~target:0 ~arg_start ~ret_dst ~prog:code
              ~is_continuation_frame:false
        | None -> fault vm "DynCall: closure has invalid code pointer")
    | _ -> fault vm "DynCall: expected NativeFun or closure"

  let do_tdcall vm ~fn_reg ~arg_start ~arg_end =
    let base = current_base vm in
    let len = if arg_end >= arg_start then arg_end - arg_start + 1 else 0 in
    let args = Array.sub vm.regs (base + arg_start) len in
    match resolve_callable vm fn_reg with
    | Value.NativeFun fn -> (
        match fn args with
        | _ -> ()
        | exception Exception.Signal s -> throw_signal vm s
        | exception Exception.Panic p -> fault vm (Exception.panic_to_string p))
    | Value.Ptr addr -> (
        let code_ptr = H.read vm.heap addr 0 in
        match Value.get_native Value.bytecode_id code_ptr with
        | Some code -> do_tcall vm ~target:0 ~arg_start ~prog:code
        | None -> fault vm "TailDynCall: closure has invalid code pointer")
    | _ -> fault vm "TailDynCall: expected NativeFun or closure"

  let step vm =
    match vm.status with
    | Halted | Fault _ -> ()
    | Running ->
        let pc = vm.pc in
        let prog = current_program vm in
        let base = current_base vm in
        if pc < 0 || pc >= Bytes.length prog then
          fault vm (Format.asprintf "pc %d out of bounds" pc)
        else begin
          let op = rd_u8 prog pc in
          T.on_instr vm.tracer_ctx ~pc ~op;
          let a = rd_u8 prog (pc + 1) in
          let b = rd_u8 prog (pc + 2) in
          let c = rd_u8 prog (pc + 3) in
          let pc4 = pc + 4 in
          let pc8 = pc + 8 in
          let pc12 = pc + 12 in
          match op with
          | 0x00 -> vm.pc <- pc4
          | 0x01 -> vm.status <- Halted
          | 0x02 ->
              let v = vm.regs.(base + b) in
              vm.regs.(base + a) <- v;
              T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
              vm.pc <- pc4
          | 0x03 ->
              let v = Value.Int (rd_i32 prog pc4) in
              vm.regs.(base + a) <- v;
              T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
              vm.pc <- pc8
          | 0x04 ->
              let align = (pc4 + 7) land lnot 7 in
              let v = Value.Float (rd_f64 prog align) in
              vm.regs.(base + a) <- v;
              T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
              vm.pc <- align + 8
          | 0x05 ->
              let v = Value.Bool (b <> 0) in
              vm.regs.(base + a) <- v;
              T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
              vm.pc <- pc4
          | 0x06 ->
              vm.regs.(base + a) <- Value.Nil;
              T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:Value.Nil;
              vm.pc <- pc4
          | 0x07 ->
              let idx = rd_i32 prog pc4 in
              if idx < 0 || idx >= Array.length vm.constants then
                fault vm
                  (Format.asprintf "LoadK: constant index %d out of range" idx)
              else begin
                let v = vm.constants.(idx) in
                vm.regs.(base + a) <- v;
                T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                vm.pc <- pc8
              end
          | 0x08 -> (
              let id = rd_i64 prog pc4 in
              match Symbol.resolve vm.symbols id with
              | Some v ->
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc12
              | None ->
                  fault vm (Format.asprintf "LoadS: unresolved symbol 0x%Lx" id)
              )
          | 0x10 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x + y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Add: expected Int")
          | 0x11 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x - y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Sub: expected Int")
          | 0x12 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x * y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Mul: expected Int")
          | 0x13 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int _, Value.Int 0 ->
                  throw_signal vm (Exception.Div_by_zero "division by zero")
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x / y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Div: expected Int")
          | 0x14 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int _, Value.Int 0 ->
                  throw_signal vm (Exception.Div_by_zero "modulo by zero")
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x mod y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Mod: expected Int")
          | 0x15 -> (
              match vm.regs.(base + b) with
              | Value.Int x ->
                  let v = Value.Int (x + rd_i32 prog pc4) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc8
              | _ -> fault vm "AddI: expected Int")
          | 0x16 -> (
              match vm.regs.(base + b) with
              | Value.Int x ->
                  let v = Value.Int (x - rd_i32 prog pc4) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc8
              | _ -> fault vm "SubI: expected Int")
          | 0x17 -> (
              match vm.regs.(base + b) with
              | Value.Int x ->
                  let v = Value.Int (x * rd_i32 prog pc4) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc8
              | _ -> fault vm "MulI: expected Int")
          | 0x18 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Float x, Value.Float y ->
                  let v = Value.Float (x +. y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "AddF: expected Float")
          | 0x19 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Float x, Value.Float y ->
                  let v = Value.Float (x -. y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "SubF: expected Float")
          | 0x1A -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Float x, Value.Float y ->
                  let v = Value.Float (x *. y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "MulF: expected Float")
          | 0x1B -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Float x, Value.Float y ->
                  let v = Value.Float (x /. y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "DivF: expected Float")
          | 0x20 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x land y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "And: expected Int")
          | 0x21 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x lor y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Or: expected Int")
          | 0x22 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x lxor y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Xor: expected Int")
          | 0x23 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x lsl y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Shl: expected Int")
          | 0x24 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x asr y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Shr: expected Int")
          | 0x25 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Int (x lsr y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "ShrU: expected Int")
          | 0x26 -> (
              match vm.regs.(base + b) with
              | Value.Int x ->
                  let v = Value.Int (x lsl rd_i32 prog pc4) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc8
              | _ -> fault vm "ShlI: expected Int")
          | 0x27 -> (
              match vm.regs.(base + b) with
              | Value.Int x ->
                  let v = Value.Int (x asr rd_i32 prog pc4) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc8
              | _ -> fault vm "ShrI: expected Int")
          | 0x28 -> (
              match vm.regs.(base + b) with
              | Value.Int x ->
                  let v = Value.Int (x lsr rd_i32 prog pc4) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc8
              | _ -> fault vm "ShrUI: expected Int")
          | 0x30 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Bool (x = y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Eq: expected Int")
          | 0x31 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Bool (x <> y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Ne: expected Int")
          | 0x32 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Bool (x < y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Lt: expected Int")
          | 0x33 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Bool (x land max_int < y land max_int) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "LtU: expected Int")
          | 0x34 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Bool (x <= y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Lte: expected Int")
          | 0x35 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Int x, Value.Int y ->
                  let v = Value.Bool (x land max_int <= y land max_int) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "LteU: expected Int")
          | 0x36 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Float x, Value.Float y ->
                  let v = Value.Bool (x = y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "EqF: expected Float")
          | 0x37 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Float x, Value.Float y ->
                  let v = Value.Bool (x <> y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "NeF: expected Float")
          | 0x38 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Float x, Value.Float y ->
                  let v = Value.Bool (x < y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "LtF: expected Float")
          | 0x39 -> (
              match (vm.regs.(base + b), vm.regs.(base + c)) with
              | Value.Float x, Value.Float y ->
                  let v = Value.Bool (x <= y) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "LteF: expected Float")
          | 0x40 -> (
              match vm.regs.(base + b) with
              | Value.Int n ->
                  let v = Value.Float (float_of_int n) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "I2F: expected Int")
          | 0x41 -> (
              match vm.regs.(base + b) with
              | Value.Float f ->
                  let v = Value.Int (int_of_float f) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "F2I: expected Float")
          | 0x42 ->
              let v = Value.Int (Value.type_of vm.regs.(base + b)) in
              vm.regs.(base + a) <- v;
              T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
              vm.pc <- pc4
          | 0x50 -> (
              let tag = rd_i32 prog pc4 in
              let size = rd_i32 prog pc8 in
              maybe_gc vm;
              match H.alloc vm.heap ~size ~tag with
              | addr ->
                  let v = Value.Ptr addr in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc12
              | exception Exception.Panic p ->
                  fault vm (Exception.panic_to_string p))
          | 0x51 -> (
              let field = rd_i32 prog pc4 in
              match vm.regs.(base + b) with
              | Value.Ptr addr -> (
                  match H.read vm.heap addr field with
                  | v ->
                      vm.regs.(base + a) <- v;
                      T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                      vm.pc <- pc8
                  | exception Exception.Signal s -> throw_signal vm s)
              | _ -> fault vm "GetField: expected Ptr")
          | 0x52 -> (
              let field = rd_i32 prog pc4 in
              match vm.regs.(base + a) with
              | Value.Ptr addr -> (
                  match H.write vm.heap addr field vm.regs.(base + b) with
                  | () -> vm.pc <- pc8
                  | exception Exception.Signal s -> throw_signal vm s)
              | _ -> fault vm "SetField: expected Ptr")
          | 0x53 -> (
              match vm.regs.(base + b) with
              | Value.Ptr addr ->
                  let v = Value.Int (H.get_tag vm.heap addr) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "GetTag: expected Ptr")
          | 0x54 -> (
              match vm.regs.(base + b) with
              | Value.Ptr addr ->
                  let v = Value.Int (H.get_size vm.heap addr) in
                  vm.regs.(base + a) <- v;
                  T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                  vm.pc <- pc4
              | _ -> fault vm "Len: expected Ptr")
          | 0x60 -> vm.pc <- rd_i32 prog pc4
          | 0x61 -> (
              match vm.regs.(base + a) with
              | Value.Bool false | Value.Int 0 -> vm.pc <- rd_i32 prog pc4
              | Value.Bool true | Value.Int _ -> vm.pc <- pc8
              | _ -> fault vm "Jz: expected Bool or Int")
          | 0x62 -> (
              match vm.regs.(base + a) with
              | Value.Bool false | Value.Int 0 -> vm.pc <- pc8
              | Value.Bool true | Value.Int _ -> vm.pc <- rd_i32 prog pc4
              | _ -> fault vm "Jnz: expected Bool or Int")
          | 0x70 ->
              let target = rd_i32 prog pc4 in
              vm.pc <- pc8;
              do_call vm ~target ~arg_start:a ~ret_dst:c
                ~prog:(current_program vm) ~is_continuation_frame:false;
              ignore b
          | 0x71 ->
              let target = rd_i32 prog pc4 in
              vm.pc <- pc8;
              do_tcall vm ~target ~arg_start:a ~prog:(current_program vm);
              ignore b
          | 0x72 ->
              let r = rd_u8 prog pc4 in
              vm.pc <- pc8;
              do_dcall vm ~fn_reg:a ~arg_start:b ~arg_end:c ~ret_dst:r;
              maybe_gc vm
          | 0x73 ->
              vm.pc <- pc4;
              do_tdcall vm ~fn_reg:a ~arg_start:b ~arg_end:c;
              maybe_gc vm
          | 0x74 ->
              if vm.frame_sp = 0 then fault vm "Ret: empty call stack"
              else begin
                T.on_ret vm.tracer_ctx ~pc;
                let result = vm.regs.(base + a) in
                let frame = vm.frames.(vm.frame_sp - 1) in
                let was_continuation_frame = frame.is_continuation_frame in
                vm.handler_sp <- frame.handler_base;
                vm.frame_sp <- vm.frame_sp - 1;
                vm.pc <- frame.return_pc;
                if was_continuation_frame then begin
                  (match vm.current_continuation with
                  | Some con ->
                      (match con.state with
                      | Con_running { caller_regs } ->
                          Array.blit caller_regs 0 vm.regs 0 256
                      | _ -> ());
                      con.state <- Con_dead;
                      vm.live_continuations <-
                        List.filter
                          (fun c -> c.id <> con.id)
                          vm.live_continuations
                  | None -> ());
                  vm.current_continuation <- None
                end;
                vm.regs.(frame.ret_dst) <- result;
                T.on_reg_write vm.tracer_ctx ~reg:frame.ret_dst ~value:result
              end
          | 0x80 ->
              let handler_pc = rd_i32 prog pc4 in
              if vm.handler_sp >= Array.length vm.handlers then
                fault vm "Try: handler stack overflow"
              else begin
                vm.handlers.(vm.handler_sp) <-
                  {
                    handler_pc;
                    handler_frame = vm.frame_sp;
                    handler_reg = base + a;
                  };
                vm.handler_sp <- vm.handler_sp + 1;
                vm.pc <- pc8
              end
          | 0x81 -> (
              T.on_throw vm.tracer_ctx ~pc;
              match vm.regs.(base + a) with
              | Value.Int code ->
                  throw_signal vm
                    (Exception.Custom (code, Format.asprintf "signal %d" code))
              | _ -> fault vm "Throw: expected Int error code")
          | 0x82 ->
              if vm.handler_sp = 0 then fault vm "EndTry without Try"
              else if vm.handler_sp <= current_handler_base vm then
                fault vm "EndTry: would pop handler from outer frame"
              else begin
                vm.handler_sp <- vm.handler_sp - 1;
                vm.pc <- pc4
              end
          | 0x90 -> (
              match vm.regs.(base + b) with
              | Value.Ptr addr when H.get_tag vm.heap addr = Tag.closure -> (
                  let code_ptr = H.read vm.heap addr 0 in
                  match Value.get_native Value.bytecode_id code_ptr with
                  | Some code ->
                      let con_id = T.on_con_new vm.tracer_ctx ~pc in
                      let con =
                        {
                          id = con_id;
                          state =
                            Con_suspended
                              {
                                resume_pc = 0;
                                saved_regs = Array.make 256 Value.Nil;
                                caller_regs = Array.make 256 Value.Nil;
                              };
                          program = code;
                          constants = vm.constants;
                        }
                      in
                      let con_ptr =
                        Value.make_native ~tag:continuation_id ~finalizer:None
                          con
                      in
                      vm.live_continuations <- con :: vm.live_continuations;
                      vm.regs.(base + a) <- con_ptr;
                      T.on_reg_write vm.tracer_ctx ~reg:(base + a)
                        ~value:con_ptr;
                      vm.pc <- pc4
                  | None -> fault vm "ConNew: closure has invalid code pointer")
              | Value.NativeFun _ ->
                  fault vm "ConNew: native functions cannot be continuations"
              | _ -> fault vm "ConNew: expected closure Ptr")
          | 0x91 -> (
              match vm.current_continuation with
              | None -> fault vm "ConYield: no active continuation"
              | Some con ->
                  T.on_con_yield vm.tracer_ctx ~con_id:con.id ~pc;
                  let yield_val = vm.regs.(base + b) in
                  let frame = vm.frames.(vm.frame_sp - 1) in
                  let saved_regs = Array.copy vm.regs in
                  let caller_regs =
                    match con.state with
                    | Con_running { caller_regs } -> caller_regs
                    | _ ->
                        fault vm "ConYield: continuation not in running state";
                        vm.regs
                  in
                  con.state <-
                    Con_suspended { resume_pc = pc4; saved_regs; caller_regs };
                  vm.current_continuation <- None;
                  vm.handler_sp <- frame.handler_base;
                  vm.frame_sp <- vm.frame_sp - 1;
                  vm.pc <- frame.return_pc;
                  Array.blit caller_regs 0 vm.regs 0 256;
                  vm.regs.(frame.ret_dst) <- yield_val;
                  T.on_reg_write vm.tracer_ctx ~reg:frame.ret_dst
                    ~value:yield_val)
          | 0x92 -> (
              match vm.regs.(base + b) with
              | Value.NativePtr _ as con_v -> (
                  match Value.get_native continuation_id con_v with
                  | Some con -> (
                      match con.state with
                      | Con_dead -> fault vm "ConResume: continuation is dead"
                      | Con_running _ ->
                          fault vm "ConResume: continuation already running"
                      | Con_suspended { resume_pc; saved_regs; _ } ->
                          T.on_con_resume vm.tracer_ctx ~con_id:con.id ~pc;
                          let arg = vm.regs.(base + c) in
                          let caller_regs = Array.copy vm.regs in
                          con.state <- Con_running { caller_regs };
                          vm.current_continuation <- Some con;
                          Array.blit saved_regs 0 vm.regs 0 256;
                          vm.regs.(0) <- arg;
                          T.on_reg_write vm.tracer_ctx ~reg:0 ~value:arg;
                          vm.pc <- pc4;
                          do_call vm ~target:resume_pc ~arg_start:0 ~ret_dst:a
                            ~prog:con.program ~is_continuation_frame:true)
                  | None -> fault vm "ConResume: not a valid continuation")
              | _ -> fault vm "ConResume: expected continuation")
          | 0x93 -> (
              match vm.regs.(base + b) with
              | Value.NativePtr _ as con_v -> (
                  match Value.get_native continuation_id con_v with
                  | Some con ->
                      let v =
                        Value.Int
                          (match con.state with
                          | Con_suspended _ -> 0
                          | Con_running _ -> 1
                          | Con_dead -> 2)
                      in
                      vm.regs.(base + a) <- v;
                      T.on_reg_write vm.tracer_ctx ~reg:(base + a) ~value:v;
                      vm.pc <- pc4
                  | None -> fault vm "ConStatus: not a valid continuation")
              | _ -> fault vm "ConStatus: expected continuation")
          | op -> fault vm (Format.asprintf "unknown opcode 0x%02X" op)
        end

  let set_sym vm id v =
    let symbol_id = Int64.of_int id in
    Hashtbl.replace vm.symbols.Symbol.by_id symbol_id (Symbol.Resolved v)

  let run vm =
    while vm.status = Running do
      step vm
    done

  let reset vm program =
    vm.program <- program;
    vm.pc <- 0;
    vm.frame_sp <- 0;
    vm.handler_sp <- 0;
    vm.status <- Running;
    vm.current_continuation <- None;
    vm.live_continuations <- [];
    Array.fill vm.regs 0 (Array.length vm.regs) Value.Nil

  let is_done vm = match vm.status with Running -> false | _ -> true
  let get_reg vm i = vm.regs.(i)
  let set_reg vm i v = vm.regs.(i) <- v
  let set_const vm i v = vm.constants.(i) <- v
  let heap vm = vm.heap
  let symbols vm = vm.symbols

  let get_status vm =
    match vm.status with
    | Running -> "running"
    | Halted -> "halted"
    | Fault m -> Format.asprintf "fault: %s" m
end
