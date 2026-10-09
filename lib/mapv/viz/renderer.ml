open Raylib
open Core
module Ev = Trace.Event

type view_mode = Registers | Heap | Both

type live_hooks = {
  step : unit -> unit;
  is_done : unit -> bool;
  status : unit -> string;
}

type playback = {
  mutable tick : int;
  mutable playing : bool;
  mutable speed : float;
  mutable accum : float;
}

type highlight = {
  mutable reg : int option;
  mutable addr : int option;
  mutable age : float;
}

type inspector = { mutable active : bool; mutable addr : int }
type scroll = { mutable offset : int }

type state = {
  model : Trace_model.t;
  pb : playback;
  hl : highlight;
  insp : inspector;
  reg_scroll : scroll;
  stack_scroll : scroll;
  heap_scroll : scroll;
  con_scroll : scroll;
  mutable mode : view_mode;
  live : live_hooks option;
  mutable follow : bool;
}

let screen_w = 1920
let screen_h = 1080
let scale = 1.75
let sw = int_of_float (float_of_int screen_w /. scale)
let sh = int_of_float (float_of_int screen_h /. scale)
let bar_h = 36
let instr_h = 52
let event_h = 22
let ctrl_h = 52
let pad = 12
let row_h = 22
let fxs = 11
let fsm = 14
let fmd = 18
let flg = 22
let s x = int_of_float (float_of_int x *. scale)
let c r g b = Color.create r g b 255
let ca r g b a = Color.create r g b a
let col_bg = c 9 10 14
let col_panel = c 13 15 20
let col_panel2 = c 16 18 25
let col_border = c 23 26 34
let col_border2 = c 31 35 45
let col_text = c 198 202 214
let col_dim = c 104 111 132
let col_dim2 = c 54 59 74
let col_accent = c 94 158 240
let col_green = c 96 199 128
let col_red = c 214 104 104
let col_yellow = c 214 186 92
let col_orange = c 224 152 88
let col_purple = c 156 128 226
let col_teal = c 96 186 180
let col_young = c 78 132 216
let col_old = c 150 96 208
let col_recent = c 236 196 96
let col_scrubber = c 24 27 36
let col_thumb = c 94 158 240

let lerp_color a b t =
  let f x y =
    int_of_float (float_of_int x +. ((float_of_int y -. float_of_int x) *. t))
  in
  c
    (f (Color.r a) (Color.r b))
    (f (Color.g a) (Color.g b))
    (f (Color.b a) (Color.b b))

let clamp_str str n =
  if String.length str <= n then str else String.sub str 0 (n - 1) ^ "…"

let draw_txt x y sz col str = draw_text str (s x) (s y) (s sz) col
let draw_rect x y w h col = draw_rectangle (s x) (s y) (s w) (s h) col
let draw_rect_l x y w h col = draw_rectangle_lines (s x) (s y) (s w) (s h) col
let draw_circ x y r col = draw_circle (s x) (s y) (float_of_int (s r)) col
let pt x y = Vector2.create (float_of_int (s x)) (float_of_int (s y))
let line_ x1 y1 x2 y2 col = draw_line (s x1) (s y1) (s x2) (s y2) col

let triangle (x1, y1) (x2, y2) (x3, y3) col =
  draw_triangle (pt x1 y1) (pt x2 y2) (pt x3 y3) col

let panel x y w h = draw_rect x y w h col_panel

let panel2 x y w h =
  draw_rect x y w h col_panel2;
  draw_rect_l x y w h col_border

let section_hdr x y w lbl =
  draw_txt x y fxs col_dim2 (String.uppercase_ascii lbl);
  draw_rect x (y + fxs + 5) w 1 col_border

let op_name = function
  | 0x00 -> "Nop"
  | 0x01 -> "Halt"
  | 0x02 -> "Mov"
  | 0x03 -> "Load"
  | 0x04 -> "LoadF"
  | 0x05 -> "LoadB"
  | 0x06 -> "LoadNil"
  | 0x07 -> "LoadK"
  | 0x08 -> "LoadS"
  | 0x10 -> "Add"
  | 0x11 -> "Sub"
  | 0x12 -> "Mul"
  | 0x13 -> "Div"
  | 0x14 -> "Mod"
  | 0x15 -> "AddI"
  | 0x16 -> "SubI"
  | 0x17 -> "MulI"
  | 0x18 -> "AddF"
  | 0x19 -> "SubF"
  | 0x1A -> "MulF"
  | 0x1B -> "DivF"
  | 0x20 -> "And"
  | 0x21 -> "Or"
  | 0x22 -> "Xor"
  | 0x23 -> "Shl"
  | 0x24 -> "Shr"
  | 0x25 -> "ShrU"
  | 0x26 -> "ShlI"
  | 0x27 -> "ShrI"
  | 0x28 -> "ShrUI"
  | 0x30 -> "Eq"
  | 0x31 -> "Ne"
  | 0x32 -> "Lt"
  | 0x33 -> "LtU"
  | 0x34 -> "Lte"
  | 0x35 -> "LteU"
  | 0x36 -> "EqF"
  | 0x37 -> "NeF"
  | 0x38 -> "LtF"
  | 0x39 -> "LteF"
  | 0x40 -> "I2F"
  | 0x41 -> "F2I"
  | 0x42 -> "TypeOf"
  | 0x50 -> "Alloc"
  | 0x51 -> "GetField"
  | 0x52 -> "SetField"
  | 0x53 -> "GetTag"
  | 0x54 -> "Len"
  | 0x60 -> "Jmp"
  | 0x61 -> "Jz"
  | 0x62 -> "Jnz"
  | 0x70 -> "Call"
  | 0x71 -> "TailCall"
  | 0x72 -> "DynCall"
  | 0x73 -> "TailDynCall"
  | 0x74 -> "Ret"
  | 0x80 -> "Try"
  | 0x81 -> "Throw"
  | 0x82 -> "EndTry"
  | 0x90 -> "ConNew"
  | 0x91 -> "ConYield"
  | 0x92 -> "ConResume"
  | 0x93 -> "ConStatus"
  | op -> Printf.sprintf "op_%02X" op

let op_col = function
  | 0x00 | 0x01 -> col_dim
  | op when op >= 0x02 && op <= 0x08 -> col_accent
  | op when op >= 0x10 && op <= 0x1B -> col_green
  | op when op >= 0x20 && op <= 0x28 -> col_purple
  | op when op >= 0x30 && op <= 0x39 -> col_teal
  | 0x40 | 0x41 | 0x42 -> col_orange
  | op when op >= 0x50 && op <= 0x54 -> col_old
  | op when op >= 0x60 && op <= 0x62 -> c 205 110 110
  | op when op >= 0x70 && op <= 0x74 -> col_green
  | op when op >= 0x80 && op <= 0x82 -> col_red
  | op when op >= 0x90 && op <= 0x93 -> col_teal
  | _ -> col_dim

let value_str = function
  | Value.Nil -> "nil"
  | Value.Bool b -> if b then "true" else "false"
  | Value.Int n -> string_of_int n
  | Value.Float f -> Printf.sprintf "%.5g" f
  | Value.Ptr p -> Printf.sprintf "ptr(%d)" p
  | Value.NativeFun _ -> "<native-fn>"
  | Value.NativePtr _ -> "<native-ptr>"

let value_col = function
  | Value.Nil -> col_dim
  | Value.Bool _ -> col_teal
  | Value.Int _ -> col_text
  | Value.Float _ -> col_orange
  | Value.Ptr _ -> col_accent
  | Value.NativeFun _ | Value.NativePtr _ -> col_purple

let total_ticks state = Trace_model.head state.model
let lo_tick state = Trace_model.lo state.model
let reg_value_at state tick reg = Trace_model.reg_value_at state.model tick reg

let reg_last_tick state tick reg =
  Trace_model.reg_last_tick state.model tick reg

let active_regs state tick = Trace_model.active_regs state.model tick
let current_instr state tick = Trace_model.current_instr state.model tick
let writes_at_tick state tick = Trace_model.writes_at_tick state.model tick
let calls_at state tick = Trace_model.calls_at state.model tick
let rets_at state tick = Trace_model.rets_at state.model tick
let allocs_at state tick = Trace_model.allocs_at state.model tick
let frees_at state tick = Trace_model.frees_at state.model tick
let promotes_at state tick = Trace_model.promotes_at state.model tick
let gc_events_at state tick = Trace_model.gc_events_at state.model tick
let gc_minor_count state tick = Trace_model.gc_minor_count state.model tick
let gc_major_count state tick = Trace_model.gc_major_count state.model tick

let gc_promoted_total state tick =
  Trace_model.gc_promoted_total state.model tick

let gc_freed_total state tick = Trace_model.gc_freed_total state.model tick
let in_gc state tick = Trace_model.in_gc state.model tick
let gen_split state tick = Trace_model.gen_split state.model tick
let continuations_at state tick = Trace_model.continuations_at state.model tick
let call_depth state tick = Trace_model.call_depth state.model tick

let active_call_frames state tick =
  Trace_model.active_call_frames state.model tick

let rows_visible body_y body_h = (body_y, body_y + body_h)

let draw_registers state x y w h =
  panel x y w h;
  let active = active_regs state state.pb.tick in
  let sparse =
    let acc = ref [] in
    for i = 255 downto 0 do
      if Hashtbl.mem active i then acc := i :: !acc
    done;
    !acc
  in
  let n_live = List.length sparse in
  let hdr_h = fxs + 14 in
  section_hdr (x + pad) (y + pad) (w - (pad * 2)) "registers";
  let body_y = y + pad + hdr_h in
  let body_h = h - pad - hdr_h in
  let clip0, clip1 = rows_visible body_y body_h in
  if n_live = 0 then
    draw_txt (x + pad) (body_y + 4) fsm col_dim2 "(no registers written yet)"
  else begin
    let visible_start = state.reg_scroll.offset in
    let max_visible = (body_h / row_h) + 2 in
    let slice = Array.of_list sparse in
    for i = visible_start to min (n_live - 1) (visible_start + max_visible) do
      let reg = slice.(i) in
      let ry = body_y + ((i - visible_start) * row_h) in
      if ry + row_h > clip0 && ry < clip1 then begin
        let lt = reg_last_tick state state.pb.tick reg in
        let age = state.pb.tick - lt in
        let recent = lt >= 0 && age <= 6 in
        let is_hl = state.hl.reg = Some reg in
        let bg =
          if is_hl then lerp_color col_accent col_panel (1.0 -. state.hl.age)
          else if recent then
            lerp_color (c 46 40 20) col_panel (float_of_int age /. 6.0)
          else col_panel
        in
        draw_rect (x + 1) ry (w - 2) (row_h - 1) bg;
        draw_txt (x + pad) (ry + 4) fxs
          (if recent then col_recent else col_dim)
          (Printf.sprintf "r%-3d" reg);
        let v =
          Option.value (reg_value_at state state.pb.tick reg) ~default:Value.Nil
        in
        let vcol = if recent then col_recent else value_col v in
        draw_txt (x + 54) (ry + 4) fxs vcol (clamp_str (value_str v) 28)
      end
    done
  end

let draw_call_stack state x y w h =
  panel x y w h;
  draw_rect x y w 1 col_border;
  let hdr_h = fxs + 14 in
  section_hdr (x + pad) (y + pad) (w - (pad * 2)) "call stack";
  let frames = active_call_frames state state.pb.tick in
  let n_frames = List.length frames in
  let body_y = y + pad + hdr_h in
  let body_h = h - pad - hdr_h in
  let clip0, clip1 = rows_visible body_y body_h in
  if n_frames = 0 then draw_txt (x + pad) (body_y + 4) fsm col_dim2 "(empty)"
  else begin
    let visible_start = state.stack_scroll.offset in
    let max_visible = (body_h / row_h) + 2 in
    let arr = Array.of_list frames in
    for i = visible_start to min (n_frames - 1) (visible_start + max_visible) do
      let ry = body_y + ((i - visible_start) * row_h) in
      if ry + row_h > clip0 && ry < clip1 then begin
        let _, src, dst = arr.(i) in
        let is_top = i = n_frames - 1 in
        let tc = if is_top then col_accent else col_dim2 in
        let vc = if is_top then col_text else col_dim in
        draw_txt (x + pad) (ry + 4) fxs tc (Printf.sprintf "#%d" i);
        draw_txt
          (x + pad + 28)
          (ry + 4) fxs vc
          (Printf.sprintf "pc %-5d  -> r%d" src dst)
      end
    done
  end

let draw_continuations state x y w h =
  panel x y w h;
  draw_rect x y w 1 col_border;
  let hdr_h = fxs + 14 in
  section_hdr (x + pad) (y + pad) (w - (pad * 2)) "continuations";
  let cons = continuations_at state state.pb.tick in
  let n_cons = List.length cons in
  let body_y = y + pad + hdr_h in
  let body_h = h - pad - hdr_h in
  let clip0, clip1 = rows_visible body_y body_h in
  if n_cons = 0 then draw_txt (x + pad) (body_y + 4) fsm col_dim2 "(none)"
  else begin
    let visible_start = state.con_scroll.offset in
    let max_visible = (body_h / row_h) + 2 in
    let arr = Array.of_list cons in
    for i = visible_start to min (n_cons - 1) (visible_start + max_visible) do
      let ry = body_y + ((i - visible_start) * row_h) in
      if ry + row_h > clip0 && ry < clip1 then begin
        let idx, birth, status, yields, resumes = arr.(i) in
        let sc, ss =
          match status with
          | `Running -> (col_green, "running")
          | `Suspended -> (col_yellow, "suspended")
          | `New -> (col_dim, "new")
        in
        draw_circ (x + pad + 3) (ry + (row_h / 2) - 1) 3 sc;
        draw_txt
          (x + pad + 14)
          (ry + 4) fxs col_text
          (Printf.sprintf "con_%d" idx);
        draw_txt (x + pad + 60) (ry + 4) fxs sc ss;
        draw_txt
          (x + w - pad - 76)
          (ry + 4) fxs col_dim2
          (Printf.sprintf "t=%d  y%d r%d" birth yields resumes)
      end
    done
  end

let draw_heap state x y w h =
  panel x y w h;
  let iw = w - (pad * 2) in
  section_hdr (x + pad) (y + pad) iw "heap";
  let cy = ref (y + pad + fxs + 16) in
  let freed_addrs = frees_at state state.pb.tick in
  let young, old = gen_split state state.pb.tick in
  let n_young = List.length young in
  let n_old = List.length old in
  let n_freed = List.length freed_addrs in
  let raw_total = n_young + n_old + n_freed in
  let n_total = max 1 raw_total in
  let b_young = List.fold_left (fun a (_, _, sz, _) -> a + sz) 0 young in
  let b_old = List.fold_left (fun a (_, _, sz, _) -> a + sz) 0 old in
  let bar_h = 14 in
  let w_young = iw * n_young / n_total in
  let w_old = iw * n_old / n_total in
  let w_freed = iw - w_young - w_old in
  draw_rect (x + pad) !cy w_young bar_h col_young;
  draw_rect (x + pad + w_young) !cy w_old bar_h col_old;
  draw_rect (x + pad + w_young + w_old) !cy w_freed bar_h col_dim2;
  cy := !cy + bar_h + 10;
  let legend =
    [|
      (col_young, Printf.sprintf "young %d · %dw" n_young b_young);
      (col_old, Printf.sprintf "old %d · %dw" n_old b_old);
      (col_dim2, Printf.sprintf "freed %d" n_freed);
    |]
  in
  Array.iteri
    (fun i (col, txt) ->
      let lx = x + pad + (i * (iw / 3)) in
      draw_circ (lx + 3) (!cy + (fxs / 2)) 3 col;
      draw_txt (lx + 12) !cy fxs col_dim txt)
    legend;
  cy := !cy + fxs + 14;
  let frag =
    if raw_total = 0 then "n/a"
    else
      Printf.sprintf "%.0f%%"
        (100.0 *. float_of_int n_freed /. float_of_int raw_total)
  in
  draw_txt (x + pad) !cy fxs col_dim2
    (Printf.sprintf "promoted %d  ·  swept %d  ·  frag %s"
       (List.length (promotes_at state state.pb.tick))
       (gc_freed_total state state.pb.tick)
       frag);
  cy := !cy + fxs + 14;
  if in_gc state state.pb.tick then begin
    draw_circ (x + pad + 3) (!cy + (fxs / 2)) 3 col_red;
    draw_txt (x + pad + 12) !cy fxs col_red "gc running"
  end
  else draw_txt (x + pad) !cy fxs col_dim2 "gc idle";
  draw_txt
    (x + iw - pad - 150)
    !cy fxs col_dim2
    (Printf.sprintf "minor %d  ·  major %d"
       (gc_minor_count state state.pb.tick)
       (gc_major_count state state.pb.tick));
  cy := !cy + fxs + 16;
  draw_rect (x + pad) !cy iw 1 col_border;
  cy := !cy + 14;
  let cell = 10 and gap = 2 in
  let cols_n = max 1 (iw / (cell + gap)) in
  let freed_set = Hashtbl.create 16 in
  List.iter (fun a -> Hashtbl.replace freed_set a ()) freed_addrs;
  let promoted_set = Hashtbl.create 16 in
  List.iter
    (fun a -> Hashtbl.replace promoted_set a ())
    (promotes_at state state.pb.tick);
  let draw_cells cells max_rows =
    List.iteri
      (fun i (_, addr, size, _) ->
        let row = i / cols_n and col2 = i mod cols_n in
        if row < max_rows then begin
          let cx = x + pad + (col2 * (cell + gap)) in
          let cy2 = !cy + (row * (cell + gap)) in
          let is_freed = Hashtbl.mem freed_set addr in
          let is_old = Hashtbl.mem promoted_set addr && not is_freed in
          let bc =
            if is_freed then c 26 28 36
            else if is_old then col_old
            else col_young
          in
          draw_rect cx cy2 cell cell bc;
          if size > 1 && not is_freed then
            draw_rect (cx + 3) (cy2 + 3) (cell - 6) (cell - 6)
              (lerp_color bc col_bg 0.35)
        end)
      cells
  in
  let rows_y = min 3 (max 1 ((n_young + cols_n - 1) / cols_n)) in
  let rows_o = min 3 (max 1 ((n_old + cols_n - 1) / cols_n)) in
  draw_cells young rows_y;
  cy := !cy + (rows_y * (cell + gap)) + 4;
  draw_cells old rows_o

let draw_inspector state x y w h =
  if not state.insp.active then ()
  else begin
    panel2 x y w h;
    let cy = ref (y + pad) in
    section_hdr (x + pad) !cy (w - (pad * 2)) "inspector";
    cy := !cy + fxs + 14;
    let addr = state.insp.addr in
    draw_txt (x + pad) !cy fxs col_dim2 (Printf.sprintf "ptr %d" addr);
    cy := !cy + fxs + 12;
    let all = allocs_at state state.pb.tick in
    let freed = frees_at state state.pb.tick in
    let proms = promotes_at state state.pb.tick in
    match List.find_opt (fun (_, a, _, _) -> a = addr) all with
    | None -> draw_txt (x + pad) !cy fsm col_dim2 "(not yet allocated)"
    | Some (_, _, size, tag) ->
        let is_freed = List.mem addr freed in
        let is_old = List.mem addr proms in
        let kv lbl v vc =
          draw_txt (x + pad) !cy fxs col_dim lbl;
          draw_txt (x + pad + 64) !cy fxs vc v;
          cy := !cy + fxs + 5
        in
        kv "tag" (string_of_int tag) col_yellow;
        kv "size" (Printf.sprintf "%d words" size) col_text;
        kv "gen"
          (if is_old then "old" else "young")
          (if is_old then col_old else col_young);
        kv "freed"
          (if is_freed then "yes" else "no")
          (if is_freed then col_red else col_green);
        cy := !cy + 4;
        draw_rect (x + pad) !cy (w - (pad * 2)) 1 col_border;
        cy := !cy + 6;
        section_hdr (x + pad) !cy (w - (pad * 2)) "FIELDS";
        cy := !cy + fxs + 8;
        for field = 0 to size - 1 do
          let last_v =
            Trace_model.last_write state.model state.pb.tick addr field
          in
          draw_txt (x + pad) !cy fxs col_dim (Printf.sprintf "[%d]" field);
          draw_txt (x + pad + 36) !cy fxs (value_col last_v) (value_str last_v);
          cy := !cy + fxs + 4
        done
  end

let draw_event_track state x y w =
  draw_rect x y w event_h col_bg;
  draw_rect x (y + event_h - 1) w 1 col_border;
  let total = float_of_int (max 1 (total_ticks state)) in
  let tx t = x + int_of_float (float_of_int t /. total *. float_of_int w) in
  (match state.live with
  | Some _ ->
      let lot = lo_tick state in
      if lot > 0 then begin
        draw_rect x y (max 1 (tx lot - x)) event_h (c 12 13 17);
        List.iter
          (fun (b : Trace_model.bucket) ->
            let bx = tx b.lo in
            let bw = max 1 (tx b.hi - bx) in
            draw_rect bx (y + event_h - 3) bw 2 col_dim2)
          (Trace_model.bucket_list state.model);
        draw_rect (tx lot) y 1 event_h (ca 150 80 50 255)
      end
  | None -> ());
  Trace_model.iter state.model (fun (ev : Ev.t) ->
      let t = ev.seq in
      match ev.kind with
      | Ev.Gc { event } ->
          let ec, eh =
            match event with
            | Ev.Minor_start -> (col_young, event_h / 2)
            | Ev.Minor_end _ -> (col_accent, event_h / 2)
            | Ev.Major_mark _ -> (col_old, event_h)
            | Ev.Major_sweep _ -> (col_purple, event_h)
            | Ev.Major_end -> (col_red, event_h)
          in
          draw_rect (tx t) (y + event_h - eh) 1 eh ec
      | Ev.Call _ -> draw_rect (tx t) y 1 4 (ca 96 199 128 170)
      | Ev.Con_yield _ -> draw_rect (tx t) y 1 4 (ca 96 186 180 170)
      | _ -> ());
  draw_rect (tx state.pb.tick) y 1 event_h col_recent

let draw_instr_bar state x y w =
  panel x y w instr_h;
  let mid = y + (instr_h / 2) in
  match current_instr state state.pb.tick with
  | None -> draw_txt (x + pad) (mid - (fsm / 2)) fsm col_dim2 "(no instruction)"
  | Some (tick, pc, op) ->
      let cat_col = op_col op in
      draw_rect (x + pad) (mid - 11) 3 22 cat_col;
      draw_txt (x + pad + 12) (mid - (flg / 2)) flg cat_col (op_name op);
      draw_txt
        (x + pad + 118)
        (mid - (fxs / 2))
        fxs col_dim
        (Printf.sprintf "pc %d" pc);
      let writes = writes_at_tick state tick in
      let wx = ref (x + pad + 220) in
      List.iter
        (fun (_, r, v) ->
          if !wx + 110 < x + w - pad then begin
            let y_pos = mid - (fxs / 2) in
            draw_txt !wx y_pos fxs col_dim (Printf.sprintf "r%d" r);
            draw_txt (!wx + 22) y_pos fxs col_dim2 "<-";
            draw_txt (!wx + 40) y_pos fxs (value_col v)
              (clamp_str (value_str v) 10);
            wx := !wx + 120
          end)
        writes

let draw_scrubber state x y w =
  panel x y w ctrl_h;
  let total = float_of_int (max 1 (total_ticks state)) in
  let t = float_of_int state.pb.tick /. total in
  let cx = x + pad and midy = y + (ctrl_h / 2) in
  draw_rect cx (midy - 7) 2 14 col_dim;
  triangle (cx + 4, midy - 7) (cx + 4, midy + 7) (cx + 13, midy) col_dim;
  triangle (cx + 22, midy - 7) (cx + 22, midy + 7) (cx + 31, midy) col_dim;
  if state.pb.playing then begin
    draw_rect (cx + 42) (midy - 7) 3 14 col_accent;
    draw_rect (cx + 49) (midy - 7) 3 14 col_accent
  end
  else
    triangle (cx + 42, midy - 7) (cx + 42, midy + 7) (cx + 54, midy) col_accent;
  triangle (cx + 64, midy - 7) (cx + 64, midy + 7) (cx + 73, midy) col_dim;
  let live = state.live <> None in
  if live then begin
    let lc = if state.follow then col_green else col_orange in
    draw_circ (cx + 86) midy 3 lc;
    draw_txt (cx + 94) (midy - (fxs / 2)) fxs lc "LIVE"
  end
  else begin
    triangle (cx + 80, midy - 7) (cx + 80, midy + 7) (cx + 89, midy) col_dim;
    draw_rect (cx + 91) (midy - 7) 2 14 col_dim
  end;
  let btn_w = 130 and spd_w = 64 in
  let track_x = x + pad + btn_w in
  let track_w = w - (pad * 2) - btn_w - spd_w in
  let track_y = midy - 2 in
  draw_rect track_x track_y track_w 4 col_scrubber;
  (match state.live with
  | Some _ when total_ticks state > 0 ->
      let lof = float_of_int (lo_tick state) /. total in
      draw_rect track_x track_y
        (int_of_float (lof *. float_of_int track_w))
        4 (c 46 32 28)
  | _ -> ());
  let fw = int_of_float (t *. float_of_int track_w) in
  draw_rect track_x track_y fw 4 col_accent;
  draw_circ (track_x + fw) midy 5 col_thumb;
  draw_txt
    (x + w - spd_w + pad)
    (midy - (fxs / 2))
    fxs col_dim2
    (Printf.sprintf "%.1fx" state.pb.speed)

let draw_topbar state =
  draw_rect 0 0 sw bar_h col_panel;
  draw_rect 0 (bar_h - 1) sw 1 col_border;
  let mid_y = bar_h / 2 in
  draw_txt pad (mid_y - (flg / 2)) flg col_text "map.viz";
  let mode_lbl =
    match state.live with Some _ -> "REAL-TIME" | None -> "POST-DUMP"
  in
  let mode_col =
    match state.live with Some _ -> col_green | None -> col_dim2
  in
  let mx = pad + 78 in
  draw_txt mx (mid_y - (fxs / 2)) fxs mode_col mode_lbl;
  let tx = mx + 78 in
  draw_txt tx
    (mid_y - (fxs / 2))
    fxs col_dim
    (Printf.sprintf "tick %d / %d" state.pb.tick (total_ticks state));
  let gx = tx + 150 in
  draw_circ (gx + 3) mid_y 3
    (if in_gc state state.pb.tick then col_red else col_green);
  draw_txt (gx + 12)
    (mid_y - (fxs / 2))
    fxs col_dim2
    (if in_gc state state.pb.tick then "gc" else "idle");
  (match state.live with
  | Some h ->
      let lx = gx + 60 in
      let st =
        if h.is_done () then "halted"
        else if state.pb.playing then "running"
        else "paused"
      in
      draw_circ (lx + 3) mid_y 3
        (if state.follow then col_green else col_orange);
      draw_txt (lx + 12)
        (mid_y - (fxs / 2))
        fxs
        (if state.follow then col_green else col_orange)
        (Printf.sprintf "%s · %s" st
           (if state.follow then "live" else "free (F)"))
  | None ->
      let lx = gx + 60 in
      if state.pb.playing then
        draw_txt lx (mid_y - (fxs / 2)) fxs col_dim "playing"
      else draw_txt lx (mid_y - (fxs / 2)) fxs col_dim2 "paused");
  let bw = 52 and bg = 8 in
  let modes = [| ("Regs", Registers); ("Heap", Heap); ("Both", Both) |] in
  let bx0 = sw - pad - (3 * bw) - (2 * bg) in
  Array.iteri
    (fun i (lbl, mode) ->
      let bx = bx0 + (i * (bw + bg)) in
      let active = state.mode = mode in
      draw_txt
        (bx + ((bw - (fxs * String.length lbl)) / 2))
        (mid_y - (fxs / 2))
        fxs
        (if active then col_accent else col_dim2)
        lbl;
      if active then draw_rect (bx + 10) (mid_y + 8) (bw - 20) 2 col_accent)
    modes

let content_y () = bar_h
let content_h () = sh - bar_h - instr_h - event_h - ctrl_h
let instr_y () = content_y () + content_h ()
let event_y () = instr_y () + instr_h
let scrub_y () = event_y () + event_h
let reg_panel_w = 280
let stack_h = 140
let con_h = 120
let insp_w = 210

let handle_input state =
  let dt = get_frame_time () in
  (match state.live with
  | None ->
      if state.pb.playing then begin
        state.pb.accum <- state.pb.accum +. (dt *. state.pb.speed);
        while state.pb.accum >= 1.0 do
          if state.pb.tick < total_ticks state - 1 then
            state.pb.tick <- state.pb.tick + 1
          else state.pb.playing <- false;
          state.pb.accum <- state.pb.accum -. 1.0
        done
      end
  | Some _ -> ());
  if state.hl.age > 0.0 then
    state.hl.age <- max 0.0 (state.hl.age -. (dt *. 2.0));
  if is_key_pressed Key.Space then begin
    state.pb.playing <- not state.pb.playing;
    if state.pb.playing then state.follow <- true
  end;
  if state.live <> None && is_key_pressed Key.F then begin
    state.follow <- true;
    state.pb.playing <- true;
    state.pb.tick <- total_ticks state
  end;
  (match state.live with
  | Some h ->
      if is_key_pressed Key.Right && not (h.is_done ()) then begin
        (try h.step () with _ -> ());
        state.follow <- true
      end;
      if is_key_pressed Key.Left then begin
        state.follow <- false;
        if state.pb.tick > lo_tick state then state.pb.tick <- state.pb.tick - 1
      end;
      if is_key_pressed Key.Home then begin
        state.follow <- false;
        state.pb.tick <- lo_tick state
      end;
      if is_key_pressed Key.End then begin
        state.follow <- true;
        state.pb.tick <- total_ticks state
      end
  | None ->
      if is_key_pressed Key.Right && state.pb.tick < total_ticks state - 1 then
        state.pb.tick <- state.pb.tick + 1;
      if is_key_pressed Key.Left && state.pb.tick > 0 then
        state.pb.tick <- state.pb.tick - 1;
      if is_key_pressed Key.Home then state.pb.tick <- 0;
      if is_key_pressed Key.End then state.pb.tick <- total_ticks state - 1);
  if is_key_pressed Key.Tab then
    state.mode <-
      (match state.mode with
      | Registers -> Heap
      | Heap -> Both
      | Both -> Registers);
  if is_key_pressed Key.Escape then state.insp.active <- false;
  let mx = int_of_float (float_of_int (get_mouse_x ()) /. scale) in
  let my = int_of_float (float_of_int (get_mouse_y ()) /. scale) in
  let wheel = get_mouse_wheel_move () in
  let cy = content_y () and ch = content_h () in
  let reg_h = ch - stack_h - con_h in
  let reg_right =
    match state.mode with Registers -> sw | Both -> reg_panel_w | Heap -> 0
  in
  let in_reg_body = mx >= 0 && mx < reg_right && my >= cy && my < cy + reg_h in
  let in_stack_body =
    mx >= 0 && mx < reg_right && my >= cy + reg_h && my < cy + reg_h + stack_h
  in
  let in_con_body =
    mx >= 0 && mx < reg_right && my >= cy + reg_h + stack_h && my < cy + ch
  in
  let in_heap =
    match state.mode with
    | Heap -> mx >= 0 && mx < sw && my >= cy && my < cy + ch
    | Both -> mx >= reg_panel_w && mx < sw && my >= cy && my < cy + ch
    | _ -> false
  in
  if wheel <> 0.0 then begin
    let d = -int_of_float wheel in
    if in_reg_body then
      state.reg_scroll.offset <- max 0 (state.reg_scroll.offset + d)
    else if in_stack_body then
      state.stack_scroll.offset <- max 0 (state.stack_scroll.offset + d)
    else if in_con_body then
      state.con_scroll.offset <- max 0 (state.con_scroll.offset + d)
    else if in_heap then
      state.heap_scroll.offset <- max 0 (state.heap_scroll.offset + d)
    else state.pb.speed <- max 0.1 (min 4.0 (state.pb.speed +. (wheel *. 0.1)))
  end;
  let sy = scrub_y () in
  let btn_w = 130 and spd_w = 64 in
  let track_x = pad + btn_w in
  let track_w = sw - (pad * 2) - btn_w - spd_w in
  if
    is_mouse_button_down MouseButton.Left
    && my >= sy
    && my <= sy + ctrl_h
    && mx >= track_x
    && mx <= track_x + track_w
  then begin
    let t = float_of_int (mx - track_x) /. float_of_int track_w in
    let tmax = total_ticks state in
    let tmin = match state.live with Some _ -> lo_tick state | None -> 0 in
    state.pb.tick <-
      max tmin
        (min
           (max tmin (tmax - 1))
           (int_of_float (t *. float_of_int (max 1 tmax))));
    state.pb.playing <- false;
    state.follow <- false
  end;
  let midy = sy + (ctrl_h / 2) in
  if
    is_mouse_button_pressed MouseButton.Left
    && my >= midy - 12
    && my <= midy + 12
  then begin
    let live = match state.live with Some h -> Some h | None -> None in
    if mx >= pad && mx < pad + 18 then begin
      state.follow <- false;
      state.pb.tick <- (match live with Some _ -> lo_tick state | None -> 0)
    end;
    if mx >= pad + 20 && mx < pad + 38 then begin
      state.follow <- false;
      let tmin = match live with Some _ -> lo_tick state | None -> 0 in
      if state.pb.tick > tmin then state.pb.tick <- state.pb.tick - 1
    end;
    if mx >= pad + 40 && mx < pad + 62 then begin
      state.pb.playing <- not state.pb.playing;
      if state.pb.playing then state.follow <- true
    end;
    (if mx >= pad + 62 && mx < pad + 80 then
       match live with
       | Some h ->
           if not (h.is_done ()) then begin
             (try h.step () with _ -> ());
             state.follow <- true
           end
       | None ->
           if state.pb.tick < total_ticks state - 1 then
             state.pb.tick <- state.pb.tick + 1);
    if mx >= pad + 80 && mx < pad + 130 then begin
      state.follow <- true;
      state.pb.playing <- live <> None;
      state.pb.tick <- total_ticks state
    end
  end;
  let btn_w2 = 52 and btn_gap = 8 in
  let bx0 = sw - pad - (3 * btn_w2) - (2 * btn_gap) in
  if is_mouse_button_pressed MouseButton.Left && my >= 0 && my <= bar_h then begin
    if mx >= bx0 && mx < bx0 + btn_w2 then state.mode <- Registers;
    if mx >= bx0 + btn_w2 + btn_gap && mx < bx0 + (2 * btn_w2) + btn_gap then
      state.mode <- Heap;
    if
      mx >= bx0 + (2 * (btn_w2 + btn_gap))
      && mx < bx0 + (3 * btn_w2) + (2 * btn_gap)
    then state.mode <- Both
  end;
  if is_mouse_button_pressed MouseButton.Right && in_reg_body then begin
    let active = active_regs state state.pb.tick in
    let sparse =
      let acc = ref [] in
      for i = 255 downto 0 do
        if Hashtbl.mem active i then acc := i :: !acc
      done;
      !acc
    in
    let hdr_h = fxs + 14 in
    let body_y = cy + pad + hdr_h in
    let row = (my - body_y) / row_h in
    let idx = state.reg_scroll.offset + row in
    if idx >= 0 && idx < List.length sparse then begin
      let reg = List.nth sparse idx in
      match reg_value_at state state.pb.tick reg with
      | Some (Value.Ptr addr) ->
          state.insp.active <- true;
          state.insp.addr <- addr
      | _ -> state.insp.active <- false
    end
  end;
  let ey = event_y () in
  if is_mouse_button_pressed MouseButton.Left && my >= ey && my < ey + event_h
  then begin
    let t = float_of_int mx /. float_of_int sw in
    let tmax = total_ticks state in
    let tmin = match state.live with Some _ -> lo_tick state | None -> 0 in
    state.pb.tick <-
      max tmin
        (min
           (max tmin (tmax - 1))
           (int_of_float (t *. float_of_int (max 1 tmax))));
    state.follow <- false
  end

let draw state =
  begin_drawing ();
  clear_background col_bg;
  draw_topbar state;
  let cy = content_y () in
  let ch = content_h () in
  let iy = instr_y () in
  let ey = event_y () in
  let sby = scrub_y () in
  let reg_h = ch - stack_h - con_h in
  (match state.mode with
  | Registers ->
      let rw = if state.insp.active then sw - insp_w else sw in
      draw_registers state 0 cy rw reg_h;
      draw_call_stack state 0 (cy + reg_h) rw stack_h;
      draw_continuations state 0 (cy + reg_h + stack_h) rw con_h;
      if state.insp.active then draw_inspector state rw cy insp_w ch
  | Heap ->
      let hw = if state.insp.active then sw - insp_w else sw in
      draw_heap state 0 cy hw ch;
      if state.insp.active then draw_inspector state hw cy insp_w ch
  | Both ->
      let heap_x = reg_panel_w in
      let heap_w =
        if state.insp.active then sw - reg_panel_w - insp_w
        else sw - reg_panel_w
      in
      draw_rect reg_panel_w cy 1 ch col_border;
      draw_registers state 0 cy reg_panel_w reg_h;
      draw_call_stack state 0 (cy + reg_h) reg_panel_w stack_h;
      draw_continuations state 0 (cy + reg_h + stack_h) reg_panel_w con_h;
      draw_heap state heap_x cy heap_w ch;
      if state.insp.active then
        draw_inspector state (heap_x + heap_w) cy insp_w ch);
  draw_instr_bar state 0 iy sw;
  draw_event_track state 0 ey sw;
  draw_scrubber state 0 sby sw;
  end_drawing ()

let make_state ?live model =
  {
    model;
    pb =
      {
        tick = 0;
        playing = (match live with Some _ -> true | None -> false);
        speed = 1.0;
        accum = 0.0;
      };
    hl = { reg = None; addr = None; age = 0.0 };
    insp = { active = false; addr = 0 };
    reg_scroll = { offset = 0 };
    stack_scroll = { offset = 0 };
    heap_scroll = { offset = 0 };
    con_scroll = { offset = 0 };
    mode = Both;
    live;
    follow = true;
  }

let run_loop state =
  init_window screen_w screen_h "Map.Viz";
  set_target_fps 60;
  while not (window_should_close ()) do
    (match state.live with
    | Some h -> (
        if state.pb.playing && not (h.is_done ()) then
          try
            let n = max 1 (int_of_float (state.pb.speed *. 64.0)) in
            for _ = 1 to n do
              h.step ()
            done
          with _ -> state.pb.playing <- false)
    | None -> ());
    handle_input state;
    (match state.live with
    | Some _ when state.follow -> state.pb.tick <- total_ticks state
    | _ -> ());
    draw state
  done;
  close_window ()

let run_file path = run_loop (make_state (Trace_model.of_file path))

let run_live model ~step ~is_done ~status =
  run_loop (make_state ~live:{ step; is_done; status } model)

let run = run_file
