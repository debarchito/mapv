open Mapv.Core
open Mapv.Namespace

module Make (H : Mapv.Heap.S) = struct
  module Fs = Fold_string.Make (H)
  module NS = Mapv.Namespace.Make (H)

  let vec2_id : Raylib.Vector2.t Type.Id.t = Type.Id.make ()
  let cam2d_id : Raylib.Camera2D.t Type.Id.t = Type.Id.make ()
  let color_id : Raylib.Color.t Type.Id.t = Type.Id.make ()

  let make_vec2 x y =
    Value.make_native ~tag:vec2_id ~finalizer:None (Raylib.Vector2.create x y)

  let expect_vec2 name v =
    match Value.get_native vec2_id v with
    | Some v2 -> v2
    | None ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf "std/raylib/%s: expected Vector2" name)))

  let make_color r g b a =
    Value.make_native ~tag:color_id ~finalizer:None
      (Raylib.Color.create r g b a)

  let expect_color name v =
    match Value.get_native color_id v with
    | Some c -> c
    | None ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf "std/raylib/%s: expected Color" name)))

  let make_cam2d c = Value.make_native ~tag:cam2d_id ~finalizer:None c

  let expect_cam2d name v =
    match Value.get_native cam2d_id v with
    | Some c -> c
    | None ->
        raise
          (Exception.Signal
             (Exception.Type_error
                (Printf.sprintf "std/raylib/%s: expected Camera2D" name)))

  let fof = float_of_int

  let register heap (reg : Mapv.Symbol.registry) =
    let nif_checked, nif_, type_err = NS.ns_builder () in
    let estr = Fs.expect_str heap in
    NS.register heap reg
      (ns "std/raylib"
         [
           nif_checked "init_window" 3 (function
             | [| Value.Int w; Value.Int h; s |] ->
                 Raylib.init_window w h (estr s);
                 Raylib.set_target_fps 60;
                 Value.Nil
             | _ -> type_err "init_window" "Int, Int, Ptr");
           nif_ "close_window" (function _ ->
               Raylib.close_window ();
               Value.Nil);
           nif_ "window_should_close" (function _ ->
               Value.Int (if Raylib.window_should_close () then 1 else 0));
           nif_ "poll_input_events" (function _ ->
               Raylib.poll_input_events ();
               Value.Nil);
           nif_ "get_screen_width" (function _ ->
               Value.Int (Raylib.get_screen_width ()));
           nif_ "get_screen_height" (function _ ->
               Value.Int (Raylib.get_screen_height ()));
           nif_ "begin_drawing" (function _ ->
               Raylib.begin_drawing ();
               Value.Nil);
           nif_ "end_drawing" (function _ ->
               Raylib.end_drawing ();
               Value.Nil);
           nif_checked "clear_background" 1 (function
             | [| c |] ->
                 Raylib.clear_background (expect_color "clear_background" c);
                 Value.Nil
             | _ -> type_err "clear_background" "Color");
           nif_checked "begin_mode_2d" 1 (function
             | [| c |] ->
                 Raylib.begin_mode_2d (expect_cam2d "begin_mode_2d" c);
                 Value.Nil
             | _ -> type_err "begin_mode_2d" "Camera2D");
           nif_checked "end_mode_2d" 0 (function
             | [||] ->
                 Raylib.end_mode_2d ();
                 Value.Nil
             | _ -> type_err "end_mode_2d" "");
           nif_checked "set_target_fps" 1 (function
             | [| Value.Int fps |] ->
                 Raylib.set_target_fps fps;
                 Value.Nil
             | _ -> type_err "set_target_fps" "Int");
           nif_checked "get_frame_time" 0 (function
             | [||] -> Value.Float (Raylib.get_frame_time ())
             | _ -> type_err "get_frame_time" "");
           nif_checked "get_fps" 0 (function
             | [||] -> Value.Int (Raylib.get_fps ())
             | _ -> type_err "get_fps" "");
           nif_checked "get_time" 0 (function
             | [||] -> Value.Float (Raylib.get_time ())
             | _ -> type_err "get_time" "");
           nif_checked "draw_text" 5 (function
             | [| s; Value.Int x; Value.Int y; Value.Int size; c |] ->
                 Raylib.draw_text (estr s) x y size (expect_color "draw_text" c);
                 Value.Nil
             | _ -> type_err "draw_text" "Ptr, Int, Int, Int, Color");
           nif_checked "draw_fps" 2 (function
             | [| Value.Int x; Value.Int y |] ->
                 Raylib.draw_fps x y;
                 Value.Nil
             | _ -> type_err "draw_fps" "Int, Int");
           ns "color"
             [
               nif_checked "make" 4 (function
                 | [| Value.Int r; Value.Int g; Value.Int b; Value.Int a |] ->
                     make_color r g b a
                 | _ -> type_err "color/make" "Int, Int, Int, Int");
               nif_checked "r" 1 (function
                 | [| c |] ->
                     Value.Int (Raylib.Color.r (expect_color "color/r" c))
                 | _ -> type_err "color/r" "Color");
               nif_checked "g" 1 (function
                 | [| c |] ->
                     Value.Int (Raylib.Color.g (expect_color "color/g" c))
                 | _ -> type_err "color/g" "Color");
               nif_checked "b" 1 (function
                 | [| c |] ->
                     Value.Int (Raylib.Color.b (expect_color "color/b" c))
                 | _ -> type_err "color/b" "Color");
               nif_checked "a" 1 (function
                 | [| c |] ->
                     Value.Int (Raylib.Color.a (expect_color "color/a" c))
                 | _ -> type_err "color/a" "Color");
             ];
           ns "vec2"
             [
               nif_checked "make" 2 (function
                 | [| Value.Float x; Value.Float y |] -> make_vec2 x y
                 | [| Value.Int x; Value.Int y |] -> make_vec2 (fof x) (fof y)
                 | _ -> type_err "vec2/make" "Float|Int, Float|Int");
               nif_checked "x" 1 (function
                 | [| v |] ->
                     Value.Float (Raylib.Vector2.x (expect_vec2 "vec2/x" v))
                 | _ -> type_err "vec2/x" "Vector2");
               nif_checked "y" 1 (function
                 | [| v |] ->
                     Value.Float (Raylib.Vector2.y (expect_vec2 "vec2/y" v))
                 | _ -> type_err "vec2/y" "Vector2");
               nif_checked "add" 2 (function
                 | [| a; b |] ->
                     let a = expect_vec2 "vec2/add" a in
                     let b = expect_vec2 "vec2/add" b in
                     make_vec2
                       (Raylib.Vector2.x a +. Raylib.Vector2.x b)
                       (Raylib.Vector2.y a +. Raylib.Vector2.y b)
                 | _ -> type_err "vec2/add" "Vector2, Vector2");
               nif_checked "scale" 2 (function
                 | [| v; Value.Float s |] ->
                     let v = expect_vec2 "vec2/scale" v in
                     make_vec2
                       (Raylib.Vector2.x v *. s)
                       (Raylib.Vector2.y v *. s)
                 | [| v; Value.Int s |] ->
                     let v = expect_vec2 "vec2/scale" v in
                     let s = fof s in
                     make_vec2
                       (Raylib.Vector2.x v *. s)
                       (Raylib.Vector2.y v *. s)
                 | _ -> type_err "vec2/scale" "Vector2, Float|Int");
               nif_checked "length" 1 (function
                 | [| v |] ->
                     let v = expect_vec2 "vec2/length" v in
                     let x = Raylib.Vector2.x v and y = Raylib.Vector2.y v in
                     Value.Float (sqrt ((x *. x) +. (y *. y)))
                 | _ -> type_err "vec2/length" "Vector2");
             ];
           ns "cam2d"
             [
               nif_checked "make" 6 (function
                 | [| ox; oy; tx; ty; Value.Float rot; Value.Float zoom |] ->
                     let to_f = function
                       | Value.Float f -> f
                       | Value.Int n -> fof n
                       | v ->
                           raise
                             (Exception.Signal
                                (Exception.Type_error
                                   (Printf.sprintf
                                      "std/raylib/cam2d/make: expected \
                                       Float|Int but got %s"
                                      (Value.to_string v))))
                     in
                     make_cam2d
                       (Raylib.Camera2D.create
                          (Raylib.Vector2.create (to_f ox) (to_f oy))
                          (Raylib.Vector2.create (to_f tx) (to_f ty))
                          rot zoom)
                 | _ -> type_err "cam2d/make" "Float|Int * 4, Float, Float");
               nif_checked "set_target" 3 (function
                 | [| c; Value.Float x; Value.Float y |] ->
                     let cam = expect_cam2d "cam2d/set_target" c in
                     Raylib.Camera2D.set_target cam (Raylib.Vector2.create x y);
                     make_cam2d cam
                 | _ -> type_err "cam2d/set_target" "Camera2D, Float, Float");
               nif_checked "set_zoom" 2 (function
                 | [| c; Value.Float z |] ->
                     let cam = expect_cam2d "cam2d/set_zoom" c in
                     Raylib.Camera2D.set_zoom cam z;
                     make_cam2d cam
                 | _ -> type_err "cam2d/set_zoom" "Camera2D, Float");
             ];
           ns "shapes"
             [
               nif_checked "draw_circle" 4 (function
                 | [| Value.Int x; Value.Int y; Value.Float r; c |] ->
                     Raylib.draw_circle x y r
                       (expect_color "shapes/draw_circle" c);
                     Value.Nil
                 | _ -> type_err "shapes/draw_circle" "Int, Int, Float, Color");
               nif_checked "draw_circle_lines" 4 (function
                 | [| Value.Int x; Value.Int y; Value.Float r; c |] ->
                     Raylib.draw_circle_lines x y r
                       (expect_color "shapes/draw_circle_lines" c);
                     Value.Nil
                 | _ ->
                     type_err "shapes/draw_circle_lines"
                       "Int, Int, Float, Color");
               nif_checked "draw_rect" 5 (function
                 | [| Value.Int x; Value.Int y; Value.Int w; Value.Int h; c |]
                   ->
                     Raylib.draw_rectangle x y w h
                       (expect_color "shapes/draw_rect" c);
                     Value.Nil
                 | _ -> type_err "shapes/draw_rect" "Int, Int, Int, Int, Color");
               nif_checked "draw_rect_lines" 5 (function
                 | [| Value.Int x; Value.Int y; Value.Int w; Value.Int h; c |]
                   ->
                     Raylib.draw_rectangle_lines x y w h
                       (expect_color "shapes/draw_rect_lines" c);
                     Value.Nil
                 | _ ->
                     type_err "shapes/draw_rect_lines"
                       "Int, Int, Int, Int, Color");
               nif_checked "draw_line" 5 (function
                 | [|
                     Value.Int x1; Value.Int y1; Value.Int x2; Value.Int y2; c;
                   |] ->
                     Raylib.draw_line x1 y1 x2 y2
                       (expect_color "shapes/draw_line" c);
                     Value.Nil
                 | _ -> type_err "shapes/draw_line" "Int, Int, Int, Int, Color");
               nif_checked "draw_line_v" 3 (function
                 | [| a; b; c |] ->
                     Raylib.draw_line_v
                       (expect_vec2 "shapes/draw_line_v" a)
                       (expect_vec2 "shapes/draw_line_v" b)
                       (expect_color "shapes/draw_line_v" c);
                     Value.Nil
                 | _ -> type_err "shapes/draw_line_v" "Vector2, Vector2, Color");
               nif_checked "draw_triangle" 4 (function
                 | [| v1; v2; v3; c |] ->
                     Raylib.draw_triangle
                       (expect_vec2 "shapes/draw_triangle" v1)
                       (expect_vec2 "shapes/draw_triangle" v2)
                       (expect_vec2 "shapes/draw_triangle" v3)
                       (expect_color "shapes/draw_triangle" c);
                     Value.Nil
                 | _ ->
                     type_err "shapes/draw_triangle"
                       "Vector2, Vector2, Vector2, Color");
               nif_checked "draw_poly" 5 (function
                 | [|
                     center;
                     Value.Int sides;
                     Value.Float radius;
                     Value.Float rotation;
                     c;
                   |] ->
                     Raylib.draw_poly
                       (expect_vec2 "shapes/draw_poly" center)
                       sides radius rotation
                       (expect_color "shapes/draw_poly" c);
                     Value.Nil
                 | _ ->
                     type_err "shapes/draw_poly"
                       "Vector2, Int, Float, Float, Color");
             ];
           ns "input"
             [
               nif_checked "is_key_down" 1 (function
                 | [| Value.Int k |] ->
                     Value.Int
                       (if Raylib.is_key_down (Raylib.Key.of_int k) then 1
                        else 0)
                 | _ -> type_err "input/is_key_down" "Int");
               nif_checked "is_key_pressed" 1 (function
                 | [| Value.Int k |] ->
                     Value.Int
                       (if Raylib.is_key_pressed (Raylib.Key.of_int k) then 1
                        else 0)
                 | _ -> type_err "input/is_key_pressed" "Int");
               nif_checked "is_key_released" 1 (function
                 | [| Value.Int k |] ->
                     Value.Int
                       (if Raylib.is_key_released (Raylib.Key.of_int k) then 1
                        else 0)
                 | _ -> type_err "input/is_key_released" "Int");
               nif_checked "mouse_x" 0 (function
                 | [||] -> Value.Int (Raylib.get_mouse_x ())
                 | _ -> type_err "input/mouse_x" "");
               nif_checked "mouse_y" 0 (function
                 | [||] -> Value.Int (Raylib.get_mouse_y ())
                 | _ -> type_err "input/mouse_y" "");
               nif_checked "mouse_pos" 0 (function
                 | [||] ->
                     let p = Raylib.get_mouse_position () in
                     make_vec2 (Raylib.Vector2.x p) (Raylib.Vector2.y p)
                 | _ -> type_err "input/mouse_pos" "");
               nif_checked "mouse_delta" 0 (function
                 | [||] ->
                     let d = Raylib.get_mouse_delta () in
                     make_vec2 (Raylib.Vector2.x d) (Raylib.Vector2.y d)
                 | _ -> type_err "input/mouse_delta" "");
               nif_checked "is_mouse_down" 1 (function
                 | [| Value.Int b |] ->
                     Value.Int
                       (if
                          Raylib.is_mouse_button_down
                            (Raylib.MouseButton.of_int b)
                        then 1
                        else 0)
                 | _ -> type_err "input/is_mouse_down" "Int");
               nif_checked "is_mouse_pressed" 1 (function
                 | [| Value.Int b |] ->
                     Value.Int
                       (if
                          Raylib.is_mouse_button_pressed
                            (Raylib.MouseButton.of_int b)
                        then 1
                        else 0)
                 | _ -> type_err "input/is_mouse_pressed" "Int");
               nif_checked "get_mouse_wheel" 0 (function
                 | [||] -> Value.Float (Raylib.get_mouse_wheel_move ())
                 | _ -> type_err "input/get_mouse_wheel" "");
             ];
         ])
end
