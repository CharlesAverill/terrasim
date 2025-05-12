open Logging
open Tsdl
open Sdl.Window

let ( let* ) r f =
  match r with Ok x -> f x | Error (`Msg e) -> fatal rc_SDL "%s" e

let init_sdl () =
  let* _ = Sdl.init Sdl.Init.(video + events) in
  ()

let create_window ?(w : int = 1920) ?(h : int = 1080) (window_name : string) =
  let* w =
    Sdl.create_window ~w ~h window_name
      (Sdl.Window.resizable + Sdl.Window.opengl)
  in
  w

let get_window_surf (w : Sdl.window) =
  let* s = Sdl.get_window_surface w in
  s

let get_color ?(format : Sdl.Pixel.format_enum = Sdl.Pixel.format_rgba8888)
    ?(a : int = 255) r g b =
  let* f = Sdl.alloc_format format in
  let x = Sdl.map_rgba f r g b a in
  Sdl.free_format f ; x

let hsv_to_rgb h s v =
  let h = float h /. 60.0 in
  let c = float v *. float s /. 255.0 /. 255.0 in
  let x = c *. (1.0 -. abs_float (mod_float h 2.0 -. 1.0)) in
  let r, g, b =
    match int_of_float h mod 6 with
    | 0 ->
        (c, x, 0.0)
    | 1 ->
        (x, c, 0.0)
    | 2 ->
        (0.0, c, x)
    | 3 ->
        (0.0, x, c)
    | 4 ->
        (x, 0.0, c)
    | _ ->
        (c, 0.0, x)
  in
  let m = (float v /. 255.0) -. c in
  let scale x = int_of_float ((x +. m) *. 255.0) in
  (scale r, scale g, scale b)
