open Logging
open Tsdl
open Sdl
open Spriteloader
open Utils

let init_sdl () =
  let* _ = init Init.(video + events) in
  ()

let create_window ?(w : int = 1920) ?(h : int = 1080) (window_name : string) =
  let open Window in
  let* w =
    create_window ~w ~h window_name (Window.vulkan + Window.fullscreen_desktop)
  in
  set_window_minimum_size w ~w:1280 ~h:720 ;
  w

let create_renderer window =
  let* r = create_renderer ~flags:Renderer.software window in
  r

let set_window_icon window blob = set_window_icon window (surface_of_blob blob)

let get_window_surf (w : window) =
  let* s = get_window_surface w in
  s

let get_color ?(format : Pixel.format_enum = Pixel.format_rgba8888)
    ?(a : int = 255) r g b =
  let* f = alloc_format format in
  let x = map_rgba f r g b a in
  free_format f ; x

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

(* https://gist.github.com/Gumichan01/332c26f6197a432db91cc4327fcabb1c *)
let render_draw_circle renderer x y radius =
  let rec loop offsetx offsety d status =
    if offsety < offsetx then
      status
    else
      let draw_points : (int * int) list =
        [ (x + offsetx, y + offsety)
        ; (x + offsety, y + offsetx)
        ; (x - offsetx, y + offsety)
        ; (x - offsety, y + offsetx)
        ; (x + offsetx, y - offsety)
        ; (x + offsety, y - offsetx)
        ; (x - offsetx, y - offsety)
        ; (x - offsety, y - offsetx) ]
      in
      let status =
        List.fold_left
          (fun acc (px, py) ->
            match render_draw_point renderer px py with
            | Ok () ->
                acc
            | Error _ ->
                -1 )
          status draw_points
      in
      if status < 0 then
        status
      else if d >= 2 * offsetx then
        loop (offsetx + 1) offsety (d - (2 * offsetx) - 1) status
      else if d < 2 * (radius - offsety) then
        loop offsetx (offsety - 1) (d + (2 * offsety) - 1) status
      else
        loop (offsetx + 1) (offsety - 1)
          (d + (2 * (offsety - offsetx - 1)))
          status
  in
  loop 0 radius (radius - 1) 0

let render_fill_circle renderer x y radius =
  let rec loop offsetx offsety d status =
    if offsety < offsetx then
      status
    else
      let draw_lines =
        [ (x - offsety, y + offsetx, x + offsety, y + offsetx)
        ; (x - offsetx, y + offsety, x + offsetx, y + offsety)
        ; (x - offsetx, y - offsety, x + offsetx, y - offsety)
        ; (x - offsety, y - offsetx, x + offsety, y - offsetx) ]
      in
      let status =
        List.fold_left
          (fun acc (x1, y1, x2, y2) ->
            match render_draw_line renderer x1 y1 x2 y2 with
            | Ok () ->
                acc
            | Error _ ->
                -1 )
          status draw_lines
      in
      if status < 0 then
        status
      else if d >= 2 * offsetx then
        loop (offsetx + 1) offsety (d - (2 * offsetx) - 1) status
      else if d < 2 * (radius - offsety) then
        loop offsetx (offsety - 1) (d + (2 * offsety) - 1) status
      else
        loop (offsetx + 1) (offsety - 1)
          (d + (2 * (offsety - offsetx - 1)))
          status
  in
  loop 0 radius (radius - 1) 0
