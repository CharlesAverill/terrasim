(** Utilities for SDL *)

open Tsdl
open Standard_utils
open Logging

(** Let binding for SDL results *)
let ( let* ) (r : 'a Sdl.result) (f : 'a -> 'b) : 'b =
  match r with Ok x -> f x | Error (`Msg e) -> fatal rc_SDL "%s" e

(** Draw a circle at center [(x, y)] with [radius]
    https://gist.github.com/Gumichan01/332c26f6197a432db91cc4327fcabb1c

    @param renderer SDL renderer to do draw
    @param x
    @param y
    @param radius *)
let render_draw_circle (renderer : Sdl.renderer) ((x, y) : int * int)
    (radius : int) =
  let rec loop offsetx offsety d =
    if offsety < offsetx then
      ()
    else
      let draw_points : (int * int) list =
        [
          (x + offsetx, y + offsety);
          (x + offsety, y + offsetx);
          (x - offsetx, y + offsety);
          (x - offsety, y + offsetx);
          (x + offsetx, y - offsety);
          (x + offsety, y - offsetx);
          (x - offsetx, y - offsety);
          (x - offsety, y - offsetx);
        ]
      in
      List.iter
        (fun (px, py) ->
          let* _ = Sdl.render_draw_point renderer px py in
          ())
        draw_points;
      if d >= 2 * offsetx then
        loop (offsetx + 1) offsety (d - (2 * offsetx) - 1)
      else if d < 2 * (radius - offsety) then
        loop offsetx (offsety - 1) (d + (2 * offsety) - 1)
      else
        loop (offsetx + 1) (offsety - 1) (d + (2 * (offsety - offsetx - 1)))
  in
  loop 0 radius (radius - 1)

(** Fill a circle at center [(x, y)] with [radius]
    https://gist.github.com/Gumichan01/332c26f6197a432db91cc4327fcabb1c

    @param renderer SDL renderer to do draw
    @param x
    @param y
    @param radius *)
let render_fill_circle (renderer : Sdl.renderer) ((x, y) : int * int)
    (radius : int) =
  let rec loop offsetx offsety d =
    if offsety < offsetx then
      ()
    else
      let draw_lines =
        [
          (x - offsety, y + offsetx, x + offsety, y + offsetx);
          (x - offsetx, y + offsety, x + offsetx, y + offsety);
          (x - offsetx, y - offsety, x + offsetx, y - offsety);
          (x - offsety, y - offsetx, x + offsety, y - offsetx);
        ]
      in
      List.iter
        (fun (x1, y1, x2, y2) ->
          let* _ = Sdl.render_draw_line renderer x1 y1 x2 y2 in
          ())
        draw_lines;
      if d >= 2 * offsetx then
        loop (offsetx + 1) offsety (d - (2 * offsetx) - 1)
      else if d < 2 * (radius - offsety) then
        loop offsetx (offsety - 1) (d + (2 * offsety) - 1)
      else
        loop (offsetx + 1) (offsety - 1) (d + (2 * (offsety - offsetx - 1)))
  in
  loop 0 radius (radius - 1)

(** Parse a hex string into an integer *)
let int_of_hex_string s =
  int_of_string
    (if String.starts_with ~prefix:"0x" s then
       s
     else
       "0x" ^ s)

(** Get RGB values of a hex code color string *)
let rgb_of_hex (hex : string) : int * int * int =
  let hex = String.uppercase_ascii hex in
  let hex =
    if String.length hex = 7 && hex.[0] = '#' then
      String.sub hex 1 6
    else if String.length hex = 6 then
      hex
    else
      fatal rc_Error "Invalid hex code"
  in
  let r = int_of_hex_string (String.sub hex 0 2) in
  let g = int_of_hex_string (String.sub hex 2 2) in
  let b = int_of_hex_string (String.sub hex 4 2) in
  (r, g, b)

(** Set the renderer draw color
    @param r
    @param g
    @param b
    @param renderer The renderer to set the color of *)
let set_render_color ((r, g, b) : int * int * int) (renderer : Sdl.renderer) =
  let* _ = Sdl.set_render_draw_color renderer r g b 255 in
  ()

(** Get an SDL Color object from RGB values *)
let sdlcolor_of_tuple ((r, g, b) : int * int * int) : Sdl.color =
  Sdl.Color.create ~r ~g ~b ~a:255

(** Clear a cache with SDL textures as the values and destroying the textures *)
let clear_texture_cache (table : ('a, Sdl.texture) Hashtbl.t) =
  Seq.iter
    (fun x -> Sdl.destroy_texture (Hashtbl.find table x))
    (Hashtbl.to_seq_keys table);
  Hashtbl.clear table

(** Get an [Sdl.rect]'s bounding box
    @return [x, y, w, h] *)
let get_rect_bounding_box (r : Sdl.rect) : int * int * int * int =
  (Sdl.Rect.x r, Sdl.Rect.y r, Sdl.Rect.w r, Sdl.Rect.h r)

(** Draw a rect with thickness that starts from the provided [Sdl.rect] and
    grows outwards
    @param renderer Application's SDL renderer
    @param base_rect The rect defining where to draw
    @param thickness Number of pixels outward to draw *)
let draw_rect_outer_thickness (renderer : Sdl.renderer) (base_rect : Sdl.rect)
    (thickness : int) =
  let x, y, w, h = get_rect_bounding_box base_rect in
  for i = 0 to thickness do
    let* _ =
      Sdl.render_draw_rect renderer
        (Some
           (Sdl.Rect.create ~x:(x - i) ~y:(y - i)
              ~w:(w + (2 * i))
              ~h:(h + (2 * i))))
    in
    ()
  done
