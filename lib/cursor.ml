open Tsdl
open Sdl
open Utils
open Spriteloader
open Sprites
open Camera
open Globals

(* World-space coordinates *)
type cursor = {mutable x: int; mutable y: int}

let global_cursor = {x= 0; y= 0}

let set_global_cursor x y =
  global_cursor.x <- x ;
  global_cursor.y <- y

let move_global_cursor dx dy =
  global_cursor.x <- global_cursor.x + dx ;
  global_cursor.y <- global_cursor.y + dy

let draw_cursor renderer =
  let dst_rect =
    Sdl.Rect.create
      ~x:((global_cursor.x - atlas_camera.x) * scaled_tile_w ())
      ~y:((global_cursor.y - atlas_camera.y) * scaled_tile_h ())
      ~w:(scaled_tile_h ()) ~h:(scaled_tile_w ())
  in
  let* _ =
    Sdl.render_copy renderer
      (texture_of_blob renderer ui_cursor_sprite)
      ~dst:dst_rect
  in
  ()

let cursor_go_to_camera () =
  let _, (x, y) = Sdl.get_mouse_state () in
  set_global_cursor
    ((x / scaled_tile_w ()) + atlas_camera.x)
    ((y / scaled_tile_h ()) + atlas_camera.y)
