open Tsdl
open Utils.Globals
open Assets.Spriteloader
open Assets.Sprites
open Cameras.Camera
open Cameras.Edit_camera
open World.Grid

(* World-space coordinates *)
type cursor = { mutable x : int; mutable y : int }

let global_cursor = { x = 0; y = 0 }

let set_global_cursor x y =
  global_cursor.x <- x;
  global_cursor.y <- y

let move_global_cursor dx dy =
  set_global_cursor (global_cursor.x + dx) (global_cursor.y + dy)

let cursor_go_to_camera () =
  let _, (x, y) = Sdl.get_mouse_state () in
  match !current_camera_mode with
  | Some (Edit2D _) ->
      set_global_cursor
        ((x / scaled_tile_w ()) + edit_camera.x)
        ((y / scaled_tile_h ()) + edit_camera.y)
  | Some Atlas2D ->
      set_global_cursor x y
  | _ ->
      ()
