(** User-controlled input cursor *)

open Tsdl
open Utils.Globals
open Assets.Assetloader
open Assets.Sprites
open Cameras.Camera
open Cameras.Edit_camera
open World.Grid

type cursor = { mutable x : int; mutable y : int }
(** Cursor position in world-space coordinates *)

let global_cursor = { x = 0; y = 0 }

(** Move the global cursor by x and y deltas
    @param dx x delta
    @param dy y delta *)
let move_global_cursor (dx : int) (dy : int) =
  global_cursor.x <- global_cursor.x + dx;
  global_cursor.y <- global_cursor.y + dy

(** Move cursor to mouse position *)
let cursor_go_to_mouse () =
  let _, (x, y) = Sdl.get_mouse_state () in
  match !current_camera_mode with
  | Some (Edit2D _) ->
      global_cursor.x <- (x / scaled_tile_w ()) + edit_camera.x;
      global_cursor.y <- (y / scaled_tile_h ()) + edit_camera.y
  | _ ->
      ()
