(** Camera for the atlas screen *)

open Camera
open Tsdl
open Sdl
open Utils
open World.Grid

(** Atlas screen camera position, [y] goes unused *)
let atlas_camera = { x = 0; y = 0 }

(** Move edit camera according to delta
    @param dx x delta *)
let move_atlas_camera (dx : int) = atlas_camera.x <- atlas_camera.x + dx

(** Pixel margin that denotes when the user's mouse position at the edge of the
    screen should pan the screen *)
let pan_margin = 32

(** Pan the atlas camera if the mouse is within {!pan_margin} of the screen's
    edge
    @param window Application's SDL window*)
let pan_atlas_camera_if_needed (window : Sdl.window) =
  let mouse_x, mouse_y = Sdl.get_mouse_state () |> snd in
  let win_w, win_h = Sdl.get_window_size window in
  if mouse_x < pan_margin then
    move_atlas_camera 1
  else if mouse_x > win_w - pan_margin then
    move_atlas_camera (-1)
