(** Controls shared between several screens *)

open Tsdl
open Cameras.Camera
open Cameras.Edit_camera
open Rendering.Graphics

let swap_camera_key = "c"
let hide_ui_key = "h"

(** Toggle camera mode between the edit, atlas, and globe screens
    @param window Application's SDL window *)
let toggle_camera_mode (window : Sdl.window) =
  match !current_camera_mode with
  | Some (Edit2D _) ->
      swap_render_mode window;
      current_camera_mode := Some Atlas2D
  | Some Atlas2D ->
      current_camera_mode := Some Globe3D
  | Some Globe3D ->
      swap_render_mode window;
      current_camera_mode := Some (Edit2D edit_camera)
  | None ->
      ()
