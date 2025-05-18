open Worldgrid
open Tsdl
open Sdl
open Globals
open Utils

(* Screen-space coordinates *)
type camera = {mutable x: int; mutable y: int}

type camera_mode = Flat2D of camera | Globe3D

type zoom = CloseZoom | NormalZoom | FarZoom

let zoom_level = ref NormalZoom

let view_width () =
  match !zoom_level with NormalZoom -> 48 | CloseZoom -> 32 | FarZoom -> 63

let view_height () =
  match !zoom_level with NormalZoom -> 27 | CloseZoom -> 18 | FarZoom -> 36

let atlas_camera = {x= 0; y= 0}

let current_camera_mode = ref (Flat2D atlas_camera)

let toggle_camera_mode renderer =
  let* _ = render_clear renderer in
  match !current_camera_mode with
  | Flat2D _ ->
      current_camera_mode := Globe3D
  | Globe3D ->
      current_camera_mode := Flat2D atlas_camera

let clamp_camera_to_bounds () =
  let max_x = world_width - view_width () in
  let max_y = world_height - view_height () in
  atlas_camera.x <- clamp atlas_camera.x (-1) max_x ;
  atlas_camera.y <- clamp atlas_camera.y (-1) (max_y + 1)

let zoom_in () =
  (zoom_level :=
     match !zoom_level with
     | CloseZoom | NormalZoom ->
         CloseZoom
     | FarZoom ->
         NormalZoom ) ;
  clamp_camera_to_bounds ()

let zoom_out () =
  (zoom_level :=
     match !zoom_level with
     | FarZoom | NormalZoom ->
         FarZoom
     | CloseZoom ->
         NormalZoom ) ;
  clamp_camera_to_bounds ()

let move_world_camera dx dy =
  atlas_camera.x <- atlas_camera.x + dx ;
  atlas_camera.y <- atlas_camera.y + dy ;
  clamp_camera_to_bounds ()

(* Edge panning *)
let pan_margin = 16

let pan_camera_if_needed window =
  let mouse_x, mouse_y = Sdl.get_mouse_state () |> snd in
  let win_w, win_h = Sdl.get_window_size window in
  if mouse_x < pan_margin then
    move_world_camera (-1) 0
  else if mouse_x > win_w - pan_margin then
    move_world_camera 1 0 ;
  if mouse_y < pan_margin then
    move_world_camera 0 (-1)
  else if mouse_y > win_h - pan_margin then
    move_world_camera 0 1
