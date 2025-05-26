open Camera
open Tsdl
open Sdl
open Utils
open Worldgrid

type zoom = CloseZoom | NormalZoom | FarZoom

let zoom_level = ref NormalZoom

let view_width () =
  match !zoom_level with NormalZoom -> 48 | CloseZoom -> 32 | FarZoom -> 63

let view_height () =
  match !zoom_level with NormalZoom -> 27 | CloseZoom -> 18 | FarZoom -> 36

let edit_camera = {x= 0; y= 0}

let clamp_camera_to_bounds () =
  (* let max_x = world_width - view_width () in *)
  let max_y = world_height - view_height () in
  (* edit_camera.x <- clamp edit_camera.x (-1) max_x ; *)
  edit_camera.y <- clamp edit_camera.y (-1) (max_y + 1) ;
  if edit_camera.x > world_width then
    edit_camera.x <- edit_camera.x - world_width ;
  if edit_camera.x + view_width () < 0 then
    edit_camera.x <- edit_camera.x + world_width

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

let move_edit_camera dx dy =
  edit_camera.x <- edit_camera.x + dx ;
  edit_camera.y <- edit_camera.y + dy ;
  clamp_camera_to_bounds ()

(* Edge panning *)
let pan_margin = 32

let pan_edit_camera_if_needed window =
  let mouse_x, mouse_y = Sdl.get_mouse_state () |> snd in
  let win_w, win_h = Sdl.get_window_size window in
  if mouse_x < pan_margin then
    move_edit_camera (-1) 0
  else if mouse_x > win_w - pan_margin then
    move_edit_camera 1 0 ;
  if mouse_y < pan_margin then
    move_edit_camera 0 (-1)
  else if mouse_y > win_h - pan_margin then
    move_edit_camera 0 1
