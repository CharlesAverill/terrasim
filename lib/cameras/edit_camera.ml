(** Camera for the edit screen *)

open Camera
open Tsdl
open Sdl
open Utils
open World.Grid

(** Levels of zoom *)
type zoom = CloseZoom | NormalZoom | FarZoom

(** Current zoom level *)
let zoom_level = ref NormalZoom

(** Width in tiles of screen according to zoom *)
let view_width () : int =
  match !zoom_level with NormalZoom -> 48 | CloseZoom -> 32 | FarZoom -> 63

(** Height in tiles of screen according to zoom *)
let view_height () : int =
  match !zoom_level with NormalZoom -> 27 | CloseZoom -> 18 | FarZoom -> 37

(** Edit screen camera position *)
let edit_camera = { x = world_width / 2; y = world_height / 2 }

(** Ensure camera stays within bounds of world grid *)
let clamp_camera_to_bounds () =
  let max_y = world_height - view_height () in
  edit_camera.y <- Utils.Standard_utils.clamp edit_camera.y (-1) (max_y + 1);
  if edit_camera.x > world_width then
    edit_camera.x <- edit_camera.x - world_width;
  if edit_camera.x + view_width () < 0 then
    edit_camera.x <- edit_camera.x + world_width

(** Increase zoom level *)
let zoom_in () =
  (zoom_level :=
     match !zoom_level with
     | CloseZoom | NormalZoom ->
         CloseZoom
     | FarZoom ->
         NormalZoom);
  clamp_camera_to_bounds ()

(** Decrease zoom level *)
let zoom_out () =
  (zoom_level :=
     match !zoom_level with
     | FarZoom | NormalZoom ->
         FarZoom
     | CloseZoom ->
         NormalZoom);
  clamp_camera_to_bounds ()

(** Move edit camera according to deltas
    @param dx x delta
    @param dy y delta *)
let move_edit_camera (dx : int) (dy : int) =
  edit_camera.x <- edit_camera.x + dx;
  edit_camera.y <- edit_camera.y + dy;
  clamp_camera_to_bounds ()

(** Get the width and height of the edit window for tiles and the height of the
    edit UI window for the UI
    @param window Application's SDL window
    @param show_ui_bar Whether the UI bar should be shown
    @return Tuple of window width and window height and UI window height *)
let get_edit_window_ui_w_h (window : Sdl.window) (show_ui_bar : bool) :
    int * (int * int) =
  let win_w, win_h = Sdl.get_window_size window in
  let wh =
    if show_ui_bar then
      0.85
    else
      1.
  in
  ( win_w,
    (int_of_float (float win_h *. wh), int_of_float (float win_h *. (1. -. wh)))
  )

(** Pixel margin that denotes when the user's mouse position at the edge of the
    screen should pan the screen *)
let pan_margin = 32

(** Pan the edit camera if the mouse is within {!pan_margin} of the screen's
    edge and the game is not paused for a popup
    @param window Application's SDL window
    @param paused_for_popup Whether the game is paused for a popup
    @param show_ui_bar Whether the UI bar should be shown *)
let pan_edit_camera_if_needed (window : Sdl.window) (paused_for_popup : bool)
    (show_ui_bar : bool) =
  if not paused_for_popup then (
    let mouse_x, mouse_y = Sdl.get_mouse_state () |> snd in
    let win_w, (win_h, ui_h) = get_edit_window_ui_w_h window show_ui_bar in
    if mouse_x < pan_margin then
      move_edit_camera (-1) 0
    else if mouse_x > win_w - pan_margin then
      move_edit_camera 1 0;
    if mouse_y > win_h + ui_h - pan_margin then
      move_edit_camera 0 1
    else if mouse_y < pan_margin then
      move_edit_camera 0 (-1)
  )
