(** Logic for drawing cursors *)

open Cameras.Camera
open Cameras.Edit_camera
open Graphics
open Utils.Globals
open Utils.Sdl_utils
open Tsdl
open Assets.Assetloader
open Assets.Sprites

(** Draw a cursor to any of the three screens
    @param window Application's SDL window
    @param cursor_x
    @param cursor_y *)
let draw_cursor (window : Sdl.window) ((cursor_x, cursor_y) : int * int) =
  match !current_camera_mode with
  | Some (Edit2D _) ->
      let y = (cursor_y - edit_camera.y) * scaled_tile_h () in
      let _, (win_h, _) =
        get_edit_window_ui_w_h window !Edit_screen_data.edit_ui_popup_open
      in
      if y < win_h then
        let renderer = get_global_renderer () in
        let dst_rect =
          Sdl.Rect.create
            ~x:((cursor_x - edit_camera.x) * scaled_tile_w ())
            ~y ~w:(scaled_tile_w ()) ~h:(scaled_tile_h ())
        in
        let* _ =
          Sdl.render_copy renderer
            (texture_of_blob renderer ui_cursor_sprite)
            ~dst:dst_rect
        in
        ()
  | Some Atlas2D ->
      ()
  | _ ->
      ()
