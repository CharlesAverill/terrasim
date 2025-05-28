open Cameras.Camera
open Cameras.Edit_camera
open Graphics
open Utils.Globals
open Utils.Standard_utils
open Tsdl
open Assets.Spriteloader
open Assets.Sprites

let draw_cursor window (cursor_x, cursor_y) =
  match !current_camera_mode with
  | Some (Edit2D _) ->
      let y = (cursor_y - edit_camera.y) * scaled_tile_h () in
      let _, (win_h, _) = get_edit_window_ui_height window in
      if y < win_h then
        let renderer = get_global_renderer () in
        let dst_rect =
          Sdl.Rect.create
            ~x:((cursor_x - edit_camera.x) * scaled_tile_w ())
            ~y ~w:(scaled_tile_h ()) ~h:(scaled_tile_w ())
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
