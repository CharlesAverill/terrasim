(** Handling rendering for the game loop *)

open Tsdl
open Cameras.Camera
open Cameras.Edit_camera
open Cameras.Atlas_camera
open Controls.Cursor
open Rendering.Globe_data
open Rendering.Graphics
open Utils.Sdl_utils

(** Do one iteration of rendering
    @param window The application's SDL window
    @param frame_counter The current frame count *)
let handle_render_iter (window : Sdl.window) (frame_counter : int) =
  match !current_camera_mode with
  | Some (Edit2D _) ->
      (* These need to run every frame, regardless of whether there is an input event *)
      pan_edit_camera_if_needed window
        !Rendering.Popup.pause_everything_for_popup
        !Rendering.Edit_screen_data.edit_ui_popup_open;
      cursor_go_to_mouse ();
      Rendering.Edit.render_edit_screen window frame_counter
  | Some Atlas2D ->
      (* These need to run every frame, regardless of whether there is an input event *)
      pan_atlas_camera_if_needed window;
      ignore (Rendering.Atlas.render_atlas_screen window)
  | Some Globe3D ->
      (* Spin globe *)
      if not !globe_pinned then (
        (* Apply velocity *)
        rotation_lat := !rotation_lat +. !velocity_lat;
        rotation_lon := !rotation_lon +. !velocity_lon;
        (* Decay lat and lon velocity back to defaults *)
        velocity_lat := !velocity_lat *. (1. -. globe_spin_friction);
        velocity_lon :=
          (!velocity_lon -. min_abs_velocity_lon)
          *. (1. -. globe_spin_friction)
          +. min_abs_velocity_lon;
        (* Decay lat rotation back to horizontal *)
        rotation_lat := !rotation_lat *. (1. -. (globe_spin_friction *. 0.5))
      );
      Rendering.Globe.render_globe_screen window frame_counter
  | None ->
      ()

(** Do one iteration of UI rendering
    @param window The application's SDL window
    @param frame_count The current frame count *)
let handle_ui_iter (window : Sdl.window) (frame_count : int) =
  match !current_camera_mode with
  | Some (Edit2D _) ->
      let renderer = get_global_renderer () in
      Rendering.Edit_ui.render_edit_ui window renderer
        (Controls.Cursor.global_cursor.x, Controls.Cursor.global_cursor.y)
        frame_count
  | Some Atlas2D ->
      let renderer = Rendering.Ui_texture.get_ui_renderer () in
      let* _ = Sdl.set_render_draw_color renderer 0 0 0 0 in
      let* _ = Sdl.render_clear renderer in
      let* _ = Sdl.set_render_draw_color renderer 255 0 0 255 in
      let* _ =
        Sdl.render_fill_rect renderer
          (Some (Sdl.Rect.create ~x:0 ~y:0 ~w:125 ~h:125))
      in
      ()
  | _ ->
      ()
