open Tsdl
open Cameras.Camera
open Cameras.Edit_camera
open Cameras.Atlas_camera
open Controls.Cursor
open Rendering.Globe_data
open Rendering.Graphics

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

let handle_ui_iter (window : Sdl.window) =
  match !current_camera_mode with
  | Some (Edit2D _) ->
      Rendering.Edit_ui.render_edit_ui window (get_global_renderer ())
        (Controls.Cursor.global_cursor.x, Controls.Cursor.global_cursor.y)
  | _ ->
      ()
