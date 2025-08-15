(** Rendering logic for the Atlas screen UI *)

open Tsdl
open Popup
open Utils.Sdl_utils
open Utils.Standard_utils
open Atlas_screen_data

(** Draw the atlas screen UI
    @param window Application's SDL window
    @param renderer UI rendering window's SDL renderer *)
let draw_ui (window : Sdl.window) (renderer : Sdl.renderer) =
  let win_w, (win_h, ui_h) = get_window_ui_w_h window in
  (* Initialize and draw the main UI popup *)
  if not !atlas_ui_window.initialized then
    atlas_ui_window :=
      {
        bounding_box = Sdl.Rect.create ~x:0 ~y:(win_h - ui_h) ~w:win_w ~h:ui_h;
        initialized = true;
      };
  let bevel_w, ui_buffer, ((ui_area_x, ui_area_y), (ui_area_w, ui_area_h)) =
    draw_popup renderer !atlas_ui_window
  in
  let* _ = Sdl.set_render_draw_color renderer 255 0 255 255 in
  let* _ =
    Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x:0 ~y:0 ~w:25 ~h:25))
  in
  ()

let render_atlas_ui (window : Sdl.window) (renderer : Sdl.renderer) =
  let* _ = Sdl.set_render_draw_color renderer 0 0 0 0 in
  let* _ = Sdl.render_clear renderer in
  draw_ui window renderer;
  ()
