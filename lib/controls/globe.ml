(** Controls for the globe screen *)

open Tsdl
open Utils.Sdl_utils
open Utils.Standard_utils
open Common
open Rendering.Globe_data

let change_view_key = "v"

(** Handle text input on the globe screen
    @param e Text input event to handle
    @param window The application's SDL window
    @param ui_window A hidden window for rendering the UI *)
let globe_handle_textinput (e : Sdl.event) (window : Sdl.window)
    (ui_window : Sdl.window) =
  let* _ = Sdl.show_cursor false in
  let text = Sdl.Event.get e Sdl.Event.text_input_text in
  (* Swap camera *)
  match text with
  | x when x = swap_camera_key ->
      toggle_camera_mode window ui_window
  | x when x = change_view_key ->
      Rendering.Atlas_screen_data.shift_atlas_view_mode ()
  | _ ->
      ()

(** Handle mouse motion input on the globe screen
    @param e Mouse motion input event to handle *)
let globe_handle_mousemotion (e : Sdl.event) =
  let x = Sdl.Event.get e Sdl.Event.mouse_motion_x in
  let y = Sdl.Event.get e Sdl.Event.mouse_motion_y in
  (* Flick globe *)
  match !globe_last_mouse_pos with
  | Some (lx, ly) when !globe_pinned ->
      let dx = -2. *. float (x - lx) in
      let dy = -2. *. float (y - ly) in
      velocity_lon := clamp dx (-.max_globe_speed) max_globe_speed;
      velocity_lat := clamp dy (-.max_globe_speed) max_globe_speed;
      globe_last_mouse_pos := Some (x, y)
  | _ ->
      globe_last_mouse_pos := Some (x, y)

(** Handle mouse button input on the globe screen
    @param e Mouse button input event to handle *)
let globe_handle_mousebutton (e : Sdl.event) =
  let down = Sdl.Event.get e Sdl.Event.mouse_button_state = Sdl.pressed in
  if down then
    globe_pinned := true
  else
    globe_pinned := false

(** Handle input event on the globe screen
    @param e Input event to handle
    @param window The application's SDL window
    @param ui_window A hidden window for rendering the UI *)
let handle_ui_event (e : Sdl.event) (window : Sdl.window)
    (ui_window : Sdl.window) =
  match Sdl.Event.get e Sdl.Event.typ with
  | t when t = Sdl.Event.mouse_motion ->
      let* _ = Sdl.show_cursor true in
      globe_handle_mousemotion e
  | t when t = Sdl.Event.mouse_button_down || t = Sdl.Event.mouse_button_up ->
      globe_handle_mousebutton e
  | t when t = Sdl.Event.text_input ->
      globe_handle_textinput e window ui_window
  | _ ->
      ()
