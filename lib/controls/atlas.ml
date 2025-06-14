(** Controls for the atlas screen *)

open Tsdl
open Utils.Sdl_utils
open Common

let change_view_key = "v"

(** Handle text input on the atlas screen
    @param e Text input event to handle
    @param window The application's SDL window *)
let atlas_handle_textinput (e : Sdl.event) (window : Sdl.window) =
  let* _ = Sdl.show_cursor false in
  let text = Sdl.Event.get e Sdl.Event.text_input_text in
  match text with
  (* Swap camera *)
  | x when x = swap_camera_key ->
      toggle_camera_mode window
  | x when x = change_view_key ->
      Rendering.Atlas_screen_data.shift_atlas_view_mode ()
  | _ ->
      ()

(** Handle input event on the atlas screen
    @param e Input event to handle
    @param window The application's SDL window *)
let handle_ui_event (e : Sdl.event) (window : Sdl.window) =
  match Sdl.Event.get e Sdl.Event.typ with
  | t when t = Sdl.Event.mouse_motion ->
      let* _ = Sdl.show_cursor true in
      ()
  | t when t = Sdl.Event.text_input ->
      atlas_handle_textinput e window
  | _ ->
      ()
