(** Controls for the edit screen *)

open Tsdl
open Utils.Standard_utils
open Utils.Sdl_utils
open Utils.Globals
open Cursor
open Cameras.Edit_camera
open Common_controls
open World.Altitude
open Rendering.Edit_screen
open Rendering.Ui_button

(** Handle scancode input on the edit screen
    @param e Scancode event to handle
    @param window The application's SDL window *)
let edit_handle_scancodes (e : Sdl.event) (window : Sdl.window) =
  let* _ = Sdl.show_cursor false in
  let mods = Sdl.get_mod_state () in
  let shift =
    mods land Sdl.Kmod.lshift <> 0 || mods land Sdl.Kmod.rshift <> 0
  in
  let ctrl = mods land Sdl.Kmod.lctrl <> 0 || mods land Sdl.Kmod.rctrl <> 0 in
  let alt = mods land Sdl.Kmod.lalt <> 0 || mods land Sdl.Kmod.ralt <> 0 in
  let scancode = Sdl.Event.get e Sdl.Event.keyboard_scancode in
  (* UD/LR camera/cursor movement *)
  match (ctrl, shift, alt, scancode) with
  | _, _, _, x when x = Sdl.Scancode.left ->
      move_global_cursor (-1) 0;
      move_edit_camera (-1) 0
  | _, _, _, x when x = Sdl.Scancode.right ->
      move_global_cursor 1 0;
      move_edit_camera 1 0
  | _, _, _, x when x = Sdl.Scancode.up ->
      move_global_cursor 0 (-1);
      move_edit_camera 0 (-1)
  | _, _, _, x when x = Sdl.Scancode.down ->
      move_global_cursor 0 1;
      move_edit_camera 0 1
  | _, _, _, _ ->
      ()

(** Handle text input on the edit screen
    @param e Text input event to handle
    @param window The application's SDL window *)
let edit_handle_textinput (e : Sdl.event) (window : Sdl.window) =
  let* _ = Sdl.show_cursor false in
  let text = Sdl.Event.get e Sdl.Event.text_input_text in
  (* Zoom and camera swap *)
  match text with
  | "+" ->
      zoom_in ()
  | "-" ->
      zoom_out ()
  | x when x = swap_camera_key ->
      toggle_camera_mode window
  | _ ->
      ()

let left_click_action = ref (List.nth !edit_screen_buttons 0).identifier

(** Handle input event on the edit screen
    @param e Event to handle
    @param window The application's SDL window *)
let handle_edit_ui_event (e : Sdl.event) (window : Sdl.window) =
  match Sdl.Event.get e Sdl.Event.typ with
  | t when t = Sdl.Event.mouse_motion ->
      let* _ = Sdl.show_cursor true in
      ()
  | t when t = Sdl.Event.mouse_button_down -> (
      let _, (x, y) = Sdl.get_mouse_state () in
      let _, (win_h, _) = get_edit_window_ui_w_h window in
      if y < win_h then (
        (* Altitude changes *)
        let tile_x = (x / scaled_tile_w ()) + edit_camera.x in
        let tile_y = (y / scaled_tile_h ()) + edit_camera.y in
        global_cursor.x <- tile_x;
        global_cursor.y <- tile_y;
        match !left_click_action with
        | Volcano ->
            adjust_terrain_gaussian ~raise:true tile_x tile_y
        | Asteroid ->
            adjust_terrain_gaussian ~raise:false tile_x tile_y
      ) else
        (* UI interaction *)
          match List.find_index (inside_button (x, y)) !edit_screen_buttons with
        | Some idx ->
            selected_edit_screen_button := idx;
            left_click_action := (List.nth !edit_screen_buttons idx).identifier
        | _ ->
            ())
  (* Scroll around map *)
  | t when t = Sdl.Event.mouse_wheel ->
      let dx, dy =
        ( Sdl.Event.get e Sdl.Event.mouse_wheel_x,
          Sdl.Event.get e Sdl.Event.mouse_wheel_y )
      in
      let dx, dy =
        let x =
          if
            Sdl.Event.get e Sdl.Event.mouse_wheel_direction
            = Sdl.Event.mouse_wheel_flipped
          then
            -1
          else
            1
        in
        (x * dx, x * dy)
      in
      let dx, dy = (clamp dx (-1) 1, clamp dy (-1) 1) in
      move_edit_camera dx (-dy)
  | t when t = Sdl.Event.text_input ->
      edit_handle_textinput e window
  | t when t = Sdl.Event.key_down ->
      edit_handle_scancodes e window
  | _ ->
      ()
