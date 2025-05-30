(** Controls for the edit screen *)

open Tsdl
open Utils.Standard_utils
open Utils.Sdl_utils
open Utils.Globals
open Cursor
open Cameras.Edit_camera
open Common
open World.Biomes
open World.Grid
open World.Altitude
open Rendering.Edit
open Rendering.Button
open Rendering.Popup
open World.Life.Lifeform

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

(** The identifier of the currently-selected edit screen button *)
let left_click_action () =
  (List.nth !edit_screen_buttons !selected_edit_screen_button).identifier

(** Handle mouse click input on the edit screen
    @param e Mouse click input event
    @param window The application's SDL window *)
let edit_handle_mouseclick (e : Sdl.event) (window : Sdl.window) =
  let _, (x, y) = Sdl.get_mouse_state () in
  if !examine_popup_open then (
    if not (point_inside_popup (x, y) !examine_popup) then
      close_examine_popup ()
  ) else if not (point_inside_popup (x, y) !edit_ui_popup) then (
    let tile_x = (x / scaled_tile_w ()) + edit_camera.x in
    let tile_y = (y / scaled_tile_h ()) + edit_camera.y in
    global_cursor.x <- tile_x;
    global_cursor.y <- tile_y;
    match left_click_action () with
    | Volcano ->
        adjust_terrain_gaussian ~raise:true tile_x tile_y
    | Asteroid ->
        adjust_terrain_gaussian ~raise:false tile_x tile_y
    | Examine -> (
        match get_grid_tile_all (tile_x, tile_y) with
        | None ->
            ()
        | Some
            [
              `Altitude alt;
              `Event event;
              `Magma magma;
              `WaterTemp w_temp;
              `WaterCurrent w_curr;
              `AirTemp a_temp;
              `AirCurrent a_cur;
              `Rain rain;
              `Biome biome;
              `Life lf;
              `Civilization civ;
            ] ->
            open_examine_popup (tile_x, tile_y) alt biome lf
        | _ ->
            [%unreachable])
  ) else
    (* UI interaction *)
      match
        List.find_index (point_inside_button (x, y)) !edit_screen_buttons
      with
    | Some idx ->
        selected_edit_screen_button := idx
    | _ ->
        ()

(** Handle input event on the edit screen
    @param e Event to handle
    @param window The application's SDL window *)
let handle_ui_event (e : Sdl.event) (window : Sdl.window) =
  match Sdl.Event.get e Sdl.Event.typ with
  | t when t = Sdl.Event.mouse_motion ->
      let* _ = Sdl.show_cursor true in
      ()
  | t when t = Sdl.Event.mouse_button_down ->
      edit_handle_mouseclick e window
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
