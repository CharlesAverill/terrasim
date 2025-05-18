open Tsdl
open Sdl
open Sdl.Event
open Logging
open Globals
open Altitude
open Camera
open Worldgrid
open Biomes
open Cursor
open Utils

let render_ui window (renderer : Sdl.renderer) = draw_cursor renderer

let atlas_handle_keycodes e window =
  let* _ = show_cursor false in
  let mods = get_mod_state () in
  let shift = mods land Kmod.lshift <> 0 || mods land Kmod.rshift <> 0 in
  let ctrl = mods land Kmod.lctrl <> 0 || mods land Kmod.rctrl <> 0 in
  let alt = mods land Kmod.lalt <> 0 || mods land Kmod.ralt <> 0 in
  let keycode = get e keyboard_keycode in
  match (ctrl, shift, alt, keycode) with _ -> ()

let atlas_handle_scancodes e window =
  let* _ = show_cursor false in
  let mods = get_mod_state () in
  let shift = mods land Kmod.lshift <> 0 || mods land Kmod.rshift <> 0 in
  let ctrl = mods land Kmod.lctrl <> 0 || mods land Kmod.rctrl <> 0 in
  let alt = mods land Kmod.lalt <> 0 || mods land Kmod.ralt <> 0 in
  let scancode = get e keyboard_scancode in
  match (ctrl, shift, alt, scancode) with
  | _, _, _, x when x = Scancode.left ->
      move_global_cursor (-1) 0 ; move_world_camera (-1) 0
  | _, _, _, x when x = Scancode.right ->
      move_global_cursor 1 0 ; move_world_camera 1 0
  | _, _, _, x when x = Scancode.up ->
      move_global_cursor 0 (-1) ; move_world_camera 0 (-1)
  | _, _, _, x when x = Scancode.down ->
      move_global_cursor 0 1 ; move_world_camera 0 1
  | _, _, _, _ ->
      ()

let globe_handle_scancodes e window =
  let* _ = show_cursor false in
  let mods = get_mod_state () in
  let shift = mods land Kmod.lshift <> 0 || mods land Kmod.rshift <> 0 in
  let ctrl = mods land Kmod.lctrl <> 0 || mods land Kmod.rctrl <> 0 in
  let alt = mods land Kmod.lalt <> 0 || mods land Kmod.ralt <> 0 in
  let scancode = get e keyboard_scancode in
  match (ctrl, shift, alt, scancode) with
  | _, _, _, x when x = Scancode.left ->
      rotation_lon := mod_wrap (!rotation_lon - 15) 360
  | _, _, _, x when x = Scancode.right ->
      rotation_lon := mod_wrap (!rotation_lon + 15) 360
  | _, _, _, x when x = Scancode.up ->
      rotation_lat := mod_wrap (!rotation_lat - 15) 360
  | _, _, _, x when x = Scancode.down ->
      rotation_lat := mod_wrap (!rotation_lat + 15) 360
  | _, _, _, _ ->
      ()

let atlas_handle_textinput e window renderer =
  let* _ = show_cursor false in
  let text = get e text_input_text in
  match text with
  | "+" ->
      zoom_in ()
  | "-" ->
      zoom_out ()
  | "c" ->
      toggle_camera_mode renderer
  | _ ->
      ()

let globe_handle_textinput e window renderer =
  let* _ = show_cursor false in
  let text = get e text_input_text in
  match text with "c" -> toggle_camera_mode renderer | _ -> ()

let handle_atlas_ui_event (e : Sdl.event) window renderer =
  let atlas_updated = ref false in
  ( match get e typ with
  | t when t = mouse_motion ->
      let* _ = show_cursor true in
      ()
  | t when t = mouse_button_down -> (
      let _, (x, y) = Sdl.get_mouse_state () in
      let button = get e mouse_button_button in
      let tile_x = (x / scaled_tile_w ()) + atlas_camera.x in
      let tile_y = (y / scaled_tile_h ()) + atlas_camera.y in
      set_global_cursor tile_x tile_y ;
      match button with
      | 1 (* Left click *) ->
          raise_terrain_gaussian tile_x tile_y ;
          atlas_updated := true
      | 3 (* Right click *) ->
          let current_tile =
            match get_global_tile tile_x tile_y with
            | None ->
                Land (Nothing 0)
            | Some x ->
                x
          in
          set_global_tile tile_x tile_y (decrease_tile current_tile) ;
          atlas_updated := true
      | _ ->
          () )
  | t when t = text_input ->
      atlas_handle_textinput e window renderer ;
      atlas_handle_keycodes e window
  | t when t = key_down ->
      atlas_handle_scancodes e window
  | _ ->
      () ) ;
  if !atlas_updated then clear_globe_cache ()

let handle_globe_ui_event (e : Sdl.event) window renderer =
  match get e typ with
  | t when t = mouse_motion ->
      let* _ = show_cursor true in
      ()
  | t when t = mouse_button_down ->
      ()
  | t when t = text_input ->
      globe_handle_textinput e window renderer
  | t when t = key_down ->
      globe_handle_scancodes e window
  | _ ->
      ()
