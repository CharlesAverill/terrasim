open Tsdl
open Utils.Standard_utils
open Utils.Logging
open Utils.Globals
open World.Altitude
open Cameras.Camera
open Cameras.Edit_camera
open World.Grid
open World.Biomes
open Cursor
open Rendering.Graphics
open Rendering.Globe_data

let dragging_mouse = ref false

let edit_handle_keycodes e window =
  let* _ = Sdl.show_cursor false in
  let mods = Sdl.get_mod_state () in
  let shift =
    mods land Sdl.Kmod.lshift <> 0 || mods land Sdl.Kmod.rshift <> 0
  in
  let ctrl = mods land Sdl.Kmod.lctrl <> 0 || mods land Sdl.Kmod.rctrl <> 0 in
  let alt = mods land Sdl.Kmod.lalt <> 0 || mods land Sdl.Kmod.ralt <> 0 in
  let keycode = Sdl.Event.get e Sdl.Event.keyboard_keycode in
  match (ctrl, shift, alt, keycode) with _ -> ()

let edit_handle_scancodes e window =
  let* _ = Sdl.show_cursor false in
  let mods = Sdl.get_mod_state () in
  let shift =
    mods land Sdl.Kmod.lshift <> 0 || mods land Sdl.Kmod.rshift <> 0
  in
  let ctrl = mods land Sdl.Kmod.lctrl <> 0 || mods land Sdl.Kmod.rctrl <> 0 in
  let alt = mods land Sdl.Kmod.lalt <> 0 || mods land Sdl.Kmod.ralt <> 0 in
  let scancode = Sdl.Event.get e Sdl.Event.keyboard_scancode in
  match (ctrl, shift, alt, scancode) with
  | _, _, _, x when x = Sdl.Scancode.left ->
      move_global_cursor (-1) 0 ; move_edit_camera (-1) 0
  | _, _, _, x when x = Sdl.Scancode.right ->
      move_global_cursor 1 0 ; move_edit_camera 1 0
  | _, _, _, x when x = Sdl.Scancode.up ->
      move_global_cursor 0 (-1) ; move_edit_camera 0 (-1)
  | _, _, _, x when x = Sdl.Scancode.down ->
      move_global_cursor 0 1 ; move_edit_camera 0 1
  | _, _, _, _ ->
      ()

let atlas_handle_scancodes e window =
  let* _ = Sdl.show_cursor false in
  let mods = Sdl.get_mod_state () in
  let shift =
    mods land Sdl.Kmod.lshift <> 0 || mods land Sdl.Kmod.rshift <> 0
  in
  let ctrl = mods land Sdl.Kmod.lctrl <> 0 || mods land Sdl.Kmod.rctrl <> 0 in
  let alt = mods land Sdl.Kmod.lalt <> 0 || mods land Sdl.Kmod.ralt <> 0 in
  let scancode = Sdl.Event.get e Sdl.Event.keyboard_scancode in
  match (ctrl, shift, alt, scancode) with _, _, _, _ -> ()

let globe_handle_scancodes e window =
  let* _ = Sdl.show_cursor false in
  let mods = Sdl.get_mod_state () in
  let shift =
    mods land Sdl.Kmod.lshift <> 0 || mods land Sdl.Kmod.rshift <> 0
  in
  let ctrl = mods land Sdl.Kmod.lctrl <> 0 || mods land Sdl.Kmod.rctrl <> 0 in
  let alt = mods land Sdl.Kmod.lalt <> 0 || mods land Sdl.Kmod.ralt <> 0 in
  let scancode = Sdl.Event.get e Sdl.Event.keyboard_scancode in
  match (ctrl, shift, alt, scancode) with
  (* | _, _, _, x when x = Scancode.left ->
      rotation_lon := modf_wrap (!rotation_lon -. 15.) 360.
  | _, _, _, x when x = Scancode.right ->
      rotation_lon := modf_wrap (!rotation_lon +. 15.) 360.
  | _, _, _, x when x = Scancode.up ->
      rotation_lat := modf_wrap (!rotation_lat -. 15.) 360.
  | _, _, _, x when x = Scancode.down ->
      rotation_lat := modf_wrap (!rotation_lat +. 15.) 360. *)
  | _, _, _, _ ->
      ()

let toggle_camera_mode window =
  match !current_camera_mode with
  | Some (Edit2D _) ->
      swap_render_mode window ;
      current_camera_mode := Some Atlas2D
  | Some Atlas2D ->
      current_camera_mode := Some Globe3D
  | Some Globe3D ->
      swap_render_mode window ;
      current_camera_mode := Some (Edit2D edit_camera)
  | None ->
      ()

let edit_handle_textinput e window =
  let* _ = Sdl.show_cursor false in
  let text = Sdl.Event.get e Sdl.Event.text_input_text in
  match text with
  | "+" ->
      zoom_in ()
  | "-" ->
      zoom_out ()
  | "c" ->
      toggle_camera_mode window
  | _ ->
      ()

let globe_handle_textinput e window =
  let* _ = Sdl.show_cursor false in
  let text = Sdl.Event.get e Sdl.Event.text_input_text in
  match text with "c" -> toggle_camera_mode window | _ -> ()

let atlas_handle_textinput e window =
  let* _ = Sdl.show_cursor false in
  let text = Sdl.Event.get e Sdl.Event.text_input_text in
  match text with "c" -> toggle_camera_mode window | _ -> ()

let globe_handle_mousemotion e =
  let x = Sdl.Event.get e Sdl.Event.mouse_motion_x in
  let y = Sdl.Event.get e Sdl.Event.mouse_motion_y in
  match !globe_last_mouse_pos with
  | Some (lx, ly) when !globe_pinned ->
      let dx = float (x - lx) in
      let dy = float (y - ly) in
      let mag =
        0.5
        (* sqrt ((dx ** 2.) +. (dy ** 2.)) in *)
      in
      let dx, dy = (-.dx /. mag, -.dy /. mag) in
      (* rotation_lon := modf_wrap (!rotation_lon +. dx) 360. ;
      rotation_lat := !rotation_lat +. dy ; *)
      velocity_lon := clamp dx (-.flick_globe_speed) flick_globe_speed ;
      velocity_lat := dy ;
      globe_last_mouse_pos := Some (x, y)
  | _ ->
      globe_last_mouse_pos := Some (x, y)

let globe_handle_mousebutton e =
  let down = Sdl.Event.get e Sdl.Event.mouse_button_state = Sdl.pressed in
  if down then
    globe_pinned := true
  else
    globe_pinned := false

let handle_edit_ui_event (e : Sdl.event) window =
  let edit_updated = ref false in
  ( match Sdl.Event.get e Sdl.Event.typ with
  | t when t = Sdl.Event.mouse_motion ->
      let* _ = Sdl.show_cursor true in
      ()
  | t when t = Sdl.Event.mouse_button_down ->
      let _, (x, y) = Sdl.get_mouse_state () in
      let _, (win_h, _) = get_edit_window_ui_height window in
      if y < win_h then (
        let button = Sdl.Event.get e Sdl.Event.mouse_button_button in
        let tile_x = (x / scaled_tile_w ()) + edit_camera.x in
        let tile_y = (y / scaled_tile_h ()) + edit_camera.y in
        set_global_cursor tile_x tile_y ;
        match button with
        | 1 (* Left click *) ->
            adjust_terrain_gaussian ~raise:true tile_x tile_y ;
            edit_updated := true
        | 3 (* Right click *) ->
            adjust_terrain_gaussian ~raise:false tile_x tile_y ;
            edit_updated := true
        | _ ->
            ()
      )
  | t when t = Sdl.Event.mouse_wheel ->
      let dx, dy =
        ( Sdl.Event.get e Sdl.Event.mouse_wheel_x
        , Sdl.Event.get e Sdl.Event.mouse_wheel_y )
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
      (* invert Y to match UI expectation *)
  | t when t = Sdl.Event.text_input ->
      edit_handle_textinput e window ;
      edit_handle_keycodes e window
  | t when t = Sdl.Event.key_down ->
      edit_handle_scancodes e window
  | _ ->
      () ) ;
  if !edit_updated then (
    clear_globe_cache () ;
    clear_opengl_globe_cache ()
  )

let handle_atlas_ui_event (e : Sdl.event) window =
  match Sdl.Event.get e Sdl.Event.typ with
  | t when t = Sdl.Event.mouse_motion ->
      let* _ = Sdl.show_cursor true in
      ()
  | t when t = Sdl.Event.mouse_button_down ->
      ()
  | t when t = Sdl.Event.text_input ->
      atlas_handle_textinput e window
  | t when t = Sdl.Event.key_down ->
      atlas_handle_scancodes e window
  | _ ->
      ()

let handle_globe_ui_event (e : Sdl.event) window =
  match Sdl.Event.get e Sdl.Event.typ with
  | t when t = Sdl.Event.mouse_motion ->
      let* _ = Sdl.show_cursor true in
      globe_handle_mousemotion e
  | t when t = Sdl.Event.mouse_button_down || t = Sdl.Event.mouse_button_up ->
      globe_handle_mousebutton e
  | t when t = Sdl.Event.text_input ->
      globe_handle_textinput e window
  | t when t = Sdl.Event.key_down ->
      globe_handle_scancodes e window
  | _ ->
      ()
