open Tsdl
open Sdl
open Graphics
open Ui
open Logging
open Camera
open Cursor
open World_setup

let frame_counter : uint8 ref = ref 64

let target_fps = 60

let real_fps = ref 0

let frame_delay = Int32.of_int (1000 / target_fps) (* in milliseconds *)

let gameloop_iter window renderer event =
  (* Compute target framerate *)
  let frame_start = Sdl.get_ticks () in
  frame_counter := !frame_counter + 1 ;
  (* Handle input events *)
  let loop_continue = ref true in
  pump_events () ;
  while poll_event (Some event) do
    if Event.get event Event.typ = Event.key_down then
      if Event.get event Event.keyboard_keycode = Sdl.K.escape then
        loop_continue := false ;
    match !current_camera_mode with
    | Flat2D _ ->
        handle_edit_ui_event event window renderer
    | Globe3D ->
        handle_globe_ui_event event window renderer
  done ;
  (* Render *)
  ( match !current_camera_mode with
  | Flat2D _ ->
      pan_camera_if_needed window ;
      cursor_go_to_camera () ;
      Rendering.render_edit window renderer !frame_counter !real_fps
  | Globe3D ->
      Rendering.render_globe window renderer !frame_counter !real_fps ) ;
  (* Delay for ideal framerate *)
  let frame_time = Int32.sub (Sdl.get_ticks ()) frame_start in
  if frame_time < frame_delay then Sdl.delay (Int32.sub frame_delay frame_time) ;
  real_fps := 1000 / (1 + Int32.to_int frame_time) ;
  _log Log_Debug "FPS: %d" !real_fps ;
  !loop_continue

let run_game_loop window renderer =
  let event = Event.create () in
  let loop_continue = ref true in
  let hue = ref 0 in
  world_setup () ;
  while !loop_continue do
    loop_continue := gameloop_iter window renderer event
  done ;
  Sdl.quit ()
