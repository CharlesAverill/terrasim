open Tsdl
open Sdl
open Graphics
open Ui
open Logging

let frame_counter : uint8 ref = ref 64

let target_fps = 30

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
    if Event.get event Event.keyboard_keycode = K.escape then
      loop_continue := false ;
    handle_ui_event event
  done ;
  (* Render *)
  Rendering.do_render window renderer !frame_counter !real_fps ;
  (* Delay for ideal framerate *)
  let frame_time = Int32.sub (Sdl.get_ticks ()) frame_start in
  if frame_time < frame_delay then Sdl.delay (Int32.sub frame_delay frame_time) ;
  real_fps := 1000 / Int32.to_int frame_time ;
  _log Log_Debug "FPS: %d" !real_fps ;
  !loop_continue

let run_game_loop window renderer =
  let event = Event.create () in
  let loop_continue = ref true in
  let hue = ref 0 in
  while !loop_continue do
    loop_continue := gameloop_iter window renderer event
  done ;
  Sdl.quit ()
