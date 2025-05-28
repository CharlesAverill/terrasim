open Tsdl
open Sdl
open Graphics
open Ui
open Logging
open Camera
open Edit_camera
open Atlas_camera
open Cursor
open World_setup
open Globe_data
open Utils

let frame_counter : uint8 ref = ref 64

let target_fps = 60

let real_fps = ref 0

let frame_delay = Int32.of_int (1000 / target_fps) (* in milliseconds *)

let gameloop_iter window event =
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
    | Some (Edit2D _) ->
        handle_edit_ui_event event window
    | Some Atlas2D ->
        handle_atlas_ui_event event window
    | Some Globe3D ->
        handle_globe_ui_event event window
    | None ->
        ()
  done ;
  (* Run simulation approx. once every other second *)
  if !frame_counter mod (target_fps * 2) = 0 then Simulation.simulate () ;
  (* Render *)
  ( match !current_camera_mode with
  | Some (Edit2D _) ->
      pan_edit_camera_if_needed window ;
      cursor_go_to_camera () ;
      Edit_screen.render_edit window !frame_counter !real_fps
      (* let vao = Opengl_tutorial.put_tri_on_gpu () in
      let sprogram = Opengl_tutorial.compile_shaders () in
      Opengl_tutorial.render_body window sprogram vao *)
  | Some Atlas2D ->
      pan_atlas_camera_if_needed window ;
      cursor_go_to_camera () ;
      (* Atlas_screen.render_atlas window renderer *)
      ignore (Atlas_screen_opengl.atlas_render window)
  | Some Globe3D ->
      (* Spin globe *)
      if not !globe_pinned then (
        (* Apply velocity *)
        rotation_lat := !rotation_lat +. !velocity_lat ;
        rotation_lon := !rotation_lon +. !velocity_lon ;
        (* Decay lat and lon velocity back to defaults *)
        velocity_lat := !velocity_lat *. (1. -. globe_spin_friction) ;
        velocity_lon :=
          (!velocity_lon -. min_abs_velocity_lon)
          *. (1. -. globe_spin_friction)
          +. min_abs_velocity_lon ;
        (* Decay lat rotation back to horizontal *)
        rotation_lat := !rotation_lat *. (1. -. (globe_spin_friction *. 0.5))
      ) ;
      Globe_screen_opengl.globe_render window !frame_counter
      (* Globe_screen.render_globe window *)
  | None ->
      () ) ;
  (* Delay for ideal framerate *)
  let frame_time = Int32.sub (Sdl.get_ticks ()) frame_start in
  if frame_time < frame_delay then Sdl.delay (Int32.sub frame_delay frame_time) ;
  real_fps := 1000 / (1 + Int32.to_int frame_time) ;
  _log Log_Debug "FPS: %d" !real_fps ;
  !loop_continue

let run_game_loop window =
  let event = Event.create () in
  let loop_continue = ref true in
  let hue = ref 0 in
  world_setup () ;
  current_camera_mode := Some (Edit2D edit_camera) ;
  (* Initializes the global renderer state *)
  swap_render_mode window ;
  (* ignore (Opengl_tutorial.main window) ; *)
  while !loop_continue do
    loop_continue := gameloop_iter window event
  done ;
  Sdl.quit ()
