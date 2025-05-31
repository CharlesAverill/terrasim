(** The entrypoint and main game loop *)

open Tsdl
open Rendering.Graphics
open Rendering.Globe_data
open Controls.Cursor
open Utils.Standard_utils
open Utils.Logging
open Cameras.Camera
open Cameras.Edit_camera
open Cameras.Atlas_camera
open World.World_setup

(** Number of frames that have passed *)
let frame_counter = ref 64

let target_fps = 60
let real_fps = ref 0
let frame_delay = Int32.of_int (1000 / target_fps) (* in milliseconds *)

(** One iteration of the main gameloop. Stages:
    - Input event handling
    - Simulation
    - Rendering
    - Delay to maintain {!target_fps}

    @param window The application's SDL window
    @param event An SDl event object *)
let gameloop_iter (window : Sdl.window) (event : Sdl.event) : bool =
  (* Compute target framerate *)
  let frame_start = Sdl.get_ticks () in
  frame_counter := !frame_counter + 1;
  (* Handle input events *)
  let loop_continue = ref true in
  Sdl.pump_events ();
  while Sdl.poll_event (Some event) do
    if Sdl.Event.get event Sdl.Event.typ = Sdl.Event.key_down then
      if Sdl.Event.get event Sdl.Event.keyboard_keycode = Sdl.K.escape then
        loop_continue := false;
    match !current_camera_mode with
    | Some (Edit2D _) ->
        Controls.Edit.handle_ui_event event window
    | Some Atlas2D ->
        Controls.Atlas.handle_ui_event event window
    | Some Globe3D ->
        Controls.Globe.handle_ui_event event window
    | None ->
        ()
  done;
  (* Run simulation approx. once every other second *)
  if
    (not !Rendering.Popup.pause_everything_for_popup)
    && !frame_counter mod (target_fps * 2) = 0
  then
    Simulation.Simulator.simulate ();
  (* Render *)
  (match !current_camera_mode with
  | Some (Edit2D _) ->
      (* These need to run every frame, regardless of whether there is an input event *)
      pan_edit_camera_if_needed window
        !Rendering.Popup.pause_everything_for_popup
        !Rendering.Edit_screen_data.edit_ui_popup_open;
      cursor_go_to_mouse ();
      Rendering.Edit.render_edit_screen window !frame_counter
        (global_cursor.x, global_cursor.y)
  | Some Atlas2D ->
      (* These need to run every frame, regardless of whether there is an input event *)
      pan_atlas_camera_if_needed window;
      ignore (Rendering.Atlas.render_atlas_screen window)
  | Some Globe3D ->
      (* Spin globe *)
      if not !globe_pinned then (
        (* Apply velocity *)
        rotation_lat := !rotation_lat +. !velocity_lat;
        rotation_lon := !rotation_lon +. !velocity_lon;
        (* Decay lat and lon velocity back to defaults *)
        velocity_lat := !velocity_lat *. (1. -. globe_spin_friction);
        velocity_lon :=
          (!velocity_lon -. min_abs_velocity_lon)
          *. (1. -. globe_spin_friction)
          +. min_abs_velocity_lon;
        (* Decay lat rotation back to horizontal *)
        rotation_lat := !rotation_lat *. (1. -. (globe_spin_friction *. 0.5))
      );
      Rendering.Globe.render_globe_screen window !frame_counter
  | None ->
      ());
  (* Delay for ideal framerate *)
  let frame_time = Int32.sub (Sdl.get_ticks ()) frame_start in
  if frame_time < frame_delay then Sdl.delay (Int32.sub frame_delay frame_time);
  real_fps := 1000 / (1 + Int32.to_int frame_time);
  _log Log_Debug "FPS: %d" !real_fps;
  !loop_continue

(** Game entrypoint, call {!World.World_setup.world_setup} and then loop over
    {!gameloop_iter}
    @param window The application's SDL window *)
let run_game_loop (window : Sdl.window) : unit =
  let event = Sdl.Event.create () in
  let loop_continue = ref true in
  world_setup ();
  current_camera_mode := Some (Edit2D edit_camera);
  (* Initializes the global renderer state *)
  swap_render_mode window;
  (* ignore (Opengl_tutorial.main window) ; *)
  while !loop_continue do
    loop_continue := gameloop_iter window event
  done;
  Sdl.quit ()
