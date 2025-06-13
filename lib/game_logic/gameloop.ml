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
  let loop_continue = Handle_input.handle_input_iter window event in
  (* Run simulation approx. once every other second *)
  if
    (not !Rendering.Popup.pause_everything_for_popup)
    && !frame_counter mod (target_fps * 2) = 0
  then
    Simulation.Simulator.simulate ();
  (* Render game screen *)
  Handle_render.handle_render_iter window !frame_counter;
  (* Render UI *)
  Handle_render.handle_ui_iter window;
  (* Delay for ideal framerate *)
  let frame_time = Int32.sub (Sdl.get_ticks ()) frame_start in
  if frame_time < frame_delay then Sdl.delay (Int32.sub frame_delay frame_time);
  real_fps := 1000 / (1 + Int32.to_int frame_time);
  _log Log_Debug "FPS: %d" !real_fps;
  loop_continue

(** Game entrypoint, call {!World.World_setup.world_setup} and then loop over
    {!gameloop_iter}
    @param window The application's SDL window
    @param ui_window A hidden window used for rendering the UI *)
let run_game_loop (window : Sdl.window) (ui_window : Sdl.window) : unit =
  let event = Sdl.Event.create () in
  let loop_continue = ref true in
  world_setup ();
  current_camera_mode := Some (Edit2D edit_camera);
  (* Initializes the global renderer state *)
  swap_render_mode ~ui_window window;
  Rendering.Ui_texture.create_ui_texture ui_window;
  (* ignore (Opengl_tutorial.main window) ; *)
  while !loop_continue do
    loop_continue := gameloop_iter window event
  done;
  Sdl.quit ()
