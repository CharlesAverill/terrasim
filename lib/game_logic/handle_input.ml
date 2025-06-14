(** Handling input for the game loop *)

open Tsdl
open Cameras.Camera

(** Do one iteration of input handling
    @param window The application's SDL window
    @param event Event object to use for polling
    @return Whether the game loop should continue *)
let handle_input_iter (window : Sdl.window) (event : Sdl.event) : bool =
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
  !loop_continue
