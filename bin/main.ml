open Argparse
open Tsdl
open Sdl
open TerraSim.Logging
open TerraSim.Graphics
open TerraSim.Ui

let () =
  let args = Argparse.parse_arguments () in
  init_sdl () ;
  let w = create_window "TerraSim" in
  let event = Event.create () in
  let loop_continue = ref true in
  let hue = ref 0 in
  while !loop_continue do
    (* Handle events in event queue *)
    pump_events () ;
    while poll_event (Some event) do
      if Event.get event Event.typ = Event.quit then loop_continue := false ;
      handle_ui_event event
    done ;
    (* Render *)
    hue := (!hue + 1) mod 360 ;
    let r, g, b = hsv_to_rgb !hue 255 128 in
    let color = get_color ~format:(get_window_pixel_format w) r g b in
    let surf = get_window_surf w in
    let* _ = fill_rect surf None color in
    let* _ = update_window_surface w in
    Sdl.delay 20l ; render_ui surf
  done ;
  Sdl.quit ()
