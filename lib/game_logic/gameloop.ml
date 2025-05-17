open Tsdl
open Sdl
open Ui
open Graphics

let tile_texture window renderer texture =
  (* 1. Get window size *)
  let window_w, window_h =
    match Sdl.get_window_size window with w, h -> (w, h)
  in
  (* 2. Get tile size from texture *)
  let tile_w, tile_h =
    match Sdl.query_texture texture with
    | Error (`Msg e) ->
        failwith ("query_texture failed: " ^ e)
    | Ok (_, _, (w, h)) ->
        (w, h)
  in
  (* 3. Compute how many tiles fit *)
  let cols = (window_w / tile_w) + 1 in
  let rows = (window_h / tile_h) + 1 in
  (* 4. Draw tile at each grid position *)
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let dst_rect =
        Sdl.Rect.create ~x:(col * tile_w) ~y:(row * tile_h) ~w:tile_w ~h:tile_h
      in
      ignore (Sdl.render_copy renderer texture ~dst:dst_rect)
    done
  done

let gameloop_iter window renderer event =
  let loop_continue = ref true in
  (* Handle input events *)
  pump_events () ;
  while poll_event (Some event) do
    if Event.get event Event.typ = Event.quit then loop_continue := false ;
    handle_ui_event event
  done ;
  (* Render *)
  ignore (Sdl.render_clear renderer) ;
  tile_texture window renderer
    (Spriteloader.texture_of_blob renderer Sprites.daisy_0_sprite) ;
  render_ui renderer ;
  (* assuming you adapt render_ui to use renderer *)
  Sdl.render_present renderer ;
  Sdl.delay 20l ;
  !loop_continue

let run_game_loop window renderer =
  let event = Event.create () in
  let loop_continue = ref true in
  let hue = ref 0 in
  while !loop_continue do
    loop_continue := gameloop_iter window renderer event
  done ;
  Sdl.quit ()
