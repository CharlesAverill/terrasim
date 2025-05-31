open Argparse
open Utils.Logging
open Rendering.Graphics
open Game_logic.Gameloop

let test_noise () =
  let open Tsdl in
  let open World.Noise in
  let open World.Grid in
  let open Utils.Sdl_utils in
  let win = create_window ~fullscreen_win:true "TerraSim noise test" in
  let renderer = create_renderer win in
  let event = Sdl.Event.create () in

  let screen_w, screen_h = Sdl.get_window_size win in

  let tile_w = screen_w / world_width in
  let tile_h = screen_h / world_height in

  let rendered = ref false in
  init_noise_gen ();
  let rec loop () =
    let frame_start = Sdl.get_ticks () in
    if not !rendered then (
      for ty = 0 to world_height do
        for tx = 0 to world_width do
          let dist_from_eq =
            abs_float
              (float (ty - (world_height / 2)) /. float (world_height / 2))
          in
          let noise_value =
            fbm_2d ~octaves:5 ~scale_xy:(0.5, 0.5) ~contrast:2. ~base_ampl:2.
              (tx, ty)
              (world_width, world_height)
          in
          let noise_value =
            noise_value
            -. dist_from_eq
               *. fbm_2d ~octaves:2 (tx, ty) (world_width, world_height)
          in
          let v =
            if noise_value < 0.35 then
              0
            else
              255
          in
          set_render_color (v, v, v) renderer;

          let rect =
            Sdl.Rect.create ~x:(tx * tile_w) ~y:(ty * tile_h) ~w:tile_w
              ~h:tile_h
          in
          let* _ = Sdl.render_fill_rect renderer (Some rect) in
          ()
        done
      done;

      rendered := true;
      Sdl.render_present renderer
    );

    (* Handle events *)
    let rec handle_events () =
      if Sdl.poll_event (Some event) then
        let open Sdl.Event in
        let typ = get event typ in
        if typ = Sdl.Event.quit then
          false
        else if
          typ = Sdl.Event.key_down && get event keyboard_keycode = Sdl.K.escape
        then
          false
        else
          handle_events ()
      else
        true
    in
    if handle_events () then (
      let frame_time = Int32.sub (Sdl.get_ticks ()) frame_start in
      if frame_time < frame_delay then
        Sdl.delay (Int32.sub frame_delay frame_time);
      loop ()
    )
  in

  loop ();
  shut_down_noise_gen ()

(** Program entrypoint, sets up SDL and passes off to {!run_game_loop} *)
let () =
  let args = Argparse.parse_arguments () in
  if args.test_noise then
    test_noise ()
  else (
    init_sdl ();
    let window = create_window "TerraSim" in
    set_window_icon window Assets.Sprites.daisy_00_sprite;
    run_game_loop window
  )
