open Argparse
open TerraSim.Logging
open TerraSim.Graphics
open TerraSim.Ui
open TerraSim.Gameloop

let () =
  let args = Argparse.parse_arguments () in
  init_sdl () ;
  let window = create_window "TerraSim" in
  set_window_icon window TerraSim.Sprites.daisy_00_sprite ;
  let renderer = create_renderer window in
  run_game_loop window renderer
