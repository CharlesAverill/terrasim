open Argparse
open Utils.Logging
open Rendering.Graphics
open Controls.Ui
open Game_logic.Gameloop

let () =
  let args = Argparse.parse_arguments () in
  init_sdl ();
  let window = create_window "TerraSim" in
  set_window_icon window Assets.Sprites.daisy_00_sprite;
  run_game_loop window
