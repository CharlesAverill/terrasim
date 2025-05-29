open Argparse
open Utils.Logging
open Rendering.Graphics
open Game_logic.Gameloop

(** Program entrypoint, sets up SDL and passes off to {!run_game_loop} *)
let () =
  let _args = Argparse.parse_arguments () in
  init_sdl ();
  let window = create_window "TerraSim" in
  set_window_icon window Assets.Sprites.daisy_00_sprite;
  run_game_loop window
