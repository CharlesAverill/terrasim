open Biomes
open Utils

let world_width = 256

let world_height = 128

let grid = Array.make_matrix world_height world_width (Ocean Regular)

let get_global_tile x y =
  if x < 0 || y < 0 then
    None
  else
    Some grid.(mod_wrap y world_height).(mod_wrap x world_width)

let set_global_tile x y t =
  if x < 0 || y < 0 then
    ()
  else
    grid.(mod_wrap y world_height).(mod_wrap x world_width) <- t
