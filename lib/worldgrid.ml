open Biomes
open Utils

let world_width = 128

let world_height = 64

let grid = Array.make_matrix world_height world_width (Ocean Deep)

let get_global_tile x y =
  if x < 0 || y < 0 || x >= world_width || y >= world_height then
    None
  else
    Some grid.(y).(x)

let set_global_tile x y t =
  if x < 0 || y < 0 || x >= world_width || y >= world_height then
    ()
  else
    grid.(y).(x) <- t
