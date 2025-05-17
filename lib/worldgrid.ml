open Biomes

let world_width = 256

let world_height = 128

let grid = Array.make_matrix world_height world_width (Ocean Regular)

let get_global_tile x y = grid.(y mod world_height).(x mod world_width)

let set_global_tile x y t = grid.(y mod world_height).(x mod world_width) <- t
