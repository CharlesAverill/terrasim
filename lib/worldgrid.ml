open Biomes
open Utils

let world_width = 128

let world_height = 64

type world_tile =
  { altitude: int
  ; event: unit
  ; magma: unit
  ; water_temp: unit
  ; water_current: unit
  ; air_temp: unit
  ; air_current: unit
  ; rain: unit
  ; biome: biome_tile
  ; life: unit
  ; civilization: unit }

type world_grid = world_tile array

let grid : world_grid =
  Array.make
    (world_height * world_width)
    { altitude= 0
    ; event= ()
    ; magma= ()
    ; water_temp= ()
    ; water_current= ()
    ; air_temp= ()
    ; air_current= ()
    ; rain= ()
    ; biome= Land Nothing
    ; life= ()
    ; civilization= () }

let altitude () = Array.map (fun t -> t.altitude) grid

let events () = Array.map (fun t -> t.event) grid

let magma () = Array.map (fun t -> t.magma) grid

let water_temp () = Array.map (fun t -> t.water_temp) grid

let water_current () = Array.map (fun t -> t.water_current) grid

let air_temp () = Array.map (fun t -> t.air_temp) grid

let air_current () = Array.map (fun t -> t.air_current) grid

let rain () = Array.map (fun t -> t.rain) grid

let biomes () = Array.map (fun t -> t.biome) grid

let life () = Array.map (fun t -> t.life) grid

let civilization () = Array.map (fun t -> t.civilization) grid

let get_global_tile ?(wrap_x = true) x y =
  if wrap_x then
    let x = ((x mod world_width) + world_width) mod world_width in
    if y < 0 || y >= world_height then
      None
    else
      Some grid.((y * world_width) + x)
  else if x < 0 || y < 0 || x >= world_width || y >= world_height then
    None
  else
    Some grid.((y * world_width) + x)

let set_global_tile ?(wrap_x = true) x y t =
  if wrap_x then
    let x = ((x mod world_width) + world_width) mod world_width in
    if y < 0 || y >= world_height then
      ()
    else
      grid.((y * world_width) + x) <- t
  else if x < 0 || y < 0 || x >= world_width || y >= world_height then
    ()
  else
    grid.((y * world_width) + x) <- t

let set_biome x y b =
  match get_global_tile x y with
  | None ->
      ()
  | Some t ->
      set_global_tile x y {t with biome= b}
