open Biomes
open Utils

let world_width = 128

let world_height = 64

type world_tile_attr_getters =
  [ `Altitude
  | `Event
  | `Magma
  | `WaterTemp
  | `AirTemp
  | `AirCurrent
  | `Rain
  | `Biome
  | `Life
  | `Civilization ]

type world_tile_attr_setters =
  [ `AirCurrent of unit
  | `AirTemp of unit
  | `Altitude of int
  | `Biome of biome_tile
  | `Civilization of unit
  | `Event of unit
  | `Life of unit
  | `Magma of unit
  | `Rain of unit
  | `WaterCurrent of unit
  | `WaterTemp of unit ]

type world_grid =
  { altitude: int array
  ; event: unit array
  ; magma: unit array
  ; water_temp: unit array
  ; water_current: unit array
  ; air_temp: unit array
  ; air_current: unit array
  ; rain: unit array
  ; biome: biome_tile array
  ; life: unit array
  ; civilization: unit array }

let grid : world_grid =
  let size = world_height * world_width in
  { altitude= Array.make size 0
  ; event= Array.make size ()
  ; magma= Array.make size ()
  ; water_temp= Array.make size ()
  ; water_current= Array.make size ()
  ; air_temp= Array.make size ()
  ; air_current= Array.make size ()
  ; rain= Array.make size ()
  ; biome= Array.make size (Land Nothing)
  ; life= Array.make size ()
  ; civilization= Array.make size () }

let get_grid_attr i = function
  | `Altitude ->
      `Altitude grid.altitude.(i)
  | `Event ->
      `Event grid.event.(i)
  | `Magma ->
      `Magma grid.magma.(i)
  | `WaterTemp ->
      `WaterTemp grid.water_temp.(i)
  | `WaterCurrent ->
      `WaterCurrent grid.water_current.(i)
  | `AirTemp ->
      `AirTemp grid.air_temp.(i)
  | `AirCurrent ->
      `AirCurrent grid.air_current.(i)
  | `Rain ->
      `Rain grid.rain.(i)
  | `Biome ->
      `Biome grid.biome.(i)
  | `Life ->
      `Life grid.life.(i)
  | `Civilization ->
      `Civilization grid.civilization.(i)

let set_grid_attr i = function
  | `Altitude x ->
      grid.altitude.(i) <- x
  | `Event x ->
      grid.event.(i) <- x
  | `Magma x ->
      grid.magma.(i) <- x
  | `WaterTemp x ->
      grid.water_temp.(i) <- x
  | `WaterCurrent x ->
      grid.water_current.(i) <- x
  | `AirTemp x ->
      grid.air_temp.(i) <- x
  | `AirCurrent x ->
      grid.air_current.(i) <- x
  | `Rain x ->
      grid.rain.(i) <- x
  | `Biome x ->
      grid.biome.(i) <- x
  | `Life x ->
      grid.life.(i) <- x
  | `Civilization x ->
      grid.civilization.(i) <- x

let get_global_tile ?(wrap_x = true) x y fields =
  let x =
    if wrap_x then
      ((x mod world_width) + world_width) mod world_width
    else
      x
  in
  if x < 0 || y < 0 || x >= world_width || y >= world_height then
    None
  else
    Some (List.map (get_grid_attr ((y * world_width) + x)) fields)

let set_global_tile ?(wrap_x = true) x y fields =
  let x =
    if wrap_x then
      ((x mod world_width) + world_width) mod world_width
    else
      x
  in
  if x < 0 || y < 0 || x >= world_width || y >= world_height then
    ()
  else
    List.iter (set_grid_attr ((y * world_width) + x)) fields

let set_biome x y b =
  match get_global_tile x y [`Biome] with
  | None ->
      ()
  | Some _ ->
      set_global_tile x y [`Biome b]
