open Worldgrid
open Noise
open Altitude
open Biomes

let classify_ocean_tile alt =
  if alt <= deep_ocean_theshold then
    Ocean Deep
  else if alt <= regular_ocean_theshold then
    Ocean Regular
  else if alt <= shallow_ocean_theshold then
    Ocean Shallow
  else
    Land Nothing

let latitudinal_falloff (y : int) =
  let dy = (2. *. (float y /. float (world_height - 1))) -. 1. in
  1. -. (dy ** 4.)

let world_setup () =
  Random.self_init () ;
  let seed = Random.int (int_of_float (Float.pow 2. 20.)) in
  for x = 0 to world_width do
    for y = 0 to world_height do
      match get_global_tile x y [] with
      | None ->
          ()
      | Some [] ->
          let world_scale = 0.3 in
          (* Altitude *)
          let alt =
            int_of_float
              ( latitudinal_falloff y *. float max_land_height
              *. fbm ~seed ~mag_scale:7. ~x_scale:world_scale
                   ~y_scale:(world_scale /. 7.) ~octaves:3
                   ~tile_x_width:(Some world_width)
                   (float x, float y, 0.) )
          in
          (* Oceans *)
          let biome = classify_ocean_tile alt in
          let is_ocean = match biome with Ocean _ -> true | _ -> false in
          set_global_tile x y [`Altitude alt; `Biome biome]
      | _ ->
          [%unreachable]
    done
  done
