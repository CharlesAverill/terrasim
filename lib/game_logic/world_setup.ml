open Worldgrid
open Noise
open Altitude
open Biomes

let classify_ocean_tile tile =
  if tile.altitude <= deep_ocean_theshold then
    Ocean Deep
  else if tile.altitude <= regular_ocean_theshold then
    Ocean Regular
  else if tile.altitude <= shallow_ocean_theshold then
    Ocean Shallow
  else
    Land Nothing

let world_setup () =
  Random.self_init () ;
  let seed = Random.int (int_of_float (Float.pow 2. 20.)) in
  for x = 0 to world_width do
    for y = 0 to world_height do
      match get_global_tile x y with
      | None ->
          ()
      | Some tile ->
          (* Altitude *)
          let tile =
            { tile with
              altitude=
                int_of_float
                  ( float max_land_height
                  *. fbm ~seed ~mag_scale:7. ~x_scale:0.25 ~y_scale:0.05
                       ~tile_x_width:(Some world_width)
                       (float x, float y, 0.) ) }
          in
          (* Oceans *)
          let tile = {tile with biome= classify_ocean_tile tile} in
          let is_ocean = match tile.biome with Ocean _ -> true | _ -> false in
          set_global_tile x y tile
    done
  done
