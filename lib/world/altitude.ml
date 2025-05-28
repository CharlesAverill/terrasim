open Grid
open Biomes
open Utils.Standard_utils
open Utils.Logging

let max_land_height = 31

let deep_ocean_theshold = int_of_float (0.25 *. float max_land_height)

let regular_ocean_theshold = int_of_float (0.35 *. float max_land_height)

let shallow_ocean_theshold = int_of_float (0.5 *. float max_land_height)

let ocean_height = function
  | Ocean Deep ->
      Some 0
  | Ocean Regular ->
      Some 1
  | Ocean Shallow ->
      Some 2
  | _ ->
      None

let change_altitude x y delta =
  match get_global_tile x y [`Altitude] with
  | None ->
      ()
  | Some [`Altitude alt] ->
      let new_alt = clamp (alt + delta) 0 max_land_height in
      set_global_tile x y [`Altitude new_alt] ;
      if new_alt < deep_ocean_theshold then
        set_biome x y (Ocean Deep)
      else if new_alt < regular_ocean_theshold then
        set_biome x y (Ocean Regular)
      else if new_alt < shallow_ocean_theshold then
        set_biome x y (Ocean Shallow)
      else
        set_biome x y (Land Nothing)
  | _ ->
      [%unreachable]

let gaussian ~x ~y ~cx ~cy ~sigma =
  let dx = float_of_int (x - cx) in
  let dy = float_of_int (y - cy) in
  let exponent = -.(((dx *. dx) +. (dy *. dy)) /. (2.0 *. sigma *. sigma)) in
  exp exponent

let adjust_terrain_gaussian ?(raise = true) x y =
  (* Parameters to control the shape of the volcano *)
  let volcano_radius = 6 in
  let volcano_peak_height = 10. in
  let volcano_sigma = float_of_int volcano_radius /. 2. in
  for dy = -volcano_radius to volcano_radius do
    for dx = -volcano_radius to volcano_radius do
      let tx = x + dx in
      let ty = y + dy in
      let influence = gaussian ~x:tx ~y:ty ~cx:x ~cy:y ~sigma:volcano_sigma in
      let delta =
        ( if raise then
            1
          else
            -1 )
        * int_of_float (influence *. volcano_peak_height)
      in
      change_altitude tx ty delta
    done
  done
