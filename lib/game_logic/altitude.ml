open Worldgrid
open Biomes
open Utils
open Logging

let max_land_height = 31

let change_altitude x y delta =
  match get_global_tile x y with
  | None ->
      ()
  | Some t ->
      set_global_tile x y
        {t with altitude= clamp (t.altitude + delta) 0 max_land_height}

let gaussian ~x ~y ~cx ~cy ~sigma =
  let dx = float_of_int (x - cx) in
  let dy = float_of_int (y - cy) in
  let exponent = -.(((dx *. dx) +. (dy *. dy)) /. (2.0 *. sigma *. sigma)) in
  exp exponent

let raise_terrain_gaussian x y =
  (* Parameters to control the shape of the volcano *)
  let volcano_radius = 6 in
  let volcano_peak_height = 10. in
  let volcano_sigma = float_of_int volcano_radius /. 2. in
  for dy = -volcano_radius to volcano_radius do
    for dx = -volcano_radius to volcano_radius do
      let tx = x + dx in
      let ty = y + dy in
      let influence = gaussian ~x:tx ~y:ty ~cx:x ~cy:y ~sigma:volcano_sigma in
      let delta = int_of_float (influence *. volcano_peak_height) in
      change_altitude tx ty delta
    done
  done
