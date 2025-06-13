(** Logic handling world altitude *)

open Grid
open Biomes
open Utils.Standard_utils
open Utils.Logging

(** Maximum altitude *)
let max_land_height = 31

(** Altitude threshold for classifying deep oceans *)
let deep_ocean_theshold = int_of_float (0.25 *. float max_land_height)

(** Altitude threshold for classifying regular oceans *)
let regular_ocean_theshold = int_of_float (0.45 *. float max_land_height)

(** Altitude threshold for classifying shallow oceans *)
let shallow_ocean_theshold = int_of_float (0.5 *. float max_land_height)

(** Classify a tile between {!Biomes.Ocean} and {!Biomes.Land} based on altitude
    @param alt Altitude
    @return Biome tile distinguishing between land and ocean *)
let classify_ocean_tile (alt : int) : biome_tile =
  if alt <= deep_ocean_theshold then
    Ocean Deep
  else if alt <= regular_ocean_theshold then
    Ocean Regular
  else if alt <= shallow_ocean_theshold then
    Ocean Shallow
  else
    Land Nothing

(** Change the altitude of a tile
    @param x x position of tile
    @param y y position of tile
    @param delta altitude delta *)
let change_altitude (x : int) (y : int) (delta : int) =
  match get_grid_tile (x, y) [ `Altitude ] with
  | None ->
      ()
  | Some [ `Altitude alt ] ->
      let new_alt = clamp (alt + delta) 0 max_land_height in
      set_grid_tile (x, y) [ `Altitude new_alt ];
      if new_alt < deep_ocean_theshold then
        set_grid_tile (x, y) [ `Biome (Ocean Deep) ]
      else if new_alt < regular_ocean_theshold then
        set_grid_tile (x, y) [ `Biome (Ocean Regular) ]
      else if new_alt < shallow_ocean_theshold then
        set_grid_tile (x, y) [ `Biome (Ocean Shallow) ]
      else
        set_grid_tile (x, y) [ `Biome (Land Nothing) ]
  | _ ->
      [%unreachable]

(** Compute the value of a 2D Gaussian distribution centered at [(cx, cy)],
    evaluated at [(x, y)]
    @param x
    @param y
    @param cx
    @param cy
    @param sigma Distribution standard deviation
    @return Distribution sample at [(x, y)] *)
let gaussian ~(x : int) ~(y : int) ~(cx : int) ~(cy : int) ~(sigma : float) :
    float =
  let dx = float (x - cx) in
  let dy = float (y - cy) in
  let exponent = -.(((dx *. dx) +. (dy *. dy)) /. (2.0 *. sigma *. sigma)) in
  exp exponent

(** Adjust the altitude around a tile at [(x, y)] according to a
    {{!gaussian}gaussian distribution}

    @param raise If true, then raise, else lower altitude
    @param x
    @param y *)
let adjust_terrain_gaussian ?(raise = true) (x : int) (y : int) =
  let center_alt =
    match get_grid_tile (x, y) [ `Altitude ] with
    | Some [ `Altitude alt ] ->
        alt
    | _ ->
        [%unreachable]
  in
  let volcano_radius = 6 in
  let volcano_peak_height = 4. in
  let volcano_sigma = float volcano_radius /. 2. in
  for dy = -volcano_radius to volcano_radius do
    for dx = -volcano_radius to volcano_radius do
      let tx = x + dx in
      let ty = y + dy in
      let influence = gaussian ~x:tx ~y:ty ~cx:x ~cy:y ~sigma:volcano_sigma in
      let delta =
        (if raise then
           1
         else
           -1)
        * int_of_float (influence *. volcano_peak_height)
      in
      match get_grid_tile (tx, ty) [ `Altitude ] with
      | Some [ `Altitude alt ] ->
          let cmp =
            if raise then
              ( <= )
            else
              ( >= )
          in
          if cmp alt center_alt then change_altitude tx ty delta
      | _ ->
          ()
    done
  done
