open Worldgrid
open Biomes

let max_land_height = 31

let increase_tile = function
  | Ocean Deep ->
      Ocean Regular
  | Ocean Regular ->
      Ocean Shallow
  | Ocean Shallow ->
      Land (Nothing 0)
  | Land (Nothing n) when n < max_land_height ->
      Land (Nothing (n + 1))
  | _ as t ->
      t (* Other types (e.g., Arctic, etc.) are unchanged *)

let decrease_tile = function
  | Land (Nothing n) when n > 0 ->
      Land (Nothing (n - 1))
  | Land (Nothing 0) ->
      Ocean Shallow
  | Ocean Shallow ->
      Ocean Regular
  | Ocean Regular ->
      Ocean Deep
  | _ as t ->
      t

(* Parameters to control the shape of the volcano *)
let volcano_radius = 6

let volcano_peak_height = 10.0

let volcano_sigma = float_of_int volcano_radius /. 2.0

let gaussian ~x ~y ~cx ~cy ~sigma =
  let dx = float_of_int (x - cx) in
  let dy = float_of_int (y - cy) in
  let exponent = -.(((dx *. dx) +. (dy *. dy)) /. (2.0 *. sigma *. sigma)) in
  exp exponent

let raise_terrain_gaussian x y =
  for dy = -volcano_radius to volcano_radius do
    for dx = -volcano_radius to volcano_radius do
      let tx = x + dx in
      let ty = y + dy in
      let influence = gaussian ~x:tx ~y:ty ~cx:x ~cy:y ~sigma:volcano_sigma in
      let delta = int_of_float (influence *. volcano_peak_height) in
      if delta > 0 then
        match get_global_tile tx ty with
        | Some current ->
            let raised =
              match current with
              | Ocean Deep ->
                  Ocean Regular
              | Ocean Regular ->
                  Ocean Shallow
              | Ocean Shallow ->
                  Land (Nothing 0)
              | Land (Nothing n) ->
                  Land (Nothing (min 31 (n + delta)))
              | Land _ as l ->
                  l
            in
            set_global_tile tx ty raised
        | None ->
            ()
    done
  done
