(** World generation *)

open Grid
open Noise
open Altitude
open Biomes
open Life.Lifeform
open Assets.Sprites

(** Generate a completely random lifeform *)
let random_lifeform () : lifeform =
  let spec_list =
    [
      Amoeba;
      Amphibian;
      Bacteria;
      Bird;
      Carnifern;
      Crab;
      Dinosaur;
      Fish;
      Insect;
      Mammal;
      Octopus;
      Reptile;
      Robot;
      Starfish;
      Trichordate;
      Whale;
    ]
  in
  let level_list = [ Early; Middle; Late; Final ] in
  let variant = Random.int 4 in
  {
    species = List.nth spec_list (Random.int (List.length spec_list));
    stage = List.nth level_list (Random.int (List.length level_list));
    variant;
  }

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

(** Compute a coefficient that scales down as [y] grows farther from the equator
    @param y Latitude value
    @return {m 1 - (2 \cdot \frac{\texttt{y}}{height - 1} - 1)^4 }*)
let latitudinal_falloff (y : int) =
  let dy = (2. *. (float y /. float (world_height - 1))) -. 1. in
  1. -. (dy ** 4.)

(** Procedurally generate world grid using {{!Noise.fbm}simplex noise} and FBM
*)
let world_setup () =
  Random.self_init ();
  let seed = Random.int (int_of_float (Float.pow 2. 20.)) in
  for x = 0 to world_width do
    for y = 0 to world_height do
      match get_grid_tile (x, y) [] with
      | None ->
          ()
      | Some [] ->
          let world_scale = 0.3 in
          (* Altitude *)
          let alt =
            int_of_float
              (latitudinal_falloff y *. float max_land_height
              *. fbm ~seed ~mag_scale:7. ~x_scale:world_scale
                   ~y_scale:(world_scale /. 7.) ~octaves:3
                   ~tile_x_width:(Some world_width)
                   (float x, float y, 0.))
          in
          (* Oceans *)
          let biome = classify_ocean_tile alt in
          let _is_ocean = match biome with Ocean _ -> true | _ -> false in
          (* TEMPORARY - life generation *)
          let lifeform =
            match Random.int 100 with
            | x when x < 5 ->
                Some (random_lifeform ())
            | _ ->
                None
          in
          set_grid_tile (x, y) [ `Altitude alt; `Biome biome; `Life lifeform ]
      | _ ->
          [%unreachable]
    done
  done
