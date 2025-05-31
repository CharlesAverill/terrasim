(** World generation *)

open Grid
open Noise
open Altitude
open Biomes
open Life.Lifeform
open Assets.Sprites
open Utils.Standard_utils

(** Generate a completely random lifeform *)
let random_lifeform () : lifeform =
  let class_list =
    [
      Eukaryote;
      Amphibian;
      Prokaryote;
      Avian;
      Carnifern;
      Arthropod;
      Dinosaur;
      Fish;
      Insect;
      Mammal;
      Mollusk;
      Reptile;
      Robot;
      Radiate;
      Trichordate;
      Cetacean;
    ]
  in
  let species = Random.int 16 in
  {
    life_class = List.nth class_list (Random.int (List.length class_list));
    species;
  }

(** Compute a coefficient that scales down as [y] grows farther from the equator
    @param y Latitude value
    @return {m 1 - (2 \cdot \frac{\texttt{y}}{height} - 1)^4 }*)
let latitudinal_falloff ?(strength : float = 1.) (y : int) =
  let dist_from_eq =
    float (abs (y - (world_height / 2))) /. float (world_height / 2)
  in
  let falloff =
    mod_float
      (Float.sin (float y *. Float.pi /. float world_height) ** strength)
      1.
  in
  let rand = Random.float dist_from_eq *. (1. -. falloff) in
  1. -. mod_float rand 1.

(** Procedurally generate world grid using {{!Noise.fbm}simplex noise} and FBM
*)
let world_setup () =
  Random.self_init ();
  let seed = Random.int (int_of_float (Float.pow 2. 20.)) in
  init_noise_gen ~seed:(Some seed) ();

  let dist_from_eq y =
    abs_float (float (y - (world_height / 2)) /. float (world_height / 2))
  in

  for x = 0 to world_width do
    for y = 0 to world_height do
      match get_grid_tile (x, y) [] with
      | None ->
          ()
      | Some [] ->
          (* let lat_falloff = latitudinal_falloff y in *)

          (* Step 1: Land mask using low-frequency noise + falloff *)
          let land_noise =
            2.
            *. (1. -. dist_from_eq y)
            *. fbm_2d ~octaves:5 ~scale_xy:(0.5, 0.5) ~contrast:2. ~base_ampl:2.
                 (x, y)
                 (world_width, world_height)
          in
          (* "Erode" land noise near poles with another layer of noise to prevent big flat edges *)
          let land_mask =
            land_noise
            -. dist_from_eq y
               *. fbm_2d ~octaves:2 (x, y) (world_width, world_height)
          in

          (* Step 2: Threshold to decide land or water *)
          let sea_level = 0.35 in
          let is_land = land_mask > sea_level in

          (* Step 3: If land, get terrain elevation using higher frequency *)
          let alt =
            let elev =
              fbm_2d ~octaves:6 ~scale_xy:(0.5, 0.5) ~contrast:2. ~base_ampl:2.
                ~base_freq:2. (x, y)
                (world_width, world_height)
            in
            if is_land then
              int_of_float
                (elev *. float (max_land_height - shallow_ocean_theshold))
              + shallow_ocean_theshold
            else
              int_of_float
                (0.5
                *. (1. -. dist_from_eq y)
                *. float shallow_ocean_theshold
                *. elev)
              + (shallow_ocean_theshold / 2)
          in
          let alt = clamp alt 0 max_land_height in

          (* Step 4: Biome classification *)
          let biome =
            if y < world_height / 20 || y > world_height - (world_height / 20)
            then
              Land Arctic
            else if not is_land then
              classify_ocean_tile alt
            else
              Land Nothing
          in

          (* Step 5: Random life generation on land *)
          let lifeform =
            if is_land && Random.int 100 < 5 then
              Some (random_lifeform ())
            else
              None
          in

          set_grid_tile (x, y) [ `Altitude alt; `Biome biome; `Life lifeform ]
      | _ ->
          [%unreachable]
    done
  done;

  shut_down_noise_gen ()
