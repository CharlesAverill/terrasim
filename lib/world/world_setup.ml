(** World generation *)

open Grid
open Noise
open Altitude
open Biomes
open Life.Lifeform
open Assets.Sprites

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
    @return {m 1 - (2 \cdot \frac{\texttt{y}}{height - 1} - 1)^4 }*)
let latitudinal_falloff (y : int) =
  let dy = (2. *. (float y /. float (world_height - 1))) -. 1. in
  1. -. (dy ** 8.)

(** Procedurally generate world grid using {{!Noise.fbm}simplex noise} and FBM
*)
let world_setup () =
  Random.self_init ();
  let seed = Random.int (int_of_float (Float.pow 2. 20.)) in
  init_noise_gen ~seed:(Some seed) ();
  for x = 0 to world_width do
    for y = 0 to world_height do
      match get_grid_tile (x, y) [] with
      | None ->
          ()
      | Some [] ->
          (* Altitude *)
          let alt =
            int_of_float
              (latitudinal_falloff y *. float max_land_height
              *. fbm_2d ~contrast:1.5 ~scale_xy:(0.55, 0.55) ~base_ampl:2. (x, y)
                   (world_width, world_height))
          in
          (* Oceans *)
          let biome = classify_ocean_tile alt in
          let _is_ocean = match biome with Ocean _ -> true | _ -> false in
          (* Ice caps *)
          let biome =
            if y < world_height / 20 || y > world_height - (world_height / 20)
            then
              Land Arctic
            else
              biome
          in
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
  done;
  shut_down_noise_gen ()
