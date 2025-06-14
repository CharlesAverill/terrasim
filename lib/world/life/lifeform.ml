(** Logic for handling the available classes of life and their properties *)

open Tsdl
open Assets.Sprites
open Assets.Assetloader
open Utils.Sdl_utils
open Utils.Logging

(** Class of life *)
type life_class =
  | Eukaryote
  | Amphibian
  | Prokaryote
  | Avian
  | Carnifern
  | Arthropod
  | Dinosaur
  | Fish
  | Insect
  | Mammal
  | Mollusk
  | Reptile
  | Robot
  | Radiate
  | Trichordate
  | Cetacean

type species = int
(** Class variant selector *)

type lifeform = { life_class : life_class; species : species }
(** Describes a single lifeform entity *)

(** Get the string representation of a class *)
let string_of_class : life_class -> string = function
  | Eukaryote ->
      "Amoeba"
  | Amphibian ->
      "Amphibian"
  | Prokaryote ->
      "Bacteria"
  | Avian ->
      "Bird"
  | Carnifern ->
      "Carnifern"
  | Arthropod ->
      "Crab"
  | Dinosaur ->
      "Dinosaur"
  | Fish ->
      "Fish"
  | Insect ->
      "Insect"
  | Mammal ->
      "Mammal"
  | Mollusk ->
      "Octopus"
  | Reptile ->
      "Reptile"
  | Robot ->
      "Robot"
  | Radiate ->
      "Starfish"
  | Trichordate ->
      "Trichordate"
  | Cetacean ->
      "Whale"

(** Get the string representation of a species variant (for debugging) *)
let string_of_species (v : species) : string =
  Printf.sprintf "species_%d" (v + 1)

(** Get the string representation of a lifeform (for debugging) *)
let string_of_lifeform (lf : lifeform) : string =
  Printf.sprintf "{%s - %s}"
    (string_of_species lf.species)
    (string_of_class lf.life_class)

(** Get the sprite blob for a lifeform
    @param frame_count The current frame counter
    @param lf The lifeform to retrieve a sprite blob for
    @return
      The sprite blob associated with the lifeform's species, stage, and variant
*)
let blob_of_lifeform (frame_count : int) (lf : lifeform) : asset_blob =
  let expected_anim_frames = 2 in
  List.nth
    (let l =
       find_matching_sprites lifeforms_sprites
         (List.map (Printf.sprintf "/%s/")
            [ string_of_species lf.species; string_of_class lf.life_class ])
     in
     if List.length l != expected_anim_frames then
       fatal rc_Error "Found %d sprites for %s, expected %d" (List.length l)
         (string_of_lifeform lf) expected_anim_frames
     else
       l)
    (frame_count mod expected_anim_frames)

(** Determine the class of life that a lifeform can evolve into
    @param lf The lifeform
    @return
      [None] if the lifeform cannot evolve, otherwise its potential next class
*)
let evolution_step (lf : lifeform) : life_class option =
  match lf.life_class with
  | Prokaryote when lf.species >= 8 ->
      Some Eukaryote
  | Eukaryote when lf.species >= 12 ->
      Some Radiate
  | Radiate when lf.species < 8 ->
      Some Arthropod
  | Radiate when lf.species < 12 ->
      Some Trichordate
  | Arthropod when lf.species < 4 ->
      Some Mollusk
  | Arthropod when lf.species < 12 ->
      Some Insect
  | Mollusk when 4 <= lf.species && lf.species < 12 ->
      Some Fish
  | Fish when lf.species < 8 ->
      Some Amphibian
  | Fish when lf.species < 12 ->
      Some Trichordate
  | Cetacean when lf.species >= 12 ->
      Some Mammal
  | Amphibian when lf.species < 8 ->
      Some Reptile
  | Reptile when lf.species < 8 ->
      Some Dinosaur
  | Reptile when lf.species < 12 ->
      Some Mammal
  | Dinosaur when lf.species < 4 ->
      Some Avian
  | Dinosaur when lf.species < 8 ->
      Some Mammal
  | Mammal when 4 <= lf.species && lf.species < 12 ->
      Some Cetacean
  | _ ->
      None

let color_of_life_class (lc : life_class) =
  rgb_of_hex
    (match lc with
    | Eukaryote ->
        "#00FFAA"
    | Amphibian ->
        "#77FF77"
    | Prokaryote ->
        "#FF00FF"
    | Avian ->
        "#FFFF55"
    | Carnifern ->
        "#00AA00"
    | Arthropod ->
        "#FF5500"
    | Dinosaur ->
        "#AA00FF"
    | Fish ->
        "#00FFFF"
    | Insect ->
        "#FFAA00"
    | Mammal ->
        "#FFFFFF"
    | Mollusk ->
        "#AAAAFF"
    | Reptile ->
        "#00FF00"
    | Robot ->
        "#888888"
    | Radiate ->
        "#FF88FF"
    | Trichordate ->
        "#CC00FF"
    | Cetacean ->
        "#66CCFF")
