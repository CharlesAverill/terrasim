(** Logic for handling the available species of life and their properties *)

open Assets.Sprites
open Assets.Assetloader
open Utils.Sdl_utils
open Utils.Logging

(** Life species *)
type species =
  | Amoeba
  | Amphibian
  | Bacteria
  | Bird
  | Carnifern
  | Crab
  | Dinosaur
  | Fish
  | Insect
  | Mammal
  | Octopus
  | Reptile
  | Robot
  | Starfish
  | Trichordate
  | Whale

(** Evolution stage for a species *)
type stage = Early | Middle | Late | Final

type variant = int
(** Visual species variant selector *)

type lifeform = { species : species; stage : stage; variant : variant }
(** Describes a single lifeform entity *)

(** Get the string representation of a species *)
let string_of_species : species -> string = function
  | Amoeba ->
      "Amoeba"
  | Amphibian ->
      "Amphibian"
  | Bacteria ->
      "Bacteria"
  | Bird ->
      "Bird"
  | Carnifern ->
      "Carnifern"
  | Crab ->
      "Crab"
  | Dinosaur ->
      "Dinosaur"
  | Fish ->
      "Fish"
  | Insect ->
      "Insect"
  | Mammal ->
      "Mammal"
  | Octopus ->
      "Octopus"
  | Reptile ->
      "Reptile"
  | Robot ->
      "Robot"
  | Starfish ->
      "Starfish"
  | Trichordate ->
      "Trichordate"
  | Whale ->
      "Whale"

(** Get the string representation of an evolution stage *)
let string_of_stage : stage -> string = function
  | Early ->
      "Early"
  | Middle ->
      "Middle"
  | Late ->
      "Late"
  | Final ->
      "Final"

(** Get the string representation of a species variant (for debugging) *)
let string_of_variant (v : variant) : string = Printf.sprintf "variant%d" (v + 1)

(** Get the string representation of a lifeform (for debugging) *)
let string_of_lifeform (lf : lifeform) : string =
  Printf.sprintf "{%s - %s - %s}"
    (string_of_species lf.species)
    (string_of_stage lf.stage)
    (string_of_variant lf.variant)

(** Lifeform sprite transparency color *)
let lifeform_colorkey_rgb = rgb_of_hex "20C8F8"

(** Get the sprite blob for a lifeform
    @param frame_count The current frame counter
    @param lf The lifeform to retrieve a sprite blob for
    @return
      The sprite blob associated with the lifeform's species, stage, and variant
*)
let blob_of_lifeform (frame_count : int) (lf : lifeform) =
  let expected_anim_frames = 2 in
  List.nth
    (let l =
       find_matching_sprites lifeforms_sprites
         (List.map (Printf.sprintf "/%s/")
            [
              string_of_species lf.species;
              string_of_stage lf.stage;
              string_of_variant lf.variant;
            ])
     in
     if List.length l != expected_anim_frames then
       fatal rc_Error "Found %d sprites for %s, expected %d" (List.length l)
         (string_of_lifeform lf) expected_anim_frames
     else
       l)
    frame_count
