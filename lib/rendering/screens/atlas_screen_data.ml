open World.Grid
(** Data for the atlas screen *)

(** Which atlas view is active *)
let atlas_view_mode : world_tile_attr_getter ref = ref `Altitude

(** True if the atlas view has shifted, reset in {!Globe} when texture cache is
    cleared *)
let atlas_view_shifted = ref false

(** Set the active atlas view *)
let shift_atlas_view_mode () =
  atlas_view_shifted := true;
  atlas_view_mode :=
    match !atlas_view_mode with
    | `Altitude ->
        `Life
    | `Life ->
        `Altitude
    | _ ->
        `Altitude
