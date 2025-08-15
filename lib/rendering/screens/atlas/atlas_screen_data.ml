(** Data for the atlas screen *)

open Tsdl
open World.Grid
open Popup

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

(** Atlas screen's main UI popup *)
let atlas_ui_window : popup ref =
  ref
    { bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0; initialized = false }

(** Popup for examining a tile *)
let examine_popup : popup ref =
  ref
    { bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0; initialized = false }
