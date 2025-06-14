(** Data for the edit screen *)

open Tsdl
open World.Grid
open World.Biomes
open World.Life.Lifeform
open Popup

(** How many frames it takes to draw the next frame of an animated tile *)
let animated_tile_update_factor = 8

(** Whether the edit UI is visible *)
let edit_ui_popup_open = ref true

(** Toggle [edit_ui_popup_open] *)
let toggle_edit_ui_popup () = edit_ui_popup_open := not !edit_ui_popup_open

(** Whether the examine popup is visible *)
let examine_popup_open = ref false

(** Information shown in the examine popup *)
let examine_popup_data : ((int * int) * int * biome_tile * lifeform option) ref
    =
  ref ((0, 0), 0, Land Nothing, None)

(** Edit screen's main UI popup *)
let edit_ui_popup : popup ref =
  ref
    { bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0; initialized = false }

(** Popup for examining a tile *)
let examine_popup : popup ref =
  ref
    { bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0; initialized = false }

(** Open the examination popup
    @param tile_x Examined tile's x coordinate
    @param tile_y Examined tile's y coordinate
    @param alt Examined tile's altitude
    @param biome Examined tile's biome
    @param lf Examined file's lifeform *)
let open_examine_popup ((tile_x, tile_y) : int * int) (alt : int)
    (biome : biome_tile) (lf : lifeform option) =
  examine_popup_open := true;
  pause_everything_for_popup := true;
  examine_popup_data := ((tile_x, tile_y), alt, biome, lf)

(** Close the examination popup *)
let close_examine_popup () =
  examine_popup_open := false;
  pause_everything_for_popup := false
