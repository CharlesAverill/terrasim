open World.Grid
(** Data for the atlas screen *)

(** Which atlas view is active *)
let atlas_view_mode : world_tile_attr_getter ref = ref `Altitude

(** Set the active atlas view *)
let set_atlas_view_mode (mode : world_tile_attr_getter) =
  atlas_view_mode := mode
