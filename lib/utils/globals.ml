(** Global data and pseudo-data *)

(** Width and height of tiles on the edit screen in pixels *)
let tile_w, tile_h = (ref 1, ref 1)

(** Size factor for scaled tile width and heigth *)
let zoom_factor = ref 1.

(** Tile width scaled w.r.t. {!zoom_factor} *)
let scaled_tile_w () : int = int_of_float (float !tile_w *. !zoom_factor)

(** Tile height scaled w.r.t. {!zoom_factor} *)
let scaled_tile_h () : int = int_of_float (float !tile_h *. !zoom_factor)

(** Whether the cache of tile textures needs to be flushed *)
let need_to_flush_edit_tile_cache = ref false

(** Signals to flush the tile texture cache *)
let clear_edit_cache () = need_to_flush_edit_tile_cache := true
