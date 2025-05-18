let tile_w, tile_h = (ref 1, ref 1)

let zoom = ref 1.

let scaled_tile_w () = int_of_float (float_of_int !tile_w *. !zoom)

let scaled_tile_h () = int_of_float (float_of_int !tile_h *. !zoom)

let rotation_lat = ref 0

let rotation_lon = ref 0
