open Tsdl
open Sdl

let tile_w, tile_h = (ref 1, ref 1)

let zoom = ref 1.

let scaled_tile_w ?(use_zoom : bool = true) () =
  int_of_float
    ( float_of_int !tile_w
    *.
    if use_zoom then
      !zoom
    else
      1. )

let scaled_tile_h ?(use_zoom : bool = true) () =
  int_of_float
    ( float_of_int !tile_h
    *.
    if use_zoom then
      !zoom
    else
      1. )

let rotation_lat = ref 0

let rotation_lon = ref 0

let globe_cache : (int * int, Sdl.texture) Hashtbl.t = Hashtbl.create 100

let clear_globe_cache () = Hashtbl.reset globe_cache

let need_to_flush_edit_tile_cache = ref false

let clear_edit_cache () = need_to_flush_edit_tile_cache := true
