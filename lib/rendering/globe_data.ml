open Tsdl
open Sdl

let rotation_lat = ref 0.

let rotation_lon = ref 0.

let velocity_lat = ref 0.0 (* Degrees per frame *)

let min_abs_velocity_lon = 0.25

let velocity_lon = ref min_abs_velocity_lon (* Always moving east *)

let flick_globe_speed = 16.

let globe_spin_friction = 0.0125

let globe_pinned = ref false

let globe_last_mouse_pos : (int * int) option ref = ref None

let globe_cache : (int * int, Sdl.texture) Hashtbl.t = Hashtbl.create 100

let clear_globe_cache () = Hashtbl.reset globe_cache

let need_to_flush_opengl_globe_cache = ref false

let clear_opengl_globe_cache () = need_to_flush_opengl_globe_cache := true
