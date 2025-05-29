(** Data for the globe screen *)

open Tsdl
open Sdl

(** Latitudinal rotation position of globe in degrees *)
let rotation_lat = ref 0.

(** Longitudinal rotation position of globe in degrees *)
let rotation_lon = ref 0.

(** Latitudinal velocity of globe in degrees per frame *)
let velocity_lat = ref 0.0

(** Minimum longitudinal velocity of globe in degrees per frame *)
let min_abs_velocity_lon = 0.25

(** Longitudinal velocity of globe in degrees per frame *)
let velocity_lon = ref min_abs_velocity_lon

(** Maximum globe spin speed in degrees per frame *)
let max_globe_speed = 16.

(** Decay of rotational velocity in degrees per frame per frame *)
let globe_spin_friction = 0.0125

(** Whether the globe is being held in place by the user *)
let globe_pinned = ref false

(** Last mouse recorded position of the mouse while pinning the globe *)
let globe_last_mouse_pos : (int * int) option ref = ref None
