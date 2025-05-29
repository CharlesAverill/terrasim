(** Common definitions between the edit, atlas, and globe screens *)

open World.Grid
open Utils.Globals

type camera = { mutable x : int; mutable y : int }
(** Camera position in screen-space coordinates *)

(** Represents which screen is active *)
type camera_mode =
  | Edit2D of camera  (** Store the global camera *)
  | Atlas2D
  | Globe3D

(** The current camera mode *)
let current_camera_mode : camera_mode option ref = ref None
