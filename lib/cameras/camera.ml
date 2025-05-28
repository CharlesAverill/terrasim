open World.Grid
open Utils.Globals

(* Screen-space coordinates *)
type camera = {mutable x: int; mutable y: int}

type camera_mode = Edit2D of camera | Atlas2D | Globe3D

let current_camera_mode : camera_mode option ref = ref None
