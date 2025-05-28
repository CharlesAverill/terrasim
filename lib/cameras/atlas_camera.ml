open Camera
open Tsdl
open Sdl
open Utils
open World.Grid

let atlas_camera : camera = { x = 0; y = 0 }
let move_atlas_camera dx = atlas_camera.x <- atlas_camera.x + dx

(* Edge panning *)
let pan_margin = 32

let pan_atlas_camera_if_needed window =
  let mouse_x, mouse_y = Sdl.get_mouse_state () |> snd in
  let win_w, win_h = Sdl.get_window_size window in
  if mouse_x < pan_margin then
    move_atlas_camera 1
  else if mouse_x > win_w - pan_margin then
    move_atlas_camera (-1)
