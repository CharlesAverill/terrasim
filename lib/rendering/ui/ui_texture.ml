(** Handling the UI texture that is drawn over other screens *)

open Tsdl
open Graphics
open Utils.Sdl_utils
open Utils.Logging

let ui_texture = ref None

let create_ui_texture (window : Sdl.window) =
  let width, height = Sdl.get_window_size window in
  let* tex =
    Sdl.create_texture (get_global_renderer ()) Sdl.Pixel.format_argb8888
      Sdl.Texture.access_target ~w:width ~h:height
  in
  let* _ = Sdl.set_texture_blend_mode tex Sdl.Blend.mode_blend in
  ui_texture := Some tex

let get_ui_texture () : Sdl.texture =
  match !ui_texture with
  | Some x ->
      x
  | None ->
      fatal rc_Error "Tried to get UI texture before it was created"
