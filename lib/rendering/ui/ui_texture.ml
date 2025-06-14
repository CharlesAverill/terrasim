(** Handling the UI texture that is drawn over other screens *)

open Tsdl
open Utils.Sdl_utils
open Utils.Logging

let ui_texture = ref None
let ui_window = ref None
let ui_renderer = ref None

let get_ui_window () =
  match !ui_window with
  | Some x ->
      x
  | None ->
      fatal rc_Error "Tried to get UI window before it was created"

let get_ui_texture () : Sdl.texture =
  match !ui_texture with
  | Some x ->
      x
  | None ->
      fatal rc_Error "Tried to get UI texture before it was created"

let get_ui_renderer () : Sdl.renderer =
  match !ui_renderer with
  | Some x ->
      x
  | None ->
      fatal rc_Error "Tried to get UI texture before it was created"

let create_ui_texture (window : Sdl.window) (renderer : Sdl.renderer) =
  (match !ui_renderer with None -> () | Some r -> Sdl.destroy_renderer r);
  (match !ui_texture with None -> () | Some t -> Sdl.destroy_texture t);
  let width, height = Sdl.get_window_size window in
  let* tex =
    Sdl.create_texture renderer Sdl.Pixel.format_argb8888
      Sdl.Texture.access_target ~w:width ~h:height
  in
  let* _ = Sdl.set_texture_blend_mode tex Sdl.Blend.mode_blend in
  ui_texture := Some tex;
  ui_window := Some window;
  ui_renderer := Some renderer

let destroy_ui_texture () =
  Sdl.destroy_texture (get_ui_texture ());
  Sdl.destroy_renderer (get_ui_renderer ());
  ui_texture := None
