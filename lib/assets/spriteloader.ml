open Tsdl
open Tsdl_image
open Logging

let ( let* ) r f =
  match r with Ok x -> f x | Error (`Msg e) -> fatal rc_SDL "%s" e

(* Convert string blob to a bigarray and use Sdl.rw_from_mem *)
let rwops_of_blob (blob : string) = Sdl.rw_from_mem (Bytes.of_string blob)

let surface_of_blob blob =
  let* rw = rwops_of_blob blob in
  let* surface = Image.load_rw rw true in
  surface

(* Load the image and create an SDL texture *)
let texture_of_blob renderer blob =
  let* rw = rwops_of_blob blob in
  let surface = surface_of_blob blob in
  match Sdl.create_texture_from_surface renderer surface with
  | Error (`Msg e) ->
      Sdl.free_surface surface ;
      failwith ("Create texture failed: " ^ e)
  | Ok texture ->
      Sdl.free_surface surface ; texture
