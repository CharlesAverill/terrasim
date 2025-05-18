open Tsdl
open Tsdl_image
open Logging
open Utils

(* Convert string blob to a bigarray and use Sdl.rw_from_mem *)
let rwops_of_blob (blob : string * string) =
  Sdl.rw_from_mem (Bytes.of_string (snd blob))

let rec surface_of_blob blob =
  let* rw = rwops_of_blob blob in
  _log Log_Debug "Loading %s" (fst blob) ;
  match Image.load_rw rw true with
  | Ok surf ->
      surf
  | Error (`Msg e) ->
      _log Log_Error "Failed to load %s, trying again" (fst blob) ;
      surface_of_blob blob

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
