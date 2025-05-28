open Tsdl
open Tsdl_image
open Utils.Standard_utils
open Utils.Logging

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
let blob_cache : (string, Sdl.texture) Hashtbl.t = Hashtbl.create 100

let texture_of_blob renderer blob =
  let digest = Digestif.SHA1.digest_string (fst blob) |> Digestif.SHA1.to_hex in
  match Hashtbl.find_opt blob_cache digest with
  | Some texture ->
      texture
  | None ->
      let* rw = rwops_of_blob blob in
      let surface = surface_of_blob blob in
      let* texture = Sdl.create_texture_from_surface renderer surface in
      Sdl.free_surface surface ;
      Hashtbl.add blob_cache digest texture ;
      texture
