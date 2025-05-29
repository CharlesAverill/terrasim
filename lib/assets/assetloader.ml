(** Logic for loading assets from program memory *)

open Tsdl
open Tsdl_image
open Utils.Sdl_utils
open Utils.Logging

type asset_blob = string * string
(** Asset filename and contents *)

(** Prepare an asset blob for loading with SDL
    @param blob Asset blob to load*)
let rwops_of_blob (blob : asset_blob) : Sdl.rw_ops =
  let* x = Sdl.rw_from_mem (Bytes.of_string (snd blob)) in
  x

(** Render a sprite blob to an SDL surface

    Loops until image loading succeeds

    @param blob Sprite blob to render
    @return Rendered surface *)
let rec surface_of_blob (blob : asset_blob) : Sdl.surface =
  let rw = rwops_of_blob blob in
  _log Log_Debug "Loading %s" (fst blob);
  match Image.load_rw rw true with
  | Ok surf ->
      surf
  | Error (`Msg e) ->
      _log Log_Error "Failed to load %s, trying again" (fst blob);
      surface_of_blob blob

(** A cache for sprite blobs *)
let blob_cache : (string, Sdl.texture) Hashtbl.t = Hashtbl.create 100

(** Render a sprite blob to an SDL texture, or pulling from the cache if
    possible
    @param renderer SDL renderer to render with
    @param blob Sprite blob to render with
    @return Rendered texture *)
let texture_of_blob (renderer : Sdl.renderer) (blob : asset_blob) =
  match Hashtbl.find_opt blob_cache (fst blob) with
  | Some texture ->
      texture
  | None ->
      let surface = surface_of_blob blob in
      let* texture = Sdl.create_texture_from_surface renderer surface in
      Sdl.free_surface surface;
      Hashtbl.add blob_cache (fst blob) texture;
      texture
