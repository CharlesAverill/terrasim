(** Logic for loading assets from program memory *)

open Tsdl
open Tsdl_image
open Utils.Standard_utils
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

    @param color_key
      An optional RGB tuple denoting what color to color key as transparent
    @param blob Sprite blob to render
    @return Rendered surface *)
let rec surface_of_blob ?(color_key : (int * int * int) option = None)
    (blob : asset_blob) : Sdl.surface =
  let rw = rwops_of_blob blob in
  _log Log_Debug "Loading %s" (fst blob);
  match Image.load_rw rw true with
  | Ok surf -> (
      match color_key with
      | None ->
          surf
      | Some (r, g, b) ->
          let pf_enum = Sdl.get_surface_format_enum surf in
          let* pf = Sdl.alloc_format pf_enum in
          let ck = Sdl.map_rgb pf r g b in
          Sdl.free_format pf;
          let* _ = Sdl.set_color_key surf true ck in
          surf)
  | Error (`Msg e) ->
      _log Log_Error "Failed to load %s, trying again" (fst blob);
      surface_of_blob ~color_key blob

(** A cache for sprite blobs *)
let blob_cache : (string, Sdl.texture) Hashtbl.t = Hashtbl.create 100

(** Render a sprite blob to an SDL texture, or pulling from the cache if
    possible

    @param color_key
      An optional RGB tuple denoting what color to color key as transparent
    @param renderer SDL renderer to render with
    @param blob Sprite blob to render with
    @return Rendered texture *)
let texture_of_blob ?(color_key : (int * int * int) option = None)
    (renderer : Sdl.renderer) (blob : asset_blob) =
  match Hashtbl.find_opt blob_cache (fst blob) with
  | Some texture ->
      texture
  | None ->
      let surface = surface_of_blob ~color_key blob in
      let* texture = Sdl.create_texture_from_surface renderer surface in
      Sdl.free_surface surface;
      Hashtbl.add blob_cache (fst blob) texture;
      texture

(** Performs a case-insensitive keyword search on the filenames of a list of
    {!asset_blob}s and returns the matching elements
    @param blob_list List of blobs to search in the filnames of
    @param keywords Keywords to search for
    @return
      A list of {!asset_blob}s from [blob_list] whose filenames contain
      everything in [keywords] in some order *)
let find_matching_sprites (blob_list : (string * string) list)
    (keywords : string list) : asset_blob list =
  List.fold_left
    (fun a b ->
      if
        List.for_all
          (fun kw ->
            contains
              (String.lowercase_ascii (fst b))
              (String.lowercase_ascii kw))
          keywords
      then
        b :: a
      else
        a)
    [] blob_list
