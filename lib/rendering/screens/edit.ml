(** Rendering logic for the edit screen *)

open Utils.Standard_utils
open Utils.Globals
open Utils.Sdl_utils
open Utils.Colors
open Tsdl
open Cameras.Edit_camera
open World.Grid
open World.Biomes
open World.Life.Lifeform
open Assets.Assetloader
open Text
open Button
open Popup
open Graphics
open Edit_screen_data

(** Cache for tile textures *)
let tile_texture_cache : (biome_tile * int * int, Sdl.texture) Hashtbl.t =
  Hashtbl.create 1024

(** Look in cache for tile texture, otherwise render it

    [(biome, altitude, frame_count)] is the key to {!tile_texture_cache}
    @param renderer Application window's SDL renderer
    @param biome Biome of tile
    @param altitude Altitude of tile
    @param frame_count The current frame counter
    @return An SDL texture containing the rendered tile *)
let texture_of_tile (renderer : Sdl.renderer)
    ((biome, altitude, frame_count) : biome_tile * int * int) : Sdl.texture =
  let frame_count = frame_count / animated_tile_update_factor in
  match Hashtbl.find_opt tile_texture_cache (biome, altitude, frame_count) with
  | Some tex ->
      tex
  | None ->
      let tex =
        texture_of_blob renderer (blob_of_tile frame_count altitude biome)
      in
      Hashtbl.add tile_texture_cache (biome, altitude, frame_count) tex;
      tex

(** Cache for lifeform textures *)
let lifeform_texture_cache : (lifeform * int, Sdl.texture) Hashtbl.t =
  Hashtbl.create 1024

(** Look in cache for lifeform texture, otherwise render it

    [(lifeform, sprite_index)] is the key to {!lifeform_texture_cache}
    @param renderer Application window's SDL renderer
    @param lf The lifeform
    @param frame_count The current frame counter
    @return An SDL texture containing the rendered lifeform *)
let texture_of_lifeform (renderer : Sdl.renderer)
    ((lf, frame_count) : lifeform * int) : Sdl.texture =
  let frame_count = frame_count / (animated_tile_update_factor * 2) mod 2 in
  match Hashtbl.find_opt lifeform_texture_cache (lf, frame_count) with
  | Some tex ->
      tex
  | None ->
      let tex =
        texture_of_blob ~color_key:(Some lifeform_colorkey_rgb) renderer
          (blob_of_lifeform frame_count lf)
      in
      Hashtbl.add lifeform_texture_cache (lf, frame_count) tex;
      tex

(** Render the edit screen
    @param window Application's SDL window
    @param frame_counter The current frame counter *)
let render_edit_screen (window : Sdl.window) (frame_counter : int) =
  let renderer = get_global_renderer () in
  if !need_to_flush_edit_caches then (
    clear_texture_cache tile_texture_cache;
    clear_texture_cache lifeform_texture_cache;
    clear_texture_cache blob_cache;
    clear_text_cache ();
    need_to_flush_edit_caches := false
  );
  set_render_color black_color renderer;
  let* _ = Sdl.render_clear renderer in
  (* Get window size in pixels *)
  let window_w, (window_h, _) =
    get_edit_window_ui_w_h window !edit_ui_popup_open
  in
  tile_w := window_w / view_width ();
  tile_h := window_h / view_height ();
  (* Determine number of tiles to draw (view width/height) *)
  let tiles_x = window_w / scaled_tile_w () in
  let tiles_y = window_h / scaled_tile_h () in
  let stw, sth = (scaled_tile_w (), scaled_tile_h ()) in
  (* Render each tile relative to camera *)
  for dy = 0 to tiles_y - 1 do
    for dx = 0 to tiles_x - 1 do
      let gx = edit_camera.x + dx in
      let gy = edit_camera.y + dy in
      let dst_rect =
        Sdl.Rect.create ~x:(dx * stw) ~y:(dy * sth) ~w:stw ~h:sth
      in
      match get_grid_tile (gx, gy) [ `Altitude; `Biome; `Life ] with
      | None ->
          set_render_color (0, 0, 0) renderer;
          let* _ = Sdl.render_fill_rect renderer (Some dst_rect) in
          ()
      | Some [ `Altitude alt; `Biome b; `Life lf ] -> (
          (* Draw tile *)
          let* _ =
            Sdl.render_copy renderer
              (texture_of_tile renderer (b, alt, frame_counter))
              ~dst:dst_rect
          in
          (* Draw lifeform *)
          match lf with
          | None ->
              ()
          | Some lf ->
              let* _ =
                Sdl.render_copy renderer
                  (texture_of_lifeform renderer (lf, frame_counter))
                  ~dst:dst_rect
              in
              ())
      | _ ->
          [%unreachable]
    done
  done
