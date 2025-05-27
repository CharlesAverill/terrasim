open Utils
open Tsdl
open Sdl
open Globals
open Edit_camera
open Worldgrid
open Biomes
open Spriteloader
open Ui
open Graphics

let animated_tile_update_factor = 8

(* Create the cache *)
let tile_texture_cache : (biome_tile * int * int, Sdl.texture) Hashtbl.t =
  Hashtbl.create 1024

(* Cached texture_of_tile function *)
let texture_of_tile renderer (biome, altitude) frame_count =
  let frame_count = frame_count / animated_tile_update_factor in
  match Hashtbl.find_opt tile_texture_cache (biome, altitude, frame_count) with
  | Some tex ->
      tex
  | None ->
      let tex =
        texture_of_blob renderer (blob_of_tile frame_count altitude biome)
      in
      Hashtbl.add tile_texture_cache (biome, altitude, frame_count) tex ;
      tex

let render_edit window frame_counter fps =
  let renderer = get_global_renderer () in
  if !need_to_flush_edit_tile_cache then (
    Hashtbl.clear tile_texture_cache ;
    Hashtbl.clear blob_cache ;
    need_to_flush_edit_tile_cache := false
  ) ;
  let* _ = Sdl.render_clear renderer in
  (* 1. Get window size in pixels *)
  let window_w, window_h =
    match Sdl.get_window_size window with w, h -> (w, h)
  in
  tile_w := window_w / view_width () ;
  tile_h := window_h / view_height () ;
  (* 2. Determine number of tiles to draw (view width/height) *)
  let tiles_x = window_w / scaled_tile_w () in
  let tiles_y = window_h / scaled_tile_h () in
  (* 3. Render each tile relative to camera *)
  for dy = 0 to tiles_y - 1 do
    for dx = 0 to tiles_x - 1 do
      let gx = edit_camera.x + dx in
      let gy = edit_camera.y + dy in
      let dst_rect =
        Sdl.Rect.create
          ~x:(dx * scaled_tile_w ())
          ~y:(dy * scaled_tile_h ())
          ~w:(scaled_tile_w ()) ~h:(scaled_tile_h ())
      in
      match get_global_tile gx gy [`Altitude; `Biome] with
      | None ->
          let* _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
          let* _ = Sdl.render_fill_rect renderer (Some dst_rect) in
          ()
      | Some [`Altitude alt; `Biome b] ->
          let* _ =
            Sdl.render_copy renderer
              (texture_of_tile renderer (b, alt) frame_counter)
              ~dst:dst_rect
          in
          ()
      | _ ->
          [%unreachable]
    done
  done ;
  (* 4. Draw UI on top *)
  render_ui window renderer ;
  (* 5. Show result *)
  Sdl.render_present renderer
