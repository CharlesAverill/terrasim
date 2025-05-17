open Tsdl
open Sdl
open Ui
open Camera
open Cursor
open Worldgrid
open Biomes
open Spriteloader
open Globals

let animated_tile_update_factor = 16

(* Create the cache *)
let tile_texture_cache : (tile * int, Sdl.texture) Hashtbl.t = Hashtbl.create 64

(* Cached texture_of_tile function *)
let texture_of_tile renderer (t : tile) frame_count =
  let frame_count = frame_count / animated_tile_update_factor in
  match Hashtbl.find_opt tile_texture_cache (t, frame_count) with
  | Some tex ->
      tex
  | None ->
      let tex = texture_of_blob renderer (blob_of_tile frame_count t) in
      Hashtbl.add tile_texture_cache (t, frame_count) tex ;
      tex

let do_render window renderer frame_counter fps =
  ignore (Sdl.render_clear renderer) ;
  (* 1. Get window size in pixels *)
  let window_w, window_h =
    match Sdl.get_window_size window with w, h -> (w, h)
  in
  tile_w := window_w / view_width ;
  tile_h := window_h / view_height ;
  (* 2. Determine number of tiles to draw (view width/height) *)
  let tiles_x = window_w / !tile_w in
  let tiles_y = window_h / !tile_h in
  (* 3. Render each tile relative to camera *)
  for dy = 0 to tiles_y - 1 do
    for dx = 0 to tiles_x - 1 do
      let gx = (current_camera.x + dx) mod world_width in
      let gy = (current_camera.y + dy) mod world_height in
      let tile = grid.(gy).(gx) in
      let dst_rect =
        Sdl.Rect.create ~x:(dx * !tile_w) ~y:(dy * !tile_h) ~w:!tile_w
          ~h:!tile_h
      in
      ignore
        (Sdl.render_copy renderer
           (texture_of_tile renderer tile frame_counter)
           ~dst:dst_rect )
    done
  done ;
  (* 4. Draw UI on top *)
  render_ui renderer ;
  (* 5. Show result *)
  Sdl.render_present renderer
