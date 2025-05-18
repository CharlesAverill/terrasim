open Tsdl
open Sdl
open Ui
open Camera
open Cursor
open Worldgrid
open Biomes
open Spriteloader
open Globals
open Utils
open Graphics

let animated_tile_update_factor = 16

let scale () = scaled_tile_w () / tile_sprite_w

(* Create the cache *)
let tile_texture_cache : (tile * int, Sdl.texture) Hashtbl.t =
  Hashtbl.create 1024

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

let render_atlas window renderer frame_counter fps =
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
      let gx = atlas_camera.x + dx in
      let gy = atlas_camera.y + dy in
      let dst_rect =
        Sdl.Rect.create
          ~x:(dx * scaled_tile_w ())
          ~y:(dy * scaled_tile_h ())
          ~w:(scaled_tile_w ()) ~h:(scaled_tile_h ())
      in
      match get_global_tile gx gy with
      | None ->
          let* _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
          let* _ = Sdl.render_fill_rect renderer (Some dst_rect) in
          ()
      | Some tile ->
          let* _ =
            Sdl.render_copy renderer
              (texture_of_tile renderer tile frame_counter)
              ~dst:dst_rect
          in
          ()
    done
  done ;
  (* 4. Draw UI on top *)
  render_ui window renderer ;
  (* 5. Show result *)
  Sdl.render_present renderer

type vertex =
  {x: float; y: float; u: float; v: float; r: int; g: int; b: int; a: int}

open Domainslib

let render_globe window renderer frame_counter fps =
  let lon_q = !rotation_lon in
  let lat_q = !rotation_lat in
  let key = (lon_q, lat_q) in
  match Hashtbl.find_opt globe_cache key with
  | Some cached_texture ->
      let* win_w, win_h = get_renderer_output_size renderer in
      let dst_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:win_w ~h:win_h in
      let* _ = Sdl.render_copy ~dst:dst_rect renderer cached_texture in
      Sdl.render_present renderer
  | None ->
      let* win_w, win_h = get_renderer_output_size renderer in
      let* target_texture =
        Sdl.create_texture renderer Sdl.Pixel.format_rgba8888
          Sdl.Texture.access_target ~w:win_w ~h:win_h
      in
      let* _ = Sdl.set_render_target renderer (Some target_texture) in
      let radius = int_of_float (float win_h /. 2.2) in
      let center_x = win_w / 2 in
      let center_y = win_h / 2 in
      let rot_lon = float_of_int lon_q *. Float.pi /. 180.0 in
      let rot_lat = float_of_int lat_q *. Float.pi /. 180.0 in
      let* _ = set_render_draw_color renderer 0 0 0 255 in
      let* _ = Sdl.render_clear renderer in
      let cos_lat = cos rot_lat in
      let sin_lat = sin rot_lat in
      let cos_lon = cos rot_lon in
      let sin_lon = sin rot_lon in
      let tile_draws : (tile, Sdl.rect list) Hashtbl.t = Hashtbl.create 103 in
      let mutex = Mutex.create () in
      (* Use a parallel task pool *)
      let pool =
        Task.setup_pool ~num_domains:(Domain.recommended_domain_count () - 1) ()
      in
      Task.run pool (fun () ->
          Task.parallel_for pool ~start:(-radius) ~finish:radius
            ~body:(fun dy ->
              let local_map = Hashtbl.create 17 in
              for dx = -radius to radius do
                let fx = float dx /. float radius in
                let fy = -.(float dy /. float radius) in
                let r2 = (fx *. fx) +. (fy *. fy) in
                if r2 <= 1.0 then
                  let fz = sqrt (1.0 -. r2) in
                  let x0, y0, z0 = (fx, fy, fz) in
                  let x2 =
                    (x0 *. cos_lon)
                    +. (y0 *. sin_lat *. sin_lon)
                    +. (z0 *. cos_lat *. sin_lon)
                  in
                  let y2 = (y0 *. cos_lat) -. (z0 *. sin_lat) in
                  let z2 =
                    (-.x0 *. sin_lon)
                    +. (y0 *. sin_lat *. cos_lon)
                    +. (z0 *. cos_lat *. cos_lon)
                  in
                  let lon = atan2 x2 z2 in
                  let lat = asin (-.y2) in
                  let x_frac = (lon /. (2.0 *. Float.pi)) +. 0.5 in
                  let y_frac = (lat /. Float.pi) +. 0.5 in
                  let wx = int_of_float (x_frac *. float world_width) in
                  let wy = int_of_float (y_frac *. float world_height) in
                  match get_global_tile wx wy with
                  | Some tile ->
                      let dst =
                        Sdl.Rect.create ~x:(center_x + dx) ~y:(center_y + dy)
                          ~w:1 ~h:1
                      in
                      let rects =
                        match Hashtbl.find_opt local_map tile with
                        | Some r ->
                            dst :: r
                        | None ->
                            [dst]
                      in
                      Hashtbl.replace local_map tile rects
                  | None ->
                      ()
              done ;
              (* Merge local_map into shared tile_draws *)
              Mutex.lock mutex ;
              Hashtbl.iter
                (fun tile rects ->
                  let existing =
                    match Hashtbl.find_opt tile_draws tile with
                    | Some lst ->
                        lst
                    | None ->
                        []
                  in
                  Hashtbl.replace tile_draws tile (rects @ existing) )
                local_map ;
              Mutex.unlock mutex ) ) ;
      Task.teardown_pool pool ;
      (* Draw all tile batches *)
      Hashtbl.iter
        (fun tile (rects : rect list) ->
          let texture = texture_of_tile renderer tile frame_counter in
          let vertices =
            List.concat_map
              (fun (dst : rect) ->
                let x : float = float dst.x in
                let y : float = float dst.y in
                let w : int = dst.w in
                let h : int = dst.h in
                let top_left =
                  Sdl.Vertex.create ~position:(Fpoint.create ~x ~y) ~color:(255, 255, 255, 255)
                    ~tex_coords:(0.0, 0.0)
                in
                let top_right =
                  Sdl.Vertex.create
                    ~position:(x +. w, y)
                    ~color:(255, 255, 255, 255) ~tex_coords:(1.0, 0.0)
                in
                let bottom_right =
                  Sdl.Vertex.create
                    ~position:(x +. w, y +. h)
                    ~color:(255, 255, 255, 255) ~tex_coords:(1.0, 1.0)
                in
                let bottom_left =
                  Sdl.Vertex.create
                    ~position:(x, y +. h)
                    ~color:(255, 255, 255, 255) ~tex_coords:(0.0, 1.0)
                in
                (* Return two triangles per quad *)
                [ top_left
                ; top_right
                ; bottom_right
                ; top_left
                ; bottom_right
                ; bottom_left ] )
              rects
          in
          let* _ = Sdl.render_geometry ~texture renderer vertices in ())
        tile_draws ;
      (* Cache the rendered view *)
      Hashtbl.replace globe_cache key target_texture ;
      let* _ = Sdl.set_render_target renderer None in
      let dst = Sdl.Rect.create ~x:0 ~y:0 ~w:win_w ~h:win_h in
      let* _ = Sdl.render_copy ~dst renderer target_texture in
      Sdl.render_present renderer
