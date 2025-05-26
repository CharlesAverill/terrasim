open Tsdl
open Sdl
open Ui
open Edit_camera
open Atlas_camera
open Cursor
open Worldgrid
open Biomes
open Spriteloader
open Globals
open Utils
open Graphics
open Gradients
open Altitude
open Globe_render.Globe_render_stubs

let animated_tile_update_factor = 8

let scale () = scaled_tile_w () / tile_sprite_w

(* Create the cache *)
let tile_texture_cache : (biome_tile * int * int, Sdl.texture) Hashtbl.t =
  Hashtbl.create 1024

(* Cached texture_of_tile function *)
let texture_of_tile renderer (t : world_tile) frame_count =
  let frame_count = frame_count / animated_tile_update_factor in
  match
    Hashtbl.find_opt tile_texture_cache (t.biome, t.altitude, frame_count)
  with
  | Some tex ->
      tex
  | None ->
      let tex =
        texture_of_blob renderer (blob_of_tile frame_count t.altitude t.biome)
      in
      Hashtbl.add tile_texture_cache (t.biome, t.altitude, frame_count) tex ;
      tex

let render_rgb_buffer renderer
    (buffer : rgb_pixel Ctypes.structure Ctypes.carray) ~win_w ~win_h =
  let open Ctypes in
  let ptr = CArray.start buffer in
  for y = 0 to win_h - 1 do
    for x = 0 to win_w - 1 do
      let i = (y * win_w) + x in
      let pixel_ptr = CArray.get buffer i in
      (* pointer to the struct *)
      let r = Unsigned.UInt8.to_int (Ctypes.getf pixel_ptr r) in
      let g = Unsigned.UInt8.to_int (Ctypes.getf pixel_ptr g) in
      let b = Unsigned.UInt8.to_int (Ctypes.getf pixel_ptr b) in
      let* () = Sdl.set_render_draw_color renderer r g b 255 in
      let rect = Sdl.Rect.create ~x ~y ~w:1 ~h:1 in
      let* () = Sdl.render_fill_rect renderer (Some rect) in
      ()
    done
  done

let render_edit window renderer frame_counter fps =
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
      match get_global_tile ~wrap_x:true gx gy with
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

module ColorSet = Set.Make (struct
  type t = int * int * int

  let compare = compare
end)

let render_atlas window renderer =
  let* win_w, win_h = get_renderer_output_size renderer in
  let altitudes = altitude () in
  let biomes = biomes () in
  let* _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
  let* _ = Sdl.render_clear renderer in
  let scale_x = float win_w /. float world_width in
  let scale_y = float win_h /. float world_height in
  (* let rects_by_color : (int * int * int, Sdl.rect list ref) Hashtbl.t =
    Hashtbl.create 32
  in *)
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  let update_rect x y w h =
    Sdl.Rect.set_x rect x ;
    Sdl.Rect.set_y rect y ;
    Sdl.Rect.set_w rect w ;
    Sdl.Rect.set_h rect h
  in
  for wy = 0 to world_height - 1 do
    for wx = 0 to world_width - 1 do
      let idx =
        (wy * world_width) + mod_wrap (wx - atlas_camera.x) world_width
      in
      let alt = Array.get altitudes idx in
      let biome = Array.get biomes idx in
      let norm_alt = clamp (float alt /. float max_land_height) 0.0 1.0 in
      let r, g, b =
        match ocean_height biome with
        | None ->
            interpolate_gradient height_gradient norm_alt
        | Some h ->
            interpolate_gradient ocean_gradient (clamp (float h /. 3.) 0. 1.)
      in
      let x = int_of_float (float wx *. scale_x) in
      let y = int_of_float (float wy *. scale_y) in
      let w =
        max 1 (int_of_float (scale_x +. 0.5))
        (* ensure at least 1px visible *)
      in
      let h = max 1 (int_of_float (scale_y +. 0.5)) in
      update_rect x y w h ;
      let* _ = Sdl.set_render_draw_color renderer r g b 255 in
      let* _ = Sdl.render_fill_rect renderer (Some rect) in
      ()
      (* let rects =
        match Hashtbl.find_opt rects_by_color (r, g, b) with
        | Some lst ->
            lst
        | None ->
            let l = ref [] in
            Hashtbl.add rects_by_color (r, g, b) l ;
            l
      in
      rects := Sdl.Rect.create ~x ~y ~w ~h :: !rects *)
    done
  done ;
  (* Hashtbl.iter
    (fun (r, g, b) rects ->
      let* _ = Sdl.set_render_draw_color renderer r g b 255 in
      let* _ = Sdl.render_fill_rects renderer !rects in
      () )
    rects_by_color ; *)
  (* 4. Draw UI on top *)
  render_ui window renderer ;
  Sdl.render_present renderer

let draw_starfield renderer =
  Random.init 42 ;
  let draw_point x y b =
    let* () = Sdl.set_render_draw_color renderer b b b 255 in
    let rect = Sdl.Rect.create ~x ~y ~w:1 ~h:1 in
    let* _ = Sdl.render_fill_rect renderer (Some rect) in
    ()
  in
  let* width, height = get_renderer_output_size renderer in
  for _ = 0 to 1000 do
    let brightness = 200 + Random.int 56 in
    let x = Random.int width in
    let y = Random.int height in
    match Random.int 3 with
    | 0 ->
        draw_point x y brightness
    | 1 ->
        let dim = brightness / 2 in
        draw_point x y brightness ;
        draw_point (x + 1) y dim ;
        draw_point (x - 1) y dim ;
        draw_point x (y + 1) dim ;
        draw_point x (y - 1) dim
    | 2 ->
        let dim = brightness / 2 in
        draw_point x y brightness ;
        draw_point (x + 1) (y + 1) dim ;
        draw_point (x - 1) (y + 1) dim ;
        draw_point (x + 1) (y - 1) dim ;
        draw_point (x - 1) (y - 1) dim
    | _ ->
        ()
  done

let render_globe window renderer =
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
      (* let open Ctypes in
      let pixel_buffer = CArray.make rgb_pixel (win_w * win_h) in
      globe_render win_w win_h lon_q lat_q world_width world_height
        (CArray.start
           (CArray.of_list int
              (Array.to_list
                 (flatten_matrix (matrix_map grid (fun t -> t.altitude)) 0) ) ) )
        max_land_height
        (CArray.start pixel_buffer) ;
      render_rgb_buffer renderer pixel_buffer ~win_w ~win_h ; *)
      let rot_lon = float_of_int lon_q *. Float.pi /. 180.0 in
      let rot_lat = float_of_int lat_q *. Float.pi /. 180.0 in
      let* _ = set_render_draw_color renderer 0 0 0 255 in
      let* _ = Sdl.render_clear renderer in
      draw_starfield renderer ;
      let cos_lat = cos rot_lat in
      let sin_lat = sin rot_lat in
      let cos_lon = cos rot_lon in
      let sin_lon = sin rot_lon in
      let altitudes = altitude () in
      let biomes = biomes () in
      let rects = ref (fun c -> []) in
      let colors = ref ColorSet.empty in
      let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
      let update_rect x y w h =
        Sdl.Rect.set_x rect x ;
        Sdl.Rect.set_y rect y ;
        Sdl.Rect.set_w rect w ;
        Sdl.Rect.set_h rect h
      in
      for dy = -radius to radius do
        for dx = -radius to radius do
          let fx = float dx /. float radius in
          let fy = -.(float dy /. float radius) in
          let r2 = (fx *. fx) +. (fy *. fy) in
          if r2 <= 1.0 then
            let fz = sqrt (1.0 -. r2) in
            let x0, y0, z0 = (fx, fy, fz) in
            (* Combined rotation matrix *)
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
            let idx = (wy * world_width) + wx in
            if idx < Array.length altitudes then (
              let alt = Array.get altitudes idx in
              let biome = Array.get biomes idx in
              let norm_alt =
                clamp (float alt /. float max_land_height) 0.0 1.0
              in
              let r, g, b =
                match ocean_height biome with
                | None ->
                    interpolate_gradient height_gradient norm_alt
                | Some h ->
                    interpolate_gradient ocean_gradient
                      (clamp (float h /. 3.) 0. 1.)
              in
              let* _ = Sdl.set_render_draw_color renderer r g b 255 in
              let dst_x = center_x + dx in
              let dst_y = center_y + dy in
              update_rect dst_x dst_y 1 1 ;
              let* _ = Sdl.render_fill_rect renderer (Some rect) in
              ()
            )
          (* colors := ColorSet.add (r, g, b) !colors ;
              rects := update !rects (r, g, b) (point :: !rects (r, g, b)) *)
        done
      done ;
      (* ColorSet.iter
        (fun (r, g, b) ->
          let* _ = Sdl.set_render_draw_color renderer r g b 255 in
          let* _ = Sdl.render_fill_rects renderer (!rects (r, g, b)) in
          () )
        !colors ; *)
      (* Store texture in cache and reset render target *)
      Hashtbl.replace globe_cache key target_texture ;
      let* _ = Sdl.set_render_target renderer None in
      let dst = Sdl.Rect.create ~x:0 ~y:0 ~w:win_w ~h:win_h in
      let* _ = Sdl.render_copy ~dst renderer target_texture in
      Sdl.render_present renderer
