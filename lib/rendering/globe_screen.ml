open Utils.Standard_utils
open Utils.Globals
open Tsdl
open World.Grid
open World.Altitude
open Gradients
open Graphics
open Globe_data

let draw_starfield renderer =
  Random.init 42;
  let draw_point x y b =
    let* () = Sdl.set_render_draw_color renderer b b b 255 in
    let rect = Sdl.Rect.create ~x ~y ~w:1 ~h:1 in
    let* _ = Sdl.render_fill_rect renderer (Some rect) in
    ()
  in
  let* width, height = Sdl.get_renderer_output_size renderer in
  for _ = 0 to 1000 do
    let brightness = 200 + Random.int 56 in
    let x = Random.int width in
    let y = Random.int height in
    match Random.int 3 with
    | 0 ->
        draw_point x y brightness
    | 1 ->
        let dim = brightness / 2 in
        draw_point x y brightness;
        draw_point (x + 1) y dim;
        draw_point (x - 1) y dim;
        draw_point x (y + 1) dim;
        draw_point x (y - 1) dim
    | 2 ->
        let dim = brightness / 2 in
        draw_point x y brightness;
        draw_point (x + 1) (y + 1) dim;
        draw_point (x - 1) (y + 1) dim;
        draw_point (x + 1) (y - 1) dim;
        draw_point (x - 1) (y - 1) dim
    | _ ->
        ()
  done

let render_globe window =
  let renderer = get_global_renderer () in
  let lon_q = !rotation_lon in
  let lat_q = !rotation_lat in
  let key = (int_of_float lon_q, int_of_float lat_q) in
  match Hashtbl.find_opt globe_cache key with
  | Some cached_texture ->
      let* win_w, win_h = Sdl.get_renderer_output_size renderer in
      let dst_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:win_w ~h:win_h in
      let* _ = Sdl.render_copy ~dst:dst_rect renderer cached_texture in
      Sdl.render_present renderer
  | None ->
      let* win_w, win_h = Sdl.get_renderer_output_size renderer in
      let* target_texture =
        Sdl.create_texture renderer Sdl.Pixel.format_rgba8888
          Sdl.Texture.access_target ~w:win_w ~h:win_h
      in
      let* _ = Sdl.set_render_target renderer (Some target_texture) in
      let radius = int_of_float (float win_h /. 2.2) in
      let center_x = win_w / 2 in
      let center_y = win_h / 2 in
      let rot_lon = lon_q *. Float.pi /. 180.0 in
      let rot_lat = lat_q *. Float.pi /. 180.0 in
      let* _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
      let* _ = Sdl.render_clear renderer in
      draw_starfield renderer;
      let cos_lat = cos rot_lat in
      let sin_lat = sin rot_lat in
      let cos_lon = cos rot_lon in
      let sin_lon = sin rot_lon in
      let altitudes = grid.altitude in
      let biomes = grid.biome in
      let rects = ref (fun c -> []) in
      (* let colors = ref ColorSet.empty in *)
      let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
      let update_rect x y w h =
        Sdl.Rect.set_x rect x;
        Sdl.Rect.set_y rect y;
        Sdl.Rect.set_w rect w;
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
              update_rect dst_x dst_y 1 1;
              let* _ = Sdl.render_fill_rect renderer (Some rect) in
              ()
            )
        done
      done;
      (* Store texture in cache and reset render target *)
      Hashtbl.replace globe_cache key target_texture;
      let* _ = Sdl.set_render_target renderer None in
      let dst = Sdl.Rect.create ~x:0 ~y:0 ~w:win_w ~h:win_h in
      let* _ = Sdl.render_copy ~dst renderer target_texture in
      Sdl.render_present renderer
