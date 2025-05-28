open Utils.Standard_utils
open Utils.Globals
open Utils.Sdl_utils
open Tsdl
open Tsdl_ttf
open Cameras.Edit_camera
open World.Grid
open World.Biomes
open Assets.Spriteloader
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
      Hashtbl.add tile_texture_cache (biome, altitude, frame_count) tex;
      tex

let ui_bg_color = rgb_of_hex "DDDDDD"
let ui_bevel_light_color = rgb_of_hex "EEEEEE"
let ui_bevel_dark_color = rgb_of_hex "CCCCCC"
let black_color = rgb_of_hex "000000"

let draw_edit_ui window renderer cursor_pos =
  Draw_cursor.draw_cursor window cursor_pos;
  let win_w, (win_h, ui_h) = get_edit_window_ui_height window in
  let bevel_w = win_w / 100 in
  let ui_buffer = bevel_w / 3 in
  (* Draw bg *)
  let ui_bg_rect = Sdl.Rect.create ~x:0 ~y:win_h ~w:win_w ~h:ui_h in
  set_render_color ui_bg_color renderer;
  let* _ = Sdl.render_fill_rect renderer (Some ui_bg_rect) in
  ();
  (* Draw bevels *)
  let light_bevel_verts =
    List.map
      (fun (x, y) ->
        let x, y = (float x, float y) in
        Sdl.Vertex.create ~position:(Sdl.Fpoint.create ~x ~y)
          ~color:(sdlcolor_of_tuple ui_bevel_light_color)
          ~tex_coord:(Sdl.Fpoint.create ~x:0. ~y:0.))
      [
        (* Left edge *)
        (0, win_h);
        (0, win_h + ui_h);
        (bevel_w, win_h + ui_h - bevel_w);
        (0, win_h);
        (bevel_w, win_h);
        (bevel_w, win_h + ui_h - bevel_w);
        (* Top edge*)
        (0, win_h);
        (win_w, win_h);
        (win_w - bevel_w, win_h + bevel_w);
        (0, win_h);
        (0, win_h + bevel_w);
        (win_w - bevel_w, win_h + bevel_w);
      ]
  in
  let dark_bevel_verts =
    List.map
      (fun (x, y) ->
        let x, y = (float x, float y) in
        Sdl.Vertex.create ~position:(Sdl.Fpoint.create ~x ~y)
          ~color:(sdlcolor_of_tuple ui_bevel_dark_color)
          ~tex_coord:(Sdl.Fpoint.create ~x:0. ~y:0.))
      [
        (* Right edge *)
        (win_w, win_h);
        (win_w, win_h + ui_h);
        (win_w - bevel_w, win_h + ui_h - bevel_w);
        (win_w, win_h);
        (win_w - bevel_w, win_h + bevel_w);
        (win_w - bevel_w, win_h + ui_h - bevel_w);
        (* Bottom edge*)
        (0, win_h + ui_h);
        (win_w, win_h + ui_h);
        (win_w, win_h + ui_h - bevel_w);
        (0, win_h + ui_h);
        (bevel_w, win_h + ui_h - bevel_w);
        (win_w, win_h + ui_h - bevel_w);
      ]
  in
  let* _ =
    Sdl.render_geometry renderer (light_bevel_verts @ dark_bevel_verts)
  in
  (* Load font *)
  let ptsize = 48 in
  let* font =
    Ttf.open_font_rw
      (let* x =
         Assets.Spriteloader.rwops_of_blob Assets.Fonts._NewPortLand_npl_font
       in
       x)
      1 ptsize
  in
  let* text_surf =
    Ttf.render_text_blended font
      (Printf.sprintf "Year: %d" !Simulation.Simulation_info.sim_year)
      (sdlcolor_of_tuple black_color)
  in
  let* text_texture = Sdl.create_texture_from_surface renderer text_surf in
  let* _, _, (text_w, text_h) = Sdl.query_texture text_texture in
  let text_loc =
    Sdl.Rect.create ~x:(bevel_w + ui_buffer) ~y:(win_h + bevel_w) ~w:text_w
      ~h:text_h
  in
  let* _ = Sdl.render_copy ~dst:text_loc renderer text_texture in
  ()

let render_edit window frame_counter fps cursor_pos =
  let renderer = get_global_renderer () in
  if !need_to_flush_edit_tile_cache then (
    Hashtbl.clear tile_texture_cache;
    Hashtbl.clear blob_cache;
    need_to_flush_edit_tile_cache := false
  );
  set_render_color black_color renderer;
  let* _ = Sdl.render_clear renderer in
  (* 1. Get window size in pixels *)
  let window_w, (window_h, _) = get_edit_window_ui_height window in
  tile_w := window_w / view_width ();
  tile_h := window_h / view_height ();
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
      match get_global_tile gx gy [ `Altitude; `Biome ] with
      | None ->
          let* _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
          let* _ = Sdl.render_fill_rect renderer (Some dst_rect) in
          ()
      | Some [ `Altitude alt; `Biome b ] ->
          let* _ =
            Sdl.render_copy renderer
              (texture_of_tile renderer (b, alt) frame_counter)
              ~dst:dst_rect
          in
          ()
      | _ ->
          [%unreachable]
    done
  done;
  (* 4. Draw UI on top *)
  draw_edit_ui window renderer cursor_pos;
  (* 5. Show result *)
  Sdl.render_present renderer
