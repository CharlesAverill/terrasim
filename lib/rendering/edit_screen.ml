(** Rendering logic for the edit screen *)

open Utils.Standard_utils
open Utils.Globals
open Utils.Sdl_utils
open Tsdl
open Tsdl_ttf
open Cameras.Edit_camera
open World.Grid
open World.Biomes
open World.Life.Lifeform
open Assets.Assetloader
open Graphics

(** How many frames it takes to draw the next frame of an animated tile *)
let animated_tile_update_factor = 8

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

let ui_bg_color = rgb_of_hex "DDDDDD"
let ui_bevel_light_color = rgb_of_hex "EEEEEE"
let ui_bevel_dark_color = rgb_of_hex "CCCCCC"
let black_color = rgb_of_hex "000000"

(** Draw the edit screen UI
    @param window Application's SDL window
    @param renderer Application window's SDL renderer
    @param cursor_pos Screen-space position of cursor *)
let draw_edit_ui (window : Sdl.window) (renderer : Sdl.renderer)
    (cursor_pos : int * int) =
  Draw_cursor.draw_cursor window cursor_pos;
  let win_w, (win_h, ui_h) = get_edit_window_ui_w_h window in
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
      (let x =
         Assets.Assetloader.rwops_of_blob Assets.Fonts._NewPortLand_npl_font
       in
       x)
      1 ptsize
  in
  let* text_surf =
    Ttf.render_text_blended font
      (Printf.sprintf "Year: %d" (Simulation.Simulation_info.get_sim_year ()))
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

(** Render the edit screen
    @param window Application's SDL window
    @param frame_counter The current frame counter
    @param cursor_pos Screen-space position of the cursor *)
let render_edit_screen (window : Sdl.window) (frame_counter : int)
    (cursor_pos : int * int) =
  let renderer = get_global_renderer () in
  if !need_to_flush_edit_caches then (
    Hashtbl.clear tile_texture_cache;
    Hashtbl.clear lifeform_texture_cache;
    Hashtbl.clear blob_cache;
    need_to_flush_edit_caches := false
  );
  set_render_color black_color renderer;
  let* _ = Sdl.render_clear renderer in
  (* 1. Get window size in pixels *)
  let window_w, (window_h, _) = get_edit_window_ui_w_h window in
  tile_w := window_w / view_width ();
  tile_h := window_h / view_height ();
  (* 2. Determine number of tiles to draw (view width/height) *)
  let tiles_x = window_w / scaled_tile_w () in
  let tiles_y = window_h / scaled_tile_h () in
  let stw, sth = (scaled_tile_w (), scaled_tile_h ()) in
  (* 3. Render each tile relative to camera *)
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
  done;
  (* 4. Draw UI on top *)
  draw_edit_ui window renderer cursor_pos;
  (* 5. Show result *)
  Sdl.render_present renderer
