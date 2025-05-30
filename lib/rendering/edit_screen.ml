(** Rendering logic for the edit screen *)

open Utils.Standard_utils
open Utils.Globals
open Utils.Sdl_utils
open Tsdl
open Cameras.Edit_camera
open World.Grid
open World.Biomes
open World.Life.Lifeform
open Assets.Assetloader
open Graphics
open Text
open Ui_button

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

type edit_button = Volcano | Asteroid

let edit_screen_buttons =
  ref
    [
      {
        identifier = Volcano;
        bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0;
        texture_blob = Assets.Sprites.events_volcano2_sprite;
        initialized = false;
      };
      {
        identifier = Asteroid;
        bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0;
        texture_blob = Assets.Sprites.events_meteor1_sprite;
        initialized = false;
      };
    ]

let selected_edit_screen_button = ref 0

let init_edit_screen_buttons (pos : int -> int -> int * int) (w : int -> int)
    (h : int -> int) base_col =
  List.iteri
    (fun i b ->
      let x, y = pos 0 (base_col + i) in
      Sdl.Rect.set_x b.bounding_box x;
      Sdl.Rect.set_y b.bounding_box y;
      Sdl.Rect.set_w b.bounding_box (w i);
      Sdl.Rect.set_h b.bounding_box (h i);
      b.initialized <- true)
    !edit_screen_buttons

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
  let ui_area_h = ui_h - ((2 * bevel_w) + (3 * ui_buffer)) in
  (* First column*)
  let n_columns = 8 in
  let fit = (win_w / n_columns, ui_area_h / 2) in
  let pos row col =
    let w, h = fit in
    ( bevel_w + ui_buffer + ((w + ui_buffer) * col),
      win_h + bevel_w + ui_buffer + (row * (h + ui_buffer)) )
  in
  (* Planet name *)
  let _ = render_text ~fit renderer (pos 0 0) "Test Planet" in
  (* Year *)
  let _ =
    render_text ~fit renderer (pos 1 0) "Year: %2d"
      (Simulation.Simulation_info.get_sim_year ())
  in
  (* UI Buttons *)
  (if any_buttons_uninit !edit_screen_buttons then
     let pos row col =
       let w, h = fit in
       ( bevel_w + ui_buffer + (((w / 2) + ui_buffer) * (col + 1)),
         win_h + bevel_w + ui_buffer )
     in
     init_edit_screen_buttons pos
       (fun _ -> ui_area_h + ui_buffer)
       (fun _ -> ui_area_h + ui_buffer)
       1);
  render_buttons renderer !edit_screen_buttons;
  (* Selected button highlight *)
  set_render_color (rgb_of_hex "FFDE64") renderer;
  draw_rect_outer_thickness renderer
    (List.nth !edit_screen_buttons !selected_edit_screen_button).bounding_box 3

(** Render the edit screen
    @param window Application's SDL window
    @param frame_counter The current frame counter
    @param cursor_pos Screen-space position of the cursor *)
let render_edit_screen (window : Sdl.window) (frame_counter : int)
    (cursor_pos : int * int) =
  let renderer = get_global_renderer () in
  if !need_to_flush_edit_caches then (
    clear_texture_cache tile_texture_cache;
    clear_texture_cache lifeform_texture_cache;
    clear_texture_cache blob_cache;
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
