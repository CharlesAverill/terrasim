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

(** Identifiers for {!edit_screen_buttons} *)
type edit_button = Volcano | Asteroid | Examine

(** List of buttons to be drawn on the edit screen *)
let edit_screen_buttons : edit_button ui_button list ref =
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
      {
        identifier = Examine;
        bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0;
        texture_blob = Assets.Sprites.ui_examine_icon_sprite;
        initialized = false;
      };
    ]

(** Edit screen's main UI popup *)
let edit_ui_popup : popup ref =
  ref
    { bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0; initialized = false }

(** Popup for examining a tile *)
let examine_popup : popup ref =
  ref
    { bounding_box = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0; initialized = false }

let examine_popup_open = ref false

let examine_popup_data : ((int * int) * int * biome_tile * lifeform option) ref
    =
  ref ((0, 0), 0, Land Nothing, None)

let open_examine_popup ((tile_x, tile_y) : int * int) (alt : int)
    (biome : biome_tile) (lf : lifeform option) =
  examine_popup_open := true;
  pause_everything_for_popup := true;
  examine_popup_data := ((tile_x, tile_y), alt, biome, lf)

let close_examine_popup () =
  examine_popup_open := false;
  pause_everything_for_popup := false

(** Index into {!edit_screen_buttons} of selected edit screen button *)
let selected_edit_screen_button = ref 0

(** Initialize the edit screen buttons' positions and sizes
    @param pos Function from [(row, col)] to [(x, y)]
    @param w Function from [col] to [w]
    @param h Function from [col] to [h]
    @param base_col The first column of the edit screen to render in *)
let init_edit_screen_buttons (pos : int -> int -> int * int) (w : int -> int)
    (h : int -> int) (base_col : int) =
  List.iteri
    (fun i (b : edit_button ui_button) ->
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
  (* Draw the cursor *)
  Draw_cursor.draw_cursor window cursor_pos;
  let win_w, (win_h, ui_h) = get_edit_window_ui_w_h window in
  (* Initialize and draw the main UI popup *)
  if not !edit_ui_popup.initialized then
    edit_ui_popup :=
      {
        bounding_box = Sdl.Rect.create ~x:0 ~y:win_h ~w:win_w ~h:ui_h;
        initialized = true;
      };
  let bevel_w, ui_buffer, ((ui_area_x, ui_area_y), (ui_area_w, ui_area_h)) =
    draw_popup renderer !edit_ui_popup
  in
  let ui_area_h =
    ui_area_h - ui_buffer
    (* take off an extra ui_buffer to handle two rows *)
  in
  (* First column *)
  let n_columns = 8 in
  let fit = (win_w / n_columns, ui_area_h / 2) in
  let pos row col =
    let w, h = fit in
    ( ui_area_x + ui_buffer + ((w + ui_buffer) * col),
      ui_area_y + ui_buffer + (row * (h + ui_buffer)) )
  in
  (* Planet name *)
  let _ = render_text ~fit renderer (pos 0 0) "Test Planet" in
  (* Year *)
  let _ =
    render_text ~fit renderer (pos 1 0) "Year: %2d"
      (Simulation.Simulation_info.get_sim_year ())
  in
  (* Remaining columns - UI Buttons *)
  (if any_buttons_uninit !edit_screen_buttons then
     let pos row col =
       let w, h = fit in
       ( ui_area_x + ui_buffer + (((w / 2) + ui_buffer) * (col + 1)),
         ui_area_y + ui_buffer )
     in
     init_edit_screen_buttons pos
       (fun _ -> ui_area_h + ui_buffer)
       (fun _ -> ui_area_h + ui_buffer)
       1);
  render_buttons renderer !edit_screen_buttons;
  (* Selected button highlight *)
  set_render_color select_highlight_color renderer;
  draw_rect_outer_thickness renderer
    (List.nth !edit_screen_buttons !selected_edit_screen_button).bounding_box 3;
  (* Draw examine popup *)
  if !examine_popup_open then (
    (if not !examine_popup.initialized then
       let popup_buffer = (scaled_tile_w (), scaled_tile_h ()) in
       let examine_w = scaled_tile_w () * 10 in
       examine_popup :=
         {
           bounding_box =
             Sdl.Rect.create
               ~x:(win_w - examine_w - fst popup_buffer)
               ~y:(snd popup_buffer) ~w:examine_w
               ~h:(win_h - (2 * snd popup_buffer));
           initialized = true;
         });
    let _ = draw_popup ~bevel_w:(Some bevel_w) renderer !examine_popup in
    ()
  )

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
    clear_text_cache ();
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
