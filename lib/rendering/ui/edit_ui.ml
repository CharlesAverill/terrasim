(** Rendering logic for the Edit screen UI *)

open Tsdl
open Button
open Popup
open Text
open World.Biomes
open World.Life.Lifeform
open Cameras.Edit_camera
open Utils.Sdl_utils
open Utils.Colors
open Utils.Globals
open Edit_screen_data

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

(** Width of the main UI window's bevel *)
let ui_bevel_w = ref 0

(** Draw the edit screen UI
    @param window Application's SDL window
    @param renderer Application window's SDL renderer
    @param cursor_pos Screen-space position of cursor *)
let draw_edit_ui (window : Sdl.window) (renderer : Sdl.renderer)
    (cursor_pos : int * int) =
  let win_w, (win_h, ui_h) =
    get_edit_window_ui_w_h window !edit_ui_popup_open
  in
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
  ui_bevel_w := bevel_w;
  let ui_area_h =
    ui_area_h - ui_buffer
    (* take off an extra ui_buffer to handle two rows *)
  in
  (* First column *)
  let n_columns = 8 in
  let fit = ((win_w / n_columns, ui_area_h / 2), true) in
  let pos row col =
    let w, h = fst fit in
    ( ui_area_x + ui_buffer + ((w + ui_buffer) * col),
      ui_area_y + ui_buffer + (row * (h + ui_buffer)) )
  in
  (* Planet name *)
  let _ = render_text ~fit:(Some fit) renderer (pos 0 0) "Test Planet" in
  (* Year *)
  let _ =
    render_text ~fit:(Some fit) renderer (pos 1 0)
      (Printf.sprintf "Year: %2d" (Simulation.Simulation_info.get_sim_year ()))
  in
  (* Remaining columns - UI Buttons *)
  (if any_buttons_uninit !edit_screen_buttons then
     let pos row col =
       let w, h = fst fit in
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
    (List.nth !edit_screen_buttons !selected_edit_screen_button).bounding_box 3

(** Draw any applicable popups
    @param window The application's SDL window
    @param renderer The applciation window's SDL renderer
    @param frame_count The current frame count *)
let draw_popups (window : Sdl.window) (renderer : Sdl.renderer)
    (frame_count : int) =
  let win_w, (win_h, ui_h) =
    get_edit_window_ui_w_h window !Edit_screen_data.edit_ui_popup_open
  in
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
               ~h:((win_h - (2 * snd popup_buffer)) / 2);
           initialized = true;
         });
    (* Draw popup *)
    let _, ui_buffer, ((x, y), (ui_w, ui_h)) =
      draw_popup ~bevel_w:(Some !ui_bevel_w) renderer !examine_popup
    in
    (* Draw examine info *)
    let (tx, ty), alt, biome, lf = !examine_popup_data in
    let lat, lon = World.Grid.latlon_of_xy (tx, ty) in
    let ptsize = 60 in
    let fit = ((ui_w, ui_h / 10), false) in
    let font_family = CourierPrime in
    let (x, y), (w, h) =
      render_lines_vertical ~ptsize ~fit ~font_family renderer (2 * ui_buffer)
        (x + ui_buffer, y + (2 * ui_buffer))
        [
          Printf.sprintf "%.5f, %.5f" lat lon; Printf.sprintf "Altitude: %d" alt;
        ]
    in
    (* Draw biome tile *)
    let fit = (((5 * ui_w / 8) - (2 * ui_buffer), ui_h / 10), false) in
    let biome_tex = Edit.texture_of_tile renderer (biome, alt, frame_count) in
    let tx, ty =
      ( x + ui_w - ui_buffer - (ui_w / 3),
        y + ((h + ui_buffer + (ui_w / 3 / 2)) / 2) - (2 * ui_buffer) )
    in
    let tw, th = (ui_w / 3, ui_w / 3) in
    let* _ =
      Sdl.render_copy
        ~dst:(Sdl.Rect.create ~x:tx ~y:ty ~w:tw ~h:th)
        renderer biome_tex
    in
    (* Draw lifeform and lifeform texture *)
    match lf with
    | None ->
        ()
    | Some lf ->
        let _ =
          render_lines_vertical ~ptsize ~fit ~font_family renderer
            (2 * ui_buffer)
            (x, ty + ui_buffer)
            [
              (if biome = Land Nothing then
                 "No Biome"
               else
                 string_of_biome_tile biome);
              Printf.sprintf "%s %d/%d"
                (string_of_class lf.life_class)
                lf.species 16;
            ]
        in
        let* _ =
          Sdl.render_copy
            ~dst:(Sdl.Rect.create ~x:tx ~y:ty ~w:tw ~h:th)
            renderer
            (Edit.texture_of_lifeform renderer (lf, frame_count))
        in
        ()
  )

(** Render the edit UI onto a texture, then copy it onto the screen (mostly just
    an exercise, perf is fine)
    @param window The application's SDL window
    @param renderer The application window's SDL renderer
    @param cursor_pos Screen-space position of cursor
    @param frame_count The current frame count *)
let render_edit_ui (window : Sdl.window) (renderer : Sdl.renderer)
    (cursor_pos : int * int) (frame_count : int) =
  let* _ = Sdl.set_render_target renderer !Ui_texture.ui_texture in
  let* _ = Sdl.set_render_draw_color renderer 0 0 0 0 in
  let* _ = Sdl.render_clear renderer in
  Draw_cursor.draw_cursor window cursor_pos;
  if !Edit_screen_data.edit_ui_popup_open then (
    draw_edit_ui window renderer cursor_pos;
    draw_popups window renderer frame_count
  );
  let* _ = Sdl.set_render_target renderer None in
  let* _ = Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend in
  let* _ = Sdl.render_copy renderer (Ui_texture.get_ui_texture ()) in
  Sdl.render_present renderer
