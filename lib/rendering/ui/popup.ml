(** Facilities for drawing popup windows *)

open Tsdl
open Utils.Sdl_utils
open Utils.Colors

type popup = { bounding_box : Sdl.rect; initialized : bool }

(** Whether to pause simulation and input to render popup and wait for user to
    click out *)
let pause_everything_for_popup = ref false

(** Draw a popup
    @param bg_color Background color
    @param dark_bevel_color Color of the bottom/right edges of the bevel
    @param light_bevel_color Color of the top/left edges of the bevel
    @param bevel_w
      Thickness of the bevel if set, otherwise auto-computed based on [w]
    @param renderer Application window's SDL renderer
    @param x
    @param y
    @param w
    @param h
    @return
      [(bevel_w, ui_buffer, (ui_position, ui_dimensions))] where [ui_buffer] is
      a computed value indicating suggested distance between UI area boundaries
      and UI elements *)
let draw_popup ?(bg_color : int * int * int = ui_bg_color)
    ?(dark_bevel_color : int * int * int = ui_bevel_dark_color)
    ?(light_bevel_color : int * int * int = ui_bevel_light_color)
    ?(bevel_w : int option = None) (renderer : Sdl.renderer) (popup : popup) :
    int * int * ((int * int) * (int * int)) =
  let x, y, w, h = get_rect_bounding_box popup.bounding_box in
  let bevel_w = match bevel_w with None -> w / 100 | Some x -> x in
  (* Draw bg *)
  let ui_bg_rect = Sdl.Rect.create ~x ~y ~w ~h in
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
        (x, y);
        (x, y + h);
        (x + bevel_w, y + h - bevel_w);
        (x, y);
        (x + bevel_w, y);
        (x + bevel_w, y + h - bevel_w);
        (* Top edge*)
        (x, y);
        (x + w, y);
        (x + w - bevel_w, y + bevel_w);
        (x, y);
        (x, y + bevel_w);
        (x + w - bevel_w, y + bevel_w);
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
        (x + w, y);
        (x + w, y + h);
        (x + w - bevel_w, y + h - bevel_w);
        (x + w, y);
        (x + w - bevel_w, y + bevel_w);
        (x + w - bevel_w, y + h - bevel_w);
        (* Bottom edge*)
        (x, y + h);
        (x + w, y + h);
        (x + w, y + h - bevel_w);
        (x, y + h);
        (x + bevel_w, y + h - bevel_w);
        (x + w, y + h - bevel_w);
      ]
  in
  let* _ =
    Sdl.render_geometry renderer (light_bevel_verts @ dark_bevel_verts)
  in
  let ui_buffer = bevel_w / 3 in
  let ui_area_x = x + bevel_w in
  let ui_area_y = y + bevel_w in
  let ui_area_w = w - (2 * bevel_w) - (2 * ui_buffer) in
  let ui_area_h = h - (2 * bevel_w) - (2 * ui_buffer) in
  (bevel_w, ui_buffer, ((ui_area_x, ui_area_y), (ui_area_w, ui_area_h)))

(** Check if a point is inside a popup
    @param x
    @param y
    @param p Popup to check inside of *)
let point_inside_popup ((x, y) : int * int) (p : popup) : bool =
  Sdl.point_in_rect (Sdl.Point.create ~x ~y) p.bounding_box
