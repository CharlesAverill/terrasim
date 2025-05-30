(** Logic for handling UI button rendering *)

open Tsdl
open Assets.Assetloader
open Utils.Sdl_utils

type 'a ui_button = {
  identifier : 'a;
      (** Assign an identifier to a button to map data defined in {!Rendering}
          to functionality in {!Controls} *)
  bounding_box : Sdl.rect;
  texture_blob : asset_blob;
  mutable initialized : bool;
}
(** UI button data *)

(** Check if two UI buttons are equal *)
let ui_button_eq (b1 : 'a ui_button) (b2 : 'a ui_button) : bool =
  b1.identifier = b2.identifier
  && Sdl.rect_equals b1.bounding_box b2.bounding_box
  && b1.texture_blob = b2.texture_blob
  && b1.initialized = b2.initialized

(** Check if a point is inside of a button
    @param x
    @param y
    @param button The button to check in
    @return Whether [(x, y)] is within [button.bounding_box] *)
let point_inside_button ((x, y) : int * int) (button : 'a ui_button) : bool =
  Sdl.point_in_rect (Sdl.Point.create ~x ~y) button.bounding_box

(** Check if any buttons are uninitialized in a list of buttons
    @param l List to search over
    @return Whether any element of [l] satisfies [not button.initialized] *)
let any_buttons_uninit (l : 'a ui_button list) : bool =
  List.exists (fun x -> not x.initialized) l

(** Render a list of buttons to the screen

    @param renderer Application window's renderer
    @param l List of buttons to render *)
let render_buttons (renderer : Sdl.renderer) (l : 'a ui_button list) =
  List.iter
    (fun b ->
      let x, y, w, h =
        ( Sdl.Rect.x b.bounding_box,
          Sdl.Rect.y b.bounding_box,
          Sdl.Rect.w b.bounding_box,
          Sdl.Rect.h b.bounding_box )
      in
      let* _ =
        Sdl.render_copy
          ~dst:(Sdl.Rect.create ~x ~y ~w ~h)
          renderer
          (texture_of_blob renderer b.texture_blob)
      in
      ())
    l
