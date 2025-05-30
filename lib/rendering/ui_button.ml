(** Logic for handling UI button rendering *)

open Tsdl
open Assets.Assetloader
open Utils.Sdl_utils

type 'a ui_button = {
  identifier : 'a;
  bounding_box : Sdl.rect;
  texture_blob : asset_blob;
  mutable initialized : bool;
}
(** UI button data *)

let ui_button_eq b1 b2 =
  b1.identifier = b2.identifier
  && Sdl.rect_equals b1.bounding_box b2.bounding_box
  && b1.texture_blob = b2.texture_blob
  && b1.initialized = b2.initialized

let inside_button ((x, y) : int * int) (button : 'a ui_button) : bool =
  Sdl.point_in_rect (Sdl.Point.create ~x ~y) button.bounding_box

let any_buttons_uninit (l : 'a ui_button list) : bool =
  List.fold_left ( || ) false (List.map (fun x -> not x.initialized) l)

let render_buttons (renderer : Sdl.renderer) (l : 'a ui_button list) : unit =
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
