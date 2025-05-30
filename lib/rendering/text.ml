(** Text rendering utilities via SDL-ttf *)

open Tsdl
open Tsdl_ttf
open Assets.Fonts
open Utils.Sdl_utils

type font = CourierPrime | Jacquard12 | NewPortLand
type text_style = Regular | Bold | Italic | BoldItalic

let font_blob_of_font_style f s =
  match f with
  | NewPortLand ->
      _NewPortLand_npl_font
  | CourierPrime -> (
      match s with
      | Regular ->
          _CourierPrime_CourierPrime_Regular_font
      | Bold ->
          _CourierPrime_CourierPrime_Bold_font
      | Italic ->
          _CourierPrime_CourierPrime_Italic_font
      | BoldItalic ->
          _CourierPrime_CourierPrime_BoldItalic_font)
  | Jacquard12 ->
      _Jacquard12_Jacquard12_Regular_font

let black_color = rgb_of_hex "000000"

let text_cache :
    ( font * text_style * int * (int * int * int) * string,
      Sdl.texture * Ttf.font )
    Hashtbl.t =
  Hashtbl.create 100

let render_text ?(font_family : font = NewPortLand)
    ?(style : text_style = Regular) ?(ptsize : int = 48)
    ?(color : int * int * int = black_color) ?(fit : (int * int) option)
    ?(draw_bounding_box : bool = false) (renderer : Sdl.renderer)
    ((x, y) : int * int) fmt =
  let real_font_size font s =
    let above_padding = Ttf.font_line_skip font - Ttf.font_ascent font - 2 in
    let below_padding = -1 - Ttf.font_descent font in
    let* w, h = Ttf.size_text font s in
    (w, h - above_padding - below_padding)
  in
  Printf.ksprintf
    (fun text_to_render ->
      let text_texture, font =
        match
          Hashtbl.find_opt text_cache
            (font_family, style, ptsize, color, text_to_render)
        with
        | None ->
            let open_font_with_size size =
              Ttf.open_font_rw
                (Assets.Assetloader.rwops_of_blob
                   (font_blob_of_font_style font_family style))
                1 size
            in

            (* Binary search for largest size that fits *)
            let* font, final_size =
              match fit with
              | None ->
                  let* f = open_font_with_size ptsize in
                  Ok (f, ptsize)
              | Some (max_w, max_h) -> (
                  let rec search lo hi best =
                    if lo > hi then
                      best
                    else
                      let mid = (lo + hi) / 2 in
                      match open_font_with_size mid with
                      | Error _ ->
                          search lo (mid - 1) best
                      | Ok f -> (
                          match real_font_size f text_to_render with
                          | w, h ->
                              if w <= max_w && h <= max_h then
                                search (mid + 1) hi (Some (f, mid))
                              else (
                                Ttf.close_font f;
                                search lo (mid - 1) best
                              ))
                  in
                  match search 4 256 None with
                  | Some res ->
                      Ok res
                  | None ->
                      Error (`Msg "No fitting font size found"))
            in

            let* text_surf =
              Ttf.render_text_blended font text_to_render
                (sdlcolor_of_tuple color)
            in
            (* Ttf.close_font font; *)
            let* text_texture =
              Sdl.create_texture_from_surface renderer text_surf
            in
            Hashtbl.add text_cache
              (font_family, style, ptsize, color, text_to_render)
              (text_texture, font);
            (text_texture, font)
        | Some tf ->
            tf
      in
      let* text_w, text_h = Ttf.size_utf8 font text_to_render in

      let rendered_text_w, rendered_text_h =
        real_font_size font text_to_render
      in

      let above_padding = Ttf.font_line_skip font - Ttf.font_ascent font - 1 in
      let text_loc =
        Sdl.Rect.create ~x ~y:(y - above_padding) ~w:text_w ~h:text_h
      in

      let* _ = Sdl.render_copy ~dst:text_loc renderer text_texture in
      (* Sdl.destroy_texture text_texture; *)
      if draw_bounding_box then (
        set_render_color (255, 0, 0) renderer;
        let* _ =
          Sdl.render_draw_rect renderer
            (Some (Sdl.Rect.create ~x ~y ~w:rendered_text_w ~h:rendered_text_h))
        in
        ()
      );
      (rendered_text_w, rendered_text_h))
    fmt
