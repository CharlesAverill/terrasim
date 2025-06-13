(** Text rendering utilities via SDL-ttf *)

open Tsdl
open Tsdl_ttf
open Assets.Assetloader
open Assets.Fonts
open Utils.Sdl_utils
open Utils.Colors
open Utils.Logging

(** Font families included in build *)
type font_family = CourierPrime | Jacquard12 | NewPortLand

type text_style = Regular | Bold | Italic | BoldItalic

(** Get a font's {!asset_blob} given its family and style *)
let font_blob_of_font_style (f : font_family) (s : text_style) : asset_blob =
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

(** A cache for rendered string textures *)
let text_cache :
    ( font_family * text_style * int * (int * int * int) * string,
      Sdl.texture * Ttf.font )
    Hashtbl.t =
  Hashtbl.create 100

(** Clear {!text_cache} and destroy its textures *)
let clear_text_cache () =
  Seq.iter
    (fun k ->
      let t, f = Hashtbl.find text_cache k in
      Sdl.destroy_texture t)
    (Hashtbl.to_seq_keys text_cache);
  Hashtbl.clear text_cache

let real_font_size font s =
  let above_padding = Ttf.font_line_skip font - Ttf.font_ascent font - 2 in
  let below_padding = -1 - Ttf.font_descent font in
  let* w, h = Ttf.size_text font s in
  (w, h - above_padding - below_padding)

let rec search_for_biggest open_font_with_size text_to_render (max_w, max_h) lo
    hi best =
  if lo > hi then
    best
  else
    let mid = (lo + hi) / 2 in
    match open_font_with_size mid with
    | Error _ ->
        search_for_biggest open_font_with_size text_to_render (max_w, max_h) lo
          (mid - 1) best
    | Ok f -> (
        match real_font_size f text_to_render with
        | w, h ->
            if w <= max_w && h <= max_h then
              search_for_biggest open_font_with_size text_to_render
                (max_w, max_h) (mid + 1) hi
                (Some (f, mid))
            else (
              Ttf.close_font f;
              search_for_biggest open_font_with_size text_to_render
                (max_w, max_h) lo (mid - 1) best
            ))

(** Render a formatted string to the screen

    @param font_family
    @param style
    @param ptsize Point size of text to render
    @param color Color of text to render
    @param fit
      A bounding box, and whether the text should be scaled up to fit to the
      bounding box ([true]) or whether the text should be scaled down to fit to
      the bounding box ([false]) in the case that it is too large
    @param draw_bounding_box
      For debugging, indicates to draw a red bounding box around the rendered
      texture
    @param renderer Application's SDL renderer
    @param x
    @param y
    @param fmt Format string
    @param ... Format string arguments
    @return
      The x, y, height, and width of the rendered texture, clipping invisiblie
      padding caused by a font's ascent and descent *)
let render_text ?(font_family : font_family = NewPortLand)
    ?(style : text_style = Regular) ?(ptsize : int = 48)
    ?(color : int * int * int = color_black)
    ?(fit : ((int * int) * bool) option = None)
    ?(draw_bounding_box : bool = false) (renderer : Sdl.renderer)
    ((x, y) : int * int) (text_to_render : string) =
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
        let font, final_size =
          match fit with
          | None ->
              let* f = open_font_with_size ptsize in
              (f, ptsize)
          | Some ((max_w, max_h), scale_up) ->
              if scale_up then
                match
                  search_for_biggest open_font_with_size text_to_render
                    (max_w, max_h) 4 256 None
                with
                | None ->
                    fatal rc_Error "Failed to find fitting font for string: %s"
                      text_to_render
                | Some x ->
                    x
              else
                let ptsize = ref ptsize in
                let font =
                  ref
                    (let* x = open_font_with_size !ptsize in
                     x)
                in
                while
                  let w, h = real_font_size !font text_to_render in
                  w > max_w || h > max_h
                do
                  ptsize := !ptsize - 1;
                  font :=
                    let* x = open_font_with_size !ptsize in
                    x
                done;
                (!font, !ptsize)
        in

        let* text_surf =
          Ttf.render_text_blended font text_to_render (sdlcolor_of_tuple color)
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

  let rendered_text_w, rendered_text_h = real_font_size font text_to_render in

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
  ((x, y), (rendered_text_w, rendered_text_h))

let render_lines_vertical ?(font_family : font_family = NewPortLand)
    ?(style : text_style = Regular) ?(ptsize : int = 48)
    ?(color : int * int * int = color_black)
    ?(fit : ((int * int) * bool) option) ?(draw_bounding_box : bool = false)
    (renderer : Sdl.renderer) (spacing : int) ((x, y) : int * int) strings =
  fst
    (List.fold_left
       (fun (((x, y), (w, h)), top) s ->
         ( render_text ~font_family ~style ~ptsize ~color ~fit
             ~draw_bounding_box renderer
             ( x,
               y + h
               +
               if top then
                 0
               else
                 spacing )
             s,
           false ))
       (((x, y), (0, 0)), true)
       strings)
