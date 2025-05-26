open Utils
open Tsdl
open Sdl
open Worldgrid
open Atlas_camera
open Altitude
open Gradients
open Ui

let render_atlas window renderer =
  let* win_w, win_h = get_renderer_output_size renderer in
  let altitudes = altitude () in
  let biomes = biomes () in
  let* _ = Sdl.set_render_draw_color renderer 0 0 0 255 in
  let* _ = Sdl.render_clear renderer in
  let scale_x = float win_w /. float world_width in
  let scale_y = float win_h /. float world_height in
  (* let rects_by_color : (int * int * int, Sdl.rect list ref) Hashtbl.t =
    Hashtbl.create 32
  in *)
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  let update_rect x y w h =
    Sdl.Rect.set_x rect x ;
    Sdl.Rect.set_y rect y ;
    Sdl.Rect.set_w rect w ;
    Sdl.Rect.set_h rect h
  in
  for wy = 0 to world_height - 1 do
    for wx = 0 to world_width - 1 do
      let idx =
        (wy * world_width) + mod_wrap (wx - atlas_camera.x) world_width
      in
      let alt = Array.get altitudes idx in
      let biome = Array.get biomes idx in
      let norm_alt = clamp (float alt /. float max_land_height) 0.0 1.0 in
      let r, g, b =
        match ocean_height biome with
        | None ->
            interpolate_gradient height_gradient norm_alt
        | Some h ->
            interpolate_gradient ocean_gradient (clamp (float h /. 3.) 0. 1.)
      in
      let x = int_of_float (float wx *. scale_x) in
      let y = int_of_float (float wy *. scale_y) in
      let w =
        max 1 (int_of_float (scale_x +. 0.5))
        (* ensure at least 1px visible *)
      in
      let h = max 1 (int_of_float (scale_y +. 0.5)) in
      update_rect x y w h ;
      let* _ = Sdl.set_render_draw_color renderer r g b 255 in
      let* _ = Sdl.render_fill_rect renderer (Some rect) in
      ()
      (* let rects =
        match Hashtbl.find_opt rects_by_color (r, g, b) with
        | Some lst ->
            lst
        | None ->
            let l = ref [] in
            Hashtbl.add rects_by_color (r, g, b) l ;
            l
      in
      rects := Sdl.Rect.create ~x ~y ~w ~h :: !rects *)
    done
  done ;
  (* Hashtbl.iter
    (fun (r, g, b) rects ->
      let* _ = Sdl.set_render_draw_color renderer r g b 255 in
      let* _ = Sdl.render_fill_rects renderer !rects in
      () )
    rects_by_color ; *)
  (* 4. Draw UI on top *)
  render_ui window renderer ;
  Sdl.render_present renderer
