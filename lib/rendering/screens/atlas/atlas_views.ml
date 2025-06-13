(** Rendering logic for each atlas view *)

open World.Grid
open World.Altitude
open World.Biomes
open Utils.Standard_utils
open Gradients
open Cameras.Atlas_camera

(** Fill in offset and color arrays for the altitude view
    @param use_atlas_camera Whether rendering to camera or texture
    @param scale_x
    @param scale_y
    @param offsets Array of NDC coordinates for each tile
    @param colors Array of colors for each tile *)
let render_altitude (use_atlas_camera : bool)
    ((scale_x, scale_y) : float * float) (offsets : (float * float) array)
    (colors : (float * float * float) array) =
  let altitudes = grid.altitude in
  let biomes = grid.biome in
  let idx = ref 0 in
  Array.iter2
    (fun alt biome ->
      let norm_alt = clamp (float alt /. float max_land_height) 0.0 1.0 in
      let r, g, b =
        match biome with
        | Land x when x <> Arctic ->
            interpolate_gradient height_gradient norm_alt
        | Ocean _ | Land Arctic ->
            interpolate_gradient ocean_gradient
              (clamp (float alt) 0. (float shallow_ocean_theshold))
        | _ ->
            [%unreachable]
      in
      let wx, wy = (!idx mod world_width, !idx / world_width) in
      let wx =
        mod_posneg
          (wx
          +
          if use_atlas_camera then
            atlas_camera.x
          else
            0)
          world_width
      in
      (* Convert to NDC position of bottom-left corner of tile *)
      let ndc_x = -1.0 +. (float wx *. scale_x) in
      let ndc_y = -1.0 +. (float wy *. scale_y) in
      offsets.(!idx) <- (ndc_x, -.ndc_y -. scale_y);
      colors.(!idx) <- (float r /. 255., float g /. 255., float b /. 255.);
      idx := !idx + 1)
    altitudes biomes
