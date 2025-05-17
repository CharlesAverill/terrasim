open Tsdl
open Logging
open Globals
open Altitude
open Camera
open Worldgrid
open Biomes

let render_ui (renderer : Sdl.renderer) = ()

let handle_ui_event (e : Sdl.event) =
  let open Sdl.Event in
  match get e typ with
  | t when t = mouse_button_down -> (
      let _, (x, y) = Sdl.get_mouse_state () in
      let button = get e mouse_button_button in
      let tile_x = (x / !tile_w) + current_camera.x in
      let tile_y = (y / !tile_h) + current_camera.y in
      match button with
      | 1 (* Left click *) ->
          raise_terrain_gaussian tile_x tile_y
      | 3 (* Right click *) ->
          let current_tile =
            match get_global_tile tile_x tile_y with
            | None ->
                Land (Nothing 0)
            | Some x ->
                x
          in
          set_global_tile tile_x tile_y (decrease_tile current_tile)
      | _ ->
          () )
  | _ ->
      ()
