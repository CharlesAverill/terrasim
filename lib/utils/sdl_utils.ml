open Tsdl
open Standard_utils

(* https://gist.github.com/Gumichan01/332c26f6197a432db91cc4327fcabb1c *)
let render_draw_circle renderer x y radius =
  let rec loop offsetx offsety d status =
    if offsety < offsetx then
      status
    else
      let draw_points : (int * int) list =
        [ (x + offsetx, y + offsety)
        ; (x + offsety, y + offsetx)
        ; (x - offsetx, y + offsety)
        ; (x - offsety, y + offsetx)
        ; (x + offsetx, y - offsety)
        ; (x + offsety, y - offsetx)
        ; (x - offsetx, y - offsety)
        ; (x - offsety, y - offsetx) ]
      in
      let status =
        List.fold_left
          (fun acc (px, py) ->
            match Sdl.render_draw_point renderer px py with
            | Ok () ->
                acc
            | Error _ ->
                -1 )
          status draw_points
      in
      if status < 0 then
        status
      else if d >= 2 * offsetx then
        loop (offsetx + 1) offsety (d - (2 * offsetx) - 1) status
      else if d < 2 * (radius - offsety) then
        loop offsetx (offsety - 1) (d + (2 * offsety) - 1) status
      else
        loop (offsetx + 1) (offsety - 1)
          (d + (2 * (offsety - offsetx - 1)))
          status
  in
  loop 0 radius (radius - 1) 0

let render_fill_circle renderer x y radius =
  let rec loop offsetx offsety d status =
    if offsety < offsetx then
      status
    else
      let draw_lines =
        [ (x - offsety, y + offsetx, x + offsety, y + offsetx)
        ; (x - offsetx, y + offsety, x + offsetx, y + offsety)
        ; (x - offsetx, y - offsety, x + offsetx, y - offsety)
        ; (x - offsety, y - offsetx, x + offsety, y - offsetx) ]
      in
      let status =
        List.fold_left
          (fun acc (x1, y1, x2, y2) ->
            match Sdl.render_draw_line renderer x1 y1 x2 y2 with
            | Ok () ->
                acc
            | Error _ ->
                -1 )
          status draw_lines
      in
      if status < 0 then
        status
      else if d >= 2 * offsetx then
        loop (offsetx + 1) offsety (d - (2 * offsetx) - 1) status
      else if d < 2 * (radius - offsety) then
        loop offsetx (offsety - 1) (d + (2 * offsety) - 1) status
      else
        loop (offsetx + 1) (offsety - 1)
          (d + (2 * (offsety - offsetx - 1)))
          status
  in
  loop 0 radius (radius - 1) 0

let hex_to_int s = int_of_string ("0x" ^ s)

let rgb_of_hex (hex : string) =
  let hex = String.uppercase_ascii hex in
  let hex =
    if String.length hex = 7 && hex.[0] = '#' then
      String.sub hex 1 6
    else if String.length hex = 6 then
      hex
    else
      failwith "Invalid hex code"
  in
  let r = hex_to_int (String.sub hex 0 2) in
  let g = hex_to_int (String.sub hex 2 2) in
  let b = hex_to_int (String.sub hex 4 2) in
  (r, g, b)

let set_render_color (r, g, b) renderer =
  let* _ = Sdl.set_render_draw_color renderer r g b 255 in
  ()

let sdlcolor_of_tuple (r, g, b) = Sdl.Color.create ~r ~g ~b ~a:255
