open Logging
open Tsdl

let update f x y x' =
  if x = x' then
    y
  else
    f x'

let clamp x min max =
  if x < min then
    min
  else if x > max then
    max
  else
    x

let mod_wrap i max = ((i mod max) + max) mod max

let modf_wrap f max =
  float
    ( ((int_of_float f mod int_of_float max) + int_of_float max)
    mod int_of_float max )

let ( let* ) r f =
  match r with Ok x -> f x | Error (`Msg e) -> fatal rc_SDL "%s" e

let rangef ?(start : float = 0.) ?(step : float = 1.) j =
  let rec aux n acc =
    if n < start then
      acc
    else
      aux (n -. step) (n :: acc)
  in
  aux (j -. 1.) []

let flatten_matrix (matrix : 'a array array) default : 'a array =
  let rows = Array.length matrix in
  let cols =
    if rows > 0 then
      Array.length matrix.(0)
    else
      0
  in
  let flat = Array.make (rows * cols) default in
  for y = 0 to rows - 1 do
    for x = 0 to cols - 1 do
      let index = (y * cols) + x in
      flat.(index) <- matrix.(y).(x)
    done
  done ;
  flat

let matrix_map (matrix : 'a array array) (f : 'a -> 'b) : 'b array array =
  Array.map (Array.map f) matrix

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
