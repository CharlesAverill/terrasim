open Logging

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
