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
