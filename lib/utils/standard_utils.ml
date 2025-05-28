open Logging
open Tsdl

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
    (((int_of_float f mod int_of_float max) + int_of_float max)
    mod int_of_float max)

let ( let* ) r f =
  match r with Ok x -> f x | Error (`Msg e) -> fatal rc_SDL "%s" e
