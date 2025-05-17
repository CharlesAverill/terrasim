let update f x y =
 fun x' ->
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
