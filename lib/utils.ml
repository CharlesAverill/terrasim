let update f x y =
 fun x' ->
  if x = x' then
    y
  else
    f x'
