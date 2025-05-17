type camera = {mutable x: int; mutable y: int}

let view_width = 48 (* in tiles *)

let view_height = 27

let current_camera = {x= 0; y= 0}

let move_current_camera dx dy =
  current_camera.x <- current_camera.x + dx ;
  current_camera.y <- current_camera.y + dy
