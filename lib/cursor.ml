type cursor = {mutable x: int; mutable y: int}

let current_cursor = {x= 0; y= 0}

let move_current_cursor dx dy =
  current_cursor.x <- current_cursor.x + dx ;
  current_cursor.y <- current_cursor.y + dy
