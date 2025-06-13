open World.Grid

let map_view_mode : world_tile_attr_getter ref = ref `Altitude
let set_map_view_mode (mode : world_tile_attr_getter) = map_view_mode := mode
