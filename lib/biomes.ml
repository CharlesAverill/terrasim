open Sprites

type biome =
  | Arctic
  | Boreal
  | Desert
  | Forest
  | Grass
  | Jungle
  | Nothing
  | Swamp

type ocean = Shallow | Regular | Deep

type tile = Land of biome | Ocean of ocean

let sprites_per_animated_tile = 8

let blob_of_tile frame_count = function
  | Land Arctic ->
      biomes_arctic_sprite
  | Land Boreal ->
      biomes_boreal_sprite
  | Land Desert ->
      biomes_desert_sprite
  | Land Forest ->
      biomes_forest_sprite
  | Land Grass ->
      biomes_grass_sprite
  | Land Jungle ->
      biomes_jungle_sprite
  | Land Nothing ->
      biomes_nothing_sprite
  | Land Swamp ->
      biomes_swamp_sprite
  | Ocean Shallow ->
      List.nth ocean_shallow_sprites (frame_count mod sprites_per_animated_tile)
  | Ocean Regular ->
      List.nth ocean_regular_sprites (frame_count mod sprites_per_animated_tile)
  | Ocean Deep ->
      List.nth ocean_deep_sprites (frame_count mod sprites_per_animated_tile)

let tile_w, tile_h = (16, 16)
