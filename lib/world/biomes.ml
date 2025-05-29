(** Logic handling world biomes *)

open Assets.Sprites
open Utils

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
type biome_tile = Land of biome | Ocean of ocean

(** Number of sprites in a looping animated tile *)
let sprites_per_animated_tile = 8

(** Get the corresponding blob to a tile given altitude and biome, accounting
    for animation
    @param frame_count Current frame count
    @param altitude Tile altitude
    @param biome Tile biome *)
let blob_of_tile frame_count altitude = function
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
      List.nth land_sprites
        (Utils.Standard_utils.clamp altitude 0 (List.length land_sprites - 1))
  | Land Swamp ->
      biomes_swamp_sprite
  | Ocean Shallow ->
      List.nth ocean_shallow_sprites (frame_count mod sprites_per_animated_tile)
  | Ocean Regular ->
      List.nth ocean_regular_sprites (frame_count mod sprites_per_animated_tile)
  | Ocean Deep ->
      List.nth ocean_deep_sprites (frame_count mod sprites_per_animated_tile)
