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

(** Get the corresponding blob to a tile given altitude and biome, accounting
    for animation
    @param animated_sprite_idx Which sprite of an animated sprite to get
    @param altitude Tile altitude
    @param biome Tile biome *)
let blob_of_tile animated_sprite_idx altitude = function
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
      List.nth ocean_shallow_sprites
        (animated_sprite_idx mod List.length ocean_shallow_sprites)
  | Ocean Regular ->
      List.nth ocean_regular_sprites
        (animated_sprite_idx mod List.length ocean_shallow_sprites)
  | Ocean Deep ->
      List.nth ocean_deep_sprites
        (animated_sprite_idx mod List.length ocean_shallow_sprites)
