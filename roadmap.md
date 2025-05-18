# Basic Engine Features

- [x] SDL window
- [x] Animated tile rendering
- [x] Toroidal grid world
- [x] Mouse-based tile editing (raise/lower terrain)
- [x] Frame management & render loop

# World and Camera Mechanics

- [x] Camera zoom
- [ ] Alternate rendering modes
    - [ ] globe
    - [ ] atlas
- [x] Implement keyboard-based cursor
- [x] Pan screen when cursor nears window edge
- [ ] Refactor grid to separate elevation from biome
- [ ] Add rendering logic to scale view based on window & view dimensions

# Geosphere and Terrain Generation

- [ ] Implement terrain generation
    - [ ] Continents
    - [ ] Biomes
    - [ ] Tectonic plates
- [ ] Track elevation as a scalar field

# Life Simulation

- [ ] Time system
- [ ] Define basic organism types (plant, herbivore, carnivore)
- [ ] Implement Game-of-Life-like rules (birth, death, spread)
- [ ] Handle organism rendering layer
- [ ] Add a tool to "plant" organisms on tiles

# Atmosphere and Climate

- [ ] Track global gases (CO2, O2, water vapor)
- [ ] Simulate temperature and greenhouse effects
- [ ] Ocean temperature
- [ ] Affect on life

# Tools, UI, Menus

- [ ] Build dropdown/hotkey-based menu system
- [ ] Implement tile inspector on click (e.g., elevation, biome, organism)
- [ ] Add simulation controls (pause/play/fast-forward)
- [ ] Add data overlays (e.g., temperature, elevation, CO₂)

# Persistence

- [ ] Serialize world state to a save file
- [ ] Load world state from file
- [ ] Asset management improvements (e.g., sprite atlases)

# Game Mechanics

- [ ] Introduce simulated events (asteroid, pandemic, climate shifts)
- [ ] Unlock evolutionary paths (new organisms)
- [ ] Add victory/loss conditions
- [ ] Balance life/climate/geosphere interactions
