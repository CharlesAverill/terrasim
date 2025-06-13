# TerraSim

![logo](https://github.com/CharlesAverill/terrasim/raw/main/assets/logo/logo.png)

A SimEarth clone in OCaml using [tsdl](https://erratique.ch/software/tsdl) and [tgls](https://erratique.ch/software/tgls).

See [roadmap.md](https://github.com/CharlesAverill/terrasim/blob/main/roadmap.md) for progress

## Building

```bash
git clone https://github.com/CharlesAverill/terrasim.git && cd terrasim

# Dependencies
sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libffi-dev libgmp-dev
git submodule update --init --recursive
opam install . --deps-only

dune build
```

To run, simply

```bash
dune exec TerraSim
```

## Controls

There are three views right now:

### Edit view (Cartesian)

| Input | Effect |
|---|---|
| Mouse move | Pan view |
| Mouse click | Run command denoted by highlighted UI button |
| `c` | Switch to Atlas view |

### Atlas view (Cartesian)

| Input | Effect |
|---|---|
| Mouse move | Pan view |
| `c` | Switch to Globe view |

### Globe view (Orthographic)

| Input | Effect |
|---|---|
| Click and drag | Spin globe |
| `c` | Switch to Edit view |
