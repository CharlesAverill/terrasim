{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.SDL2
    pkgs.SDL2_image
	pkgs.SDL2_ttf
	pkgs.libffi
	pkgs.pkg-config
	pkgs.linuxPackages_latest.perf
	pkgs.flamegraph
	pkgs.glxinfo
  ];

  shellHook = ''
	eval $(opam env --switch=terrasim)
  '';
}

