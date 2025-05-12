(lang dune 3.9)

(name TerraSim)

(generate_opam_files true)

(source
 (github CharlesAverill/TerraSim))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license _SPX_LICENSE_)

(documentation https://github.com/CharlesAverill/TerraSim)

(package
 (name TerraSim)
 (synopsis "A SimEarth clone")
 (description "A SimEarth clone")
 (depends ocaml dune alcotest ocamlformat odoc))

