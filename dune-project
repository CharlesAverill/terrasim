(lang dune 3.9)

(name TerraSim)

(generate_opam_files true)

(source
 (github CharlesAverill/TerraSim))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license MIT)

(documentation https://github.com/CharlesAverill/TerraSim)

(package
 (name TerraSim)
 (synopsis "A SimEarth clone")
 (description "A SimEarth clone")
 (depends 
  (ocaml (= 5.3.0)) 
  dune
  (alcotest :with-test)
  (ocamlformat :with-test)
  (ocaml-lsp-server :with-test)
  (merlin :with-test)
  (odoc :with-doc)
  (tsdl (= 1.1.0))
  tsdl-image
  ppx_blob
  domainslib
  digestif
 )
)

