(lang dune 3.9)

(name terrasim)

(generate_opam_files true)

(source
 (github CharlesAverill/TerraSim))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license MIT)

(documentation https://github.com/CharlesAverill/TerraSim)

(package
 (name terrasim)
 (synopsis "A SimEarth clone")
 (description "A SimEarth clone")
 (depends 
  (ocaml (= 5.1.1)) 
  (dune :with-test)
  (ocamlformat :with-test)
  (odoc 
   (and (= 2.4.4) :with-doc))
  (md2mld :with-doc)
  (tsdl (= 1.1.0))
  tgls
  tsdl-image
  tsdl-ttf
  ppx_blob
  ppx_unreachable
  zarith
 )
)

