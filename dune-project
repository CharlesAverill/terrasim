(lang dune 3.9)

(name _PROJECT_NAME_)

(generate_opam_files true)

(source
 (github _AUTHOR_USERNAME_/_PROJECT_NAME_))

(authors "_AUTHOR_NAME_")

(maintainers "_AUTHOR_NAME_")

(license LICENSE)

(documentation https://github.com/_AUTHOR_USERNAME_/_PROJECT_NAME_)

(package
 (name _PROJECT_NAME_)
 (synopsis "An OCaml Window Mangaer")
 (description "A window manager built on top of the Graphics library")
 (depends ocaml dune graphics))

