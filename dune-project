(lang dune 3.9)

(name PROJECT_NAME_)

(generate_opam_files true)

(source
 (github _AUTHOR_USERNAME_/PROJECT_NAME_))

(authors "_AUTHOR_NAME_")

(maintainers "_AUTHOR_NAME_")

(license LICENSE)

(documentation https://github.com/_AUTHOR_USERNAME_/PROJECT_NAME_)

(package
 (name PROJECT_NAME_)
 (synopsis "_PROJECT_SYNOPSIS_")
 (description "_PROJECT_DESCRIPTION_")
 (depends ocaml dune alcotest ocamlformat))

