(lang dune 3.7)
(name tweed)

(generate_opam_files true)

(source (github smondet/tweed))

(authors "Seb Mondet <seb@mondet.org>")

(maintainers "Seb Mondet <seb@mondet.org>")

(license MIT)

(documentation https://github.com/smondet/tweed/)

(package
 (name tweed)
 (synopsis "Higher-level constructs on top of Nottui")
 (description "Experiments to quickly build TUIs.")
 (depends ocaml dune
         fmt base nottui
         ppx_base ppx_deriving ppx_fields_conv ppx_variants_conv
         ppx_inline_test ppx_expect)
 (tags
  (topics "TUI" Nottui)))
