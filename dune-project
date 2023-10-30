(lang dune 3.10)
(using cinaps 1.0)

(name links_lsp)

(generate_opam_files true)

(source
 (github brandonf2002/links-lsp))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name links_lsp)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune
  yojson
  (ppx_yojson_conv_lib (>= "v0.14")))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
