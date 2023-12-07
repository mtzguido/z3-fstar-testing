(lang dune 3.7)

(name z3_fstar_testing)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Guido Martínez")

(maintainers "Guido Martínez")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name z3_fstar_testing)
 (synopsis "A utility to test Z3 against F*")
 (description "IOU")
 (depends ocaml dune)
 ; (tags
 ;  (topics "to describe" your project))
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
