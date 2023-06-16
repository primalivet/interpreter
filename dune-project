(lang dune 3.7)

(name interpreter)

(generate_opam_files true)

(source
 (github primalivet/interpreter))

(authors "Gustaf Holm")

(maintainers "Gustaf Holm")

(license LICENSE)

(documentation https://github.com/primalivet/interpreter)

(package
 (name interpreter)
 (synopsis "Coding language interpreter")
 (description "Coding language interpreter")
 (depends
  (ocaml
   (>= 5.0.0))
  (base
   (>= 0.16.0))
  (stdio
   (>= 0.16.0))
  (ppx_deriving
   (>= 5.2.1))
  dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
