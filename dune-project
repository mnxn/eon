(lang dune 3.7)

(name eon)

(authors "Max Lantas")

(maintainers "Max Lantas <mnxndev@outlook.com")

(source
 (github mnxn/eon))

(license AGPL-3.0-only)

(generate_opam_files true)

(using menhir 2.1)

(package
 (name eon)
 (synopsis "The eon programming language")
 (depends
  (ocaml
   (and
    (>= 4.14)
    (< 4.15)))
  (sedlex
   (>= 3.1))
  (menhir
   (>= 20230415))
  ppx_deriving
  ppx_import))
