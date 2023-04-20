(lang dune 3.7)

(name eon)

(authors "Max Lantas")

(maintainers "Max Lantas <mnxndev@outlook.com")

(source
 (github mnxn/eon))

(license AGPL-3.0-only)

(generate_opam_files true)

(package
 (name eon)
 (synopsis "The eon programming language")
 (depends
  (ocaml
   (and
    (>= 4.14)
    (< 4.15)))))
