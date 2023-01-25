(lang dune 2.9)

(version 1.0)

(name gemblogger)

(license "GPL-3.0")

(authors "The gemblogger programmers")

(maintainers "tim.arnouts@protonmail.com")

(source
 (github Psi-Prod/Gemblogger))

(generate_opam_files true)

(package
 (name gemblogger)
 (synopsis
  "A Gemini capsule in pure OCaml build on top of YOCaml and Mehari")
 (depends
  (dune
   (>= 3.0))
  (ocaml
   (>= 4.14))))
