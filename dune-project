(lang dune 2.9)

(version 1.0)

(name capsule)

(license "GPL-3.0")

(authors "Psi-prod's members")

(maintainers "tim.arnouts@protonmail.com" "lelolartichaut@laposte.net")

(source
 (github Psi-Prod/Capsule))

(generate_opam_files true)

(package
 (name capsule)
 (synopsis "Psi-prod's gemini capsule")
 (depends
  (dune
   (>= 3.0))
  (ocaml
   (>= 5.0))
  (eio
   (>= 0.6))
  (eio_main
   (>= 0.6))
  (irmin
   (>= 3.5.1))
  (irmin-git
   (>= 3.5.1))
  (irmin-git
   (>= 3.5.1))
  (logs
   (>= 0.7.0))
  (mehari
   (>= 0.1))
  (mehari-eio
   (>= 0.1))
  (preface
   (>= 1.0.0))
  (ptime
   (>= 1.0.0))))
