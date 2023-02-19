(lang dune 2.9)

(version 1.0)

(name capsule)

(license "GPL-3.0")

(authors "Psi-prod's members")

(maintainers "tim.arnouts@protonmail.com" "lelolartichaut@laposte.net")

(source
 (github Psi-Prod/Capsule))

(homepage "https://github.com/Psi-Prod/Capsule")

(generate_opam_files true)

(package
 (name capsule)
 (synopsis "Psi-prod's gemini capsule")
 (description "Psi-prod's gemini capsule.")
 (depends
  (ocaml
   (>= 4.14))
  (irmin
   (>= 3.5.1))
  (irmin-git
   (>= 3.6.0))
  (mehari
   (>= 0.2))
  (mehari-lwt-unix
   (>= 0.2))
  (logs
   (>= 0.7.0))
  (preface
   (>= 1.0.0))
  (ptime
   (>= 1.0.0))))
