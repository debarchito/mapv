(lang dune 3.24)

(name mapv)

(generate_opam_files true)

(cram enable)

(source
 (sourcehut debarchito/mapv))

(authors "Debarchito Nath <debarchiton@proton.me>")

(maintainers "Debarchito Nath <debarchiton@proton.me>")

(package
 (name mapv)
 (description "An embedable and visualizable VM for OCaml")
 (license LGPL-3.0-only)
 (depends
  (ocaml
   (>= "5.5.0"))
  (ppx_deriving
   (= "6.1.3"))
  (raylib
   (= "2.2.2"))))
