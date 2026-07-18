(lang dune 3.24)

(name map)

(generate_opam_files true)

(cram enable)

(source
 (codeberg debarchito/map))

(authors "Debarchito Nath <debarchiton@proton.me>")

(maintainers "Debarchito Nath <debarchiton@proton.me>")

(package
 (name mapv)
 (description "An embedable and visualizable VM for OCaml")
 (license LGPL-3.0-only)
 (depends
  (ocaml
   (>= "5.5.0"))
  (raylib
   (>= "1.6.0"))))

(package
 (name fold)
 (description "An infuriatingly elegant lisp for the Mapv VM")
 (license GPL-3.0-only)
 (depends
  (ocaml
   (>= "5.5.0"))
  mapv
  (menhir
   (>= "20260209"))
  (raylib
   (>= "1.6.0"))))

(using menhir 3.0)
