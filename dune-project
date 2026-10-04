(lang dune 3.22)

(name butler)

(license MIT)

(package
 (name butler)
 (synopsis "Butler service manager")
 (description
  "Runs a dependency graph of services and restarts them when watched files change.")
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.22))
  re
  yojson))
