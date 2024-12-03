(lang dune 3.16)

(name FP3)

(generate_opam_files true)

(source
 (github dimativator/FP3))

(authors "Dmitrii Andriianov")

(maintainers "Dmitrii Andriianov")

(license LICENSE)

(documentation https://github.com/dimativator/FP3)

(package
 (name FP3)
 (synopsis "Interpolation app")
 (description ".")
 (depends
   (ocaml (>= 4.14.0))
   (dune (>= 3.16.0))
   (cmdliner (>= 2.0.0))
   (alcotest :with-test)
   )
 (tags
  (topics "interpolation compmath")))
