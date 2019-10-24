(lang dune 1.11)
(name alg_structs)

(generate_opam_files true)

(authors "Shon Feder")
(maintainers "shon.feder@gmail.com")
(homepage "TODO")
(bug_reports "TODO")

(package
 (name alg_structs)
 (synopsis "Interfaces and module combinators for algebraic structures")
 (description "TODO")
 (depends
  (dune (and :build (>= 1.11.3)))

  (ppx_deriving (>= 4.4))

  (alcotest (and :with-test (>= 0.8.5)))
  (qcheck (and :with-test (>= 0.11)))
  (qcheck-alcotest (and :with-test (>= 0.11)))
))

(package
 (name alg_structs_qcheck)
 (synopsis "Provides qCheck generators for laws of alg_structs")
 (description "TODO")
 (depends
  (dune (and :build (>= 1.11.3)))

  (qcheck (>= 0.11))
))
