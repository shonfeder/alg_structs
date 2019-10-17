(lang dune 1.11)
(name alg)

(generate_opam_files true)

(authors "Shon Feder")
(maintainers "shon.feder@gmail.com")
(homepage "TODO")
(bug_reports "TODO")

(package
 (name alg)
 (synopsis "Interfaces and module combinators for algebraic structures")
 (description "TODO")
 (depends
  (dune (and :build (>= 1.11.3)))

  (alcotest (and :with-test (>= 0.8.5)))
  (qcheck (and :with-test (>= 0.11)))
  (qcheck-alcotest (and :with-test (>= 0.11)))
))

(package
 (name alg.qcheck)
 (synopsis "qCheck generators for the Alg library")
 (description "TODO")
 (depends
  (dune (and :build (>= 1.11.3)))

  (qcheck (and :with-test (>= 0.11)))
))
