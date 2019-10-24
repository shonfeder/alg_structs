(lang dune 1.11)
(name alg_structs)

(generate_opam_files true)

(authors "Shon Feder")
(license MIT)
(source (github shonfeder/alg_structs))
(maintainers "shon.feder@gmail.com")
(homepage "https://github.com/shonfeder/alg_structs")
(bug_reports "https://github.com/shonfeder/alg_structs/issues")
(documentation "TODO")

(package
 (name alg_structs)
 (synopsis "Interfaces and module combinators for algebraic structures")
 (description "
An experimental library for programming with algebraic and category-theoretic
structures in OCaml.

Currently, implemented structures include applicative, foldable, functor, monoid,
and semigroup.
")
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
 (description "
Combinators for generating qCheck property based tests to check that
implementations of the algebraic structures provided by alg_structs adhere to
the stated laws.
")
 (depends
  (dune (and :build (>= 1.11.3)))

  (qcheck (>= 0.11))
))
