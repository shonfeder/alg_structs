(** Support for generating {{:https://github.com/c-cube/qcheck} QCheck} tests
    for the structures supplied by {!module:Alg_structs}.

    Each of the modules under {{!section:structure} Structure test support}
    provides three things:

    + A signature [S] that specifies an interface for a module that can be used
      to generate a suite of QCheck tests to check for violations of the
      structures laws.
    + A function [test] that takes a module satisfying [S] and produces a set of
      tests to check for the laws.
    + A function [tests] that takes a list of modules satisfying [S] and produces
      a list of tests, as per [test], for each implementation supplied.

    Here is an example of how to use this library to generat tests for various
    implementations of [Functor]:

    {[
      open QCheck
      open Alg_structs

      (* A conveninece wrapper for QCheck and Alcotest *)
      let suite name tests =
        let tests = List.map QCheck_alcotest.to_alcotest tests in
        (name, tests)

      let functor_laws =
        let open Alg_structs_qcheck.Functor in
        suite "Functor Laws" @@ tests
            [
                (module struct
                    include Functor.Option
                    let name = "Functor.Option"
                    let arbitrary = option
                end);

                (module struct
                    include Functor.List
                    let name = "Functor.List"
                    let arbitrary = list
                end);

                (module struct
                    include Functor.Array
                    let name = "Functor.Array"
                    let arbitrary = array
                end);
            ]

     (* Runs the tests *)
     let () = Alcotest.run "alg" [ functor_laws ]
    ]} *)


(** {1:structure Structure test support} *)

(** QCheck tests of {{!module:Alg_structs.Functor.Law} Functor Laws} *)
module Functor = Functor

(** QCheck tests of {{!module:Alg_structs.Applicative.Law} Applicative Laws} *)
module Applicative = Applicative

(** QCheck tests of {{!module:Alg_structs.Semigroup.Law} Semigroup Laws} *)
module Semigroup = Semigroup

(** QCheck tests of {{!module:Alg_structs.Monoid.Law} Monoid Laws} *)
module Monoid = Monoid

(** QCheck tests of {{!module:Alg_structs.Foldable.Law} Foldable Laws} *)
module Foldable = Foldable


(** {1 Utilities} *)

module Utils = Utils
