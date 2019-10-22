open QCheck
open Alg_structs

let property name = Test.make ~name

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

let semigroup_laws =
  let open Alg_structs_qcheck.Semigroup in
  suite "Semigroup Laws" @@ tests
    [
      (module struct
        include Semigroup.Bool.Or
        let name = "Bool.Or"
        let arbitrary = bool
      end);

      (module struct
        include Semigroup.Bool.And
        let name = "Bool.And"
        let arbitrary = bool
      end);

      (module struct
        include Semigroup.Int.Sum
        let name = "Int.Sum"
        let arbitrary = int
      end);

      (module struct
        include Semigroup.Int.Product
        let name = "Int.Sum"
        let arbitrary = int
      end);

      (module struct
        include Semigroup.Option.Make (Semigroup.Bool.Or)
        let name = "Option.Make (Bool.Or)"
        let arbitrary = option bool
      end);

      (module struct
        include Semigroup.Option.Make (Semigroup.Int.Sum)
        let name = "Option.Make (Int.Sum)"
        let arbitrary = option int
      end);

      (* TODO Figure out if it's possible to test this using the framework,
              or it needs a custom test written *)
      (* (module struct
       *   include (val Semigroup.Endo.make 1)
       *   let name = "Semigroup.Endo for (int -> int)"
       *   let arbitrary = Alg_structs_qcheck.Utils.int_fun
       * end) *)

      (module struct
        include Semigroup.Dual.Make (Semigroup.Int.Sum)
        let name = "Dual.Make (Int.Sum)"
        let arbitrary = int
      end);
    ]

let applicative_laws =
  let open Alg_structs_qcheck.Applicative in
  suite "Applicative Laws" @@ tests
    [
      (module struct
        include Applicative.List
        let name = "Applicative.List"
        let arbitrary = list
      end);

      (module struct
        include Applicative.Option
        let name = "Applicative.Option"
        let arbitrary = option
      end);

      (module struct
        include Applicative.Array
        let name = "Applicative.Array"
        let arbitrary = array
      end);
    ]

let foldable_laws =
  let open Alg_structs_qcheck.Foldable in
  suite "Foldable Laws" @@ tests
    [
      (module struct
        include Foldable.Option
        let name = "Foldable.Option"
        let arbitrary = option
      end);

      (module struct
        include Foldable.List
        let name = "Foldable.List"
        let arbitrary = list
      end);

      (module struct
        include Foldable.Array
        let name = "Foldable.Array"
        let arbitrary = array
      end);
    ]

let () =
  Alcotest.run "alg"
    [ functor_laws
    ; semigroup_laws
    ; applicative_laws
    ; foldable_laws
    ]
