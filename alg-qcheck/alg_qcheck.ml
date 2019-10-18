open QCheck
module Alg = Alg_structs

let test_law structure impl law =
  let name = Printf.sprintf "%s - %s: %s"  structure impl law in
  Test.make ~name

let make_tests test ms =
  List.fold_left (fun ms' m -> test m @ ms') [] ms

let int_fun = fun1 (Observable.make (fun _ -> "functor")) int
let int_list_fun = fun1 (Observable.make (fun _ -> "functor")) (list int)

(** QCheck tests of the laws for [Functor]s *)
module Functor :
sig

  (** The {!module-type:Alg.Functor.S} interface extended with a way of generating
      arbitrary {!module:QCheck} values.*)
  module type S = sig
    include Alg.Functor.S
    val name : string
    val arbitrary : 'a arbitrary -> 'a t arbitrary
  end

  (** [test (module F)] is a list of {!module:QCheck} property based tests that
      check whether the {{!module:Alg.Functor.Law} Functor Laws} hold for the
      given implementation [F]. *)
  val test : (module S) -> Test.t list

  (** [test implementations] is a flattened list of {!val:test}s
      generated for each implementation in implementations *)
  val tests : (module S) list -> Test.t list

end
= struct
  module type S = sig
    include Alg.Functor.S
    val name : string
    val arbitrary : 'a arbitrary -> 'a t arbitrary
  end

  let test (module F : S) =
    let module Law = Alg.Functor.Law(F) in
    let comp_law (Fun (_, f), Fun (_, g), x) =
      Law.composition f g x
    in
    [
      test_law
        "Functor" F.name "Identity - [map id = id] for int"
        (F.arbitrary int)
        Law.identity;

      test_law
        "Functor" F.name "Identity - [map id = id] for int list"
        (F.arbitrary (list int))
        Law.identity;

      test_law
        "Functor" F.name "Composition - [map (f . g) = (map f) . (map g)] for int"
        (triple int_fun int_fun (F.arbitrary int))
        comp_law;

      test_law "Functor" F.name "Composition - [map (f . g) = (map f) . (map g)] for int list"
        (triple int_list_fun int_list_fun (F.arbitrary (list int)))
        comp_law;
    ]

  let tests ts = make_tests test ts
end

(** QCheck tests of the laws for [Semigroup]s *)
module Semigroup :
sig
  (** The {!module-type:Alg.Semigroup.S} interface extended with a way of
      generating arbitrary {!module:QCheck} values. *)
  module type S = sig
    include Alg.Semigroup.S
    val name : string
    val arbitrary : t arbitrary
  end

  (** [test impl_name (module S)] is a list of {!module:QCheck} property based tests that
      check whether the {{!module:Alg.Semigroup.Law} Semigroup Laws} hold for
      the given implementation [S]. *)
  val test : (module S) -> Test.t list

  (** [test implementations] is a flattened list of {!val:test}s
      generated for each implementation in implementations *)
  val tests : (module S) list -> Test.t list
end = struct
  module type S = sig
    include Alg.Semigroup.S
    val name : string
    val arbitrary : t arbitrary
  end

  let test (module S : S) =
    let module Law = Alg.Semigroup.Law(S)
    in
    let three = triple S.arbitrary S.arbitrary S.arbitrary
    in
    [ test_law
        "Semigroup" S.name "Associativity - [(x * (y * z)) = ((x * y) * z)]"
        three
        (fun (x, y, z) -> (Law.associativity x y z))
    ]

  let tests ts = make_tests test ts
end

module Applicative = struct
  module type S = sig
    include Alg.Applicative.S
    val name : string
    val arbitrary : 'a arbitrary -> 'a t arbitrary
  end

  let test (module A : S) =
    let module Law = Alg.Applicative.Law(A) in
    let comp_law (Fun (_, f), Fun (_, g), x) = Law.composition (A.return f) (A.return g) x in
    let homo_law (Fun (_, f), x) = Law.homomorphism f x in
    let inter_law (Fun (_, f), x) = Law.interchange (A.return f) x
    in
    [ test_law
        "Applicative" A.name "Identity - [return id <*> x = x] for ints"
        (A.arbitrary int)
        Law.identity;

      test_law
        "Applicative" A.name "Identity - [return id <*> x = x] for int lists"
        (A.arbitrary (list int))
        Law.identity;

      test_law
        "Applicative" A.name "Composition - [(return (.) <*> u <*> v <*> w) = (u <*> (v <*> w))] for int"
        (triple int_fun int_fun (A.arbitrary int))
        comp_law;

      test_law
        "Applicative" A.name "Composition - [(return (.) <*> u <*> v <*> w) = (u <*> (v <*> w))] for int list"
        (triple int_list_fun int_list_fun (A.arbitrary (list int)))
        comp_law;

      test_law
        "Applicative" A.name "Homomorphism - [return f <*> return x = return (f x)] for int"
        (pair int_fun int)
        homo_law;

      test_law
        "Applicative" A.name "Homomorphism - [return f <*> return x = return (f x)] for int list"
        (pair int_list_fun (list int))
        homo_law;

      test_law
        "Applicative" A.name "Interchange - [(u <*> return y) = (return (fun f -> f y) <*> u)] for int"
        (pair int_fun int)
        inter_law;

      test_law
        "Applicative" A.name "Interchange - [(u <*> return y) = (return (fun f -> f y) <*> u)] for int list"
        (pair int_list_fun (list int))
        inter_law;
    ]

  let tests ts = make_tests test ts
end
