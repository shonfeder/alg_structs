(** {1 [alg_structs]: Algebraic Structures in OCaml Structs}

    {2 Summary}

    An library specifying algebraic structures and category-theoretic idioms
    useful in the design and implementation of software.

    Currently, this library should be viewed as an experiment to determine
    whether easy access to such mechanisms can be used to any advantage in OCaml
    programs.

    The library is modeled after a fragment of Haskell’s rich ecosystem of
    algebraic structures implemented via typeclasses. However, liberties have
    been taken to adapt the implementations to be more amenable to idiomatic
    OCaml where it seemed appropriate.

    {2 Conventions}

    {3 A structure's signature is named [S]}

    Each {{!section:structures} structure} includes a signature [S] which gives
    its specification. [S] specifies the core types and operations of the
    structure as well any additional functions derived from those core aspects.

    {3 A structure can be built from its [Seed]}

    Most of the structures can be built up from a [Seed]. Where applicable, a
    structure's [Seed] specifies the essential types and operators needed to
    elaborate out the extended structure.

    Users are free to implement their own fully customized versions of a
    structure, or to build one from a [Seed] and then override whichever
    functions they want. See each structure for relevant examples.

    {3 A structure should obey its [Law]s}

    Every structure includes a parameterized module called [Law]. The laws are
    expressed as predicates that should be true for any arguments of the
    specified type. The [Law] serves both as documentation of those necessary
    properties of a structure that cannot be encoded in the type system and as a
    tool for checking that your own implementations are lawfull.

    If you implement a structure satisfying some spec, you
    should ensure it follows the laws. You can use the package
    [alg_structs_qcheck] to help generate property based tests for this purpose.
*)


(** {1:structures Structures} *)

(** {2 Triviality} *)

module Triv = Triv

(** {2 Functors} *)

module Functor = Functor
module Applicative = Applicative

(** {2 Algebras} *)

module Semigroup = Semigroup
module Monoid = Monoid

(** {2 Folding}*)

module Foldable = Foldable

(** {1 Utilities }*)

module NonEmptyList = NonEmptyList

(* Don't need to document this since it's included *)
(**/**)
module Util = Util
(**/**)

include Util
