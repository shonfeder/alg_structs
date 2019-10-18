(* TODO RENAME? *)
(** TODO description *)

(** {1 Seed} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Foldable} *)
module type Seed = sig
  type 'a t
  val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
end

(** {1 Interface} *)

module type S = sig
  include Seed
  val fold_map : m:(module Monoid.S with type t = 'm) -> f:('a -> 'm) -> 'a t -> 'm
  val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t ->  'b
  (* TODO Folds over semigroups *)

  val to_list : 'a t -> 'a list
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val any : f:('a -> bool) -> 'a t -> bool
  val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
  val max : compare:('a -> 'a -> int) -> 'a t -> 'a option
  val min : compare:('a -> 'a -> int) -> 'a t -> 'a option
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Foldable} in the form of predicates that should be true
    for any arguments of the appropriate type.

    You can use {!module:Alg_qcheck.Foldable} to generate property based
    tests of these laws for new modules satisfying this interface.

    @param A An implementation of {{!module-type: S} Foldable} *)
module Law (C : S) : sig
end

(** {1 Constructors}

    Module functors for creating implementations of {{!module-type:S}
    Foldable} *)
module Make (Seed : Seed) : S with type 'a t = 'a Seed.t

(** {1 Implementations} *)

module Option : S with type 'a t = 'a Option.t
module List : S with type 'a t = 'a List.t
module Array : S with type 'a t = 'a Array.t
