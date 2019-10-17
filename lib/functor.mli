(** An interface for types that can be mapped over.

    The use of the word {i functor} in this context refers to the category
    theoretic concept of {{:https://en.wikipedia.org/wiki/Functor} Functor},
    which is a map between categories. *)

(** {1 Interface} *)

(** A module satisfying {!module-type:S} is an implementation of a functor.  *)
module type S = sig
  type 'a t

  (** [map ~f (T a)] is [T (f a)] *)
  val map : f:('a -> 'b) -> 'a t -> 'b t
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Functor} in the form of predicates that should be true
    for any arguments of the appropriate type.

    You can use {!module:Alg_qcheck.Functor} to generate property based tests of
    these laws for new modules satisfying this interface.

    @param F An implementation of a {{!module-type: S} Functor} *)
module Law (F : S) : sig

  (** [identity x]: [F.map ~f:Fun.id x = Fun.id x] *)
  val identity : 'a F.t -> bool

  (** [composition f g x]: [F.map ~f:(f % g) x = (F.map ~f % F.map ~f:g) x]

      where [%] is composition. *)
  val composition : ('a -> 'b) -> ('c -> 'a) -> 'c F.t -> bool
end

(** {1 Constructors}

    Module functors and signatures for expediting instantiation of
    {{!module-type:S} Functors}. *)

(** Interface for a mappable type with an unlabeled [map] function. *)
module type UnlabeledFunctor = sig
  type 'a t

  (** Equivalent to {!val:S.map} but with an unlabeled function argument *)
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** [LabelMap] makes an module instantiating {!module-type:S}, with a labeled
    {!val:S.map}, out of a module instantiating {!module-type:UnlabeledFunctor}
    with an unlabeled [map] function. *)
module LabelMap (F : UnlabeledFunctor) : S with type 'a t = 'a F.t

(** {1 Implementations} *)

module Option : S with type 'a t = 'a Option.t
module List : S with type 'a t = 'a List.t
module Array : S with type 'a t = 'a Array.t
