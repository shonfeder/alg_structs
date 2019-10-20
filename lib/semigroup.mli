(** TODO Description *)

(** {1 Seed} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Semigroup} for the type {{!type:Seed.t} t} *)
module type Seed = sig
  type t

  (** [op x y] is an associative operation over all elements [x] and [y] of type
      {!type:t} *)
  val op : t -> t -> t
end

(** As {!module-type:Seed} but for parameteric types of one variable *)
module type Seed1 = sig
  type 'a t

  (** [op x y] is an associative operation over all elements [x] and [y] of type
      {!type:t} *)
  val op : 'a t -> 'a t -> 'a t
end

(** {1 Interface} *)

(** A semigroup is a set of objects with an associative binary operation over it *)
module type S = sig
  include Seed

  (** The infix version of {!val:op}. *)
  val ( * ) : t -> t -> t

  val concat : t NonEmptyList.t ->  t
end

module type S1 = sig
  include Seed1
  val ( * ) : 'a t -> 'a t -> 'a t
  val concat : 'a t NonEmptyList.t -> 'a t
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Semigroup} in the form of predicates that should be true
    for any arguments of the appropriate type.

    You can use {!module:Alg_qcheck.Semigroup} to generate property based tests
    of these laws for new modules satisfying interface.

    @param S An implementation of a {{!module-type: S} Semigroup} *)
module Law (S : S) : sig

  (** [associativity x y z]: [S.(x * (y * z)) = S.((x * y) * z)] *)
  val associativity : S.t -> S.t -> S.t -> bool
end

(* TODO S2 for monoids over parametric types *)

(** {1 Constructors}

    Functions and module functors for creating implementations of
    {{!module-type:S} Semigroups} *)

(** [Make (S)] is an implementation of {{!module-type:S} Semigroup} generated
    from the {!module-type:Seed}. *)
module Make (S:Seed) : S with type t = S.t

(** [make op] is an implementation of {{!module-type:S} Semigroup} generated
    from the operation [op]. *)
val make : ('a -> 'a -> 'a) -> (module S with type t = 'a)

(** {1 Implementations} *)

module Bool : sig
  module Or : S with type t = bool
  module And : S with type t = bool
end

module Int : sig
  module Sum : S with type t = int
  module Product : S with type t = int
end

module Option : sig
  module Make (S : S) : S with type t = S.t Option.t
end
