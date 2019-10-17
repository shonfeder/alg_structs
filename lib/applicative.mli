(** {1 Seed} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Applicative} *)
module type Seed = sig
  include Functor.S
  val return : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
end

(** {1 Interface} *)

module type S = sig
  include Seed
  val lift : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> )  : 'a t -> 'b t -> 'b t
  val ( <* )  : 'a t -> 'b t -> 'a t
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Applicative} in the form of predicates that should be true
    for any arguments of the appropriate type.

    You can use {!module:Alg_qcheck.Applicative} to generate property based
    tests of these laws for new modules satisfying this interface.

    @param A An implementation of {{!module-type: S} Applicative} *)
module Law (A : S) : sig

  (** [identity x]: [return Fun.id <*> x = x] *)
  val identity : 'a A.t -> bool

  (** [composition u v w]: [(return (%) <*> u <*> v <*> w) = (u <*> (v <*> w))] *)
  val composition : ('b -> 'c) A.t -> ('a -> 'b) A.t -> 'a A.t -> bool

  (** [homomorphism f x]: [return f <*> return x = return (f x)] *)
  val homomorphism : ('a -> 'b) -> 'a -> bool

  (** [interchange u y]: [(u <*> return y) = (return (fun f -> f y) <*> u)] *)
  val interchange : ('a -> 'b) A.t -> 'a -> bool
end

(** {1 Constructors}

    Module functors for creating implementations of {{!module-type:S}
    Applicative} *)

module Make (B : Seed) : S with type 'a t = 'a B.t

(** {1 Implementations} *)

module List : S with type 'a t = 'a List.t
module Option : S with type 'a t = 'a Option.t
module Array : S with type 'a t = 'a Array.t
