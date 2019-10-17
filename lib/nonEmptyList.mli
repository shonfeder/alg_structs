(** TODO *)

type 'a t
include Functor.S with type 'a t := 'a t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val cons : 'a -> 'a t -> 'a t
val uncons : 'a t -> ('a * 'a list)
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val append : 'a t -> 'a t -> 'a t
val op : 'a t -> 'a t -> 'a t
val concat : ('a -> 'a -> 'a) -> 'a t -> 'a
