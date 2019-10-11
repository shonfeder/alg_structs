module type Base = sig
  include Functor.S
  val return : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
end

module type S = sig
  include Base
  val lift : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> )  : 'a t -> 'b t -> 'b t
  val ( <* )  : 'a t -> 'b t -> 'a t
end

module Make (B : Base) : S with type 'a t = 'a B.t  = struct
  include B
  let lift ~f a b = app (B.map ~f a) b
  let ( <*> ) = app
  let ( *> ) a b = lift ~f:Fun.(flip const) a b
  let ( <* ) a b = lift ~f:Fun.const a b
end
