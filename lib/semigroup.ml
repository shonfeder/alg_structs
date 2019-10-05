module type List = sig
  type 'a t
  include Functor.S with type 'a t := 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val cons : 'a -> 'a t -> 'a t
  val uncons : 'a t -> ('a * 'a list)
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a list
end

module List : List = struct
  type 'a t =
    | Cons of 'a * 'a list
  [@@deriving eq, ord]

  let cons x (Cons (x', xs)) = Cons (x, x' :: xs)
  let uncons (Cons (x, xs)) = (x, xs)

  let map ~f (Cons (x, xs)) = Cons (f x, List.map f xs)

  let hd (Cons (x, _))  = x
  let tl (Cons (_, xs)) = xs
end

module type Base = sig
  type t
  val op : t -> t -> t
end

module type S = sig
  include Base
  val concat : t List.t ->  t
end


module Make (S:Base) : S with type t = S.t = struct
  include S
  let ( * ) a b = op a b

  let concat xs =
    let (x, xs) = List.uncons xs in
    let rec aux x = function
      | x' :: xs -> x * aux x' xs
      | [] -> x
    in
    aux x xs

  (* TODO repeated apply *)
end
