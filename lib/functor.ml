module type S = sig
  type 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
end
