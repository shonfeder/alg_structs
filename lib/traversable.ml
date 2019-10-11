module Base = struct
  module type S = sig
    include Functor.S
    include Cata.S with type 'a t := 'a t
    module A : Applicative.S
    val traverse : f:('a -> 'b A.t) -> 'a t -> 'b t A.t
  end
end

module type S = sig
  include Base.S
  val sequence : 'a A.t t -> 'a t A.t
  (* TODO val map_m *)
end

module Make (B:Base.S) : S with type 'a t = 'a B.t
= struct
  include B
  let sequence t = traverse ~f:Fun.id t
end
