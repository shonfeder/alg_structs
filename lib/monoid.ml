module type S = sig
  include Semigroup.S
  val unit : t
  val mconcat : t list -> t
end

module type Seed = sig
  include Semigroup.Seed
  val unit : t
end

module Make (B : Seed) : S with type t = B.t = struct
  include B
  include Semigroup.Make (B)

  let mconcat t = List.fold_left op unit t
end

(* Constructors *)

let make (type a) unit op =
  let module Seed = (struct
    type t = a
    let unit = unit
    let op = op
  end : Seed with type t = a)
  in
  (module Make (Seed) : S with type t = a)

let of_semigroup (type a) (module S : Semigroup.S with type t = a) unit =
  let module Seed = (struct
    include S
    let unit = unit
  end : Seed with type t = a)
  in
  (module Make (Seed) : S with type t = a)

(* Implementations *)

module Bool = struct
  module Or = (val of_semigroup (module Semigroup.Bool.Or) false)
  module And = (val of_semigroup (module Semigroup.Bool.And) true)
end

module Int = struct
  module Sum = (val of_semigroup (module Semigroup.Int.Sum) 0)
  module Product = (val of_semigroup (module Semigroup.Int.Product) 1)
end

module Option = struct
  module Make (S : Semigroup.S) = struct
    module Semi = Semigroup.Option.Make (S)
    include (val of_semigroup (module Semi) None)
  end
end

module Endo = struct
  module Make (T : Triv.S) = struct
    module Semi = Semigroup.Endo.Make (T)
    include (val of_semigroup (module Semi) (Fun.id : T.t -> T.t))
  end

  let make (type a) (x : a) =
    let semi = Semigroup.Endo.make x in
    of_semigroup semi (Fun.id : a -> a)
end
