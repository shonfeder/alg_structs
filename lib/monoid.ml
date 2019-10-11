module type S = sig
  include Semigroup.S
  val unit : t
  val concat : t list -> t
end

module type Base = sig
  include Semigroup.Base
  val unit : t
end

module Make (B : Base) : S with type t = B.t = struct
  include B
  include Semigroup.Make (B)

  let concat t = List.fold_left op unit t
end

let make (type a) unit op =
  let module Base = (struct
    type t = a
    let unit = unit
    let op = op
  end : Base with type t = a)
  in
  (module Make (Base) : S with type t = a)

let of_semigroup (type a) (module S : Semigroup.S with type t = a) unit =
  let module Base = (struct
    include S
    let unit = unit
  end : Base with type t = a)
  in
  (module Make (Base) : S with type t = a)

module Bool = struct
  module Or = (val of_semigroup (module Semigroup.Bool.Or) false)
  module And = (val of_semigroup (module Semigroup.Bool.And) true)
end

module Int = struct
  module Sum = (val of_semigroup (module Semigroup.Int.Sum) 0)
  module Product = (val of_semigroup (module Semigroup.Int.Product) 1)
end
