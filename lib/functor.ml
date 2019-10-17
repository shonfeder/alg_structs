module type S = sig
  type 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module Law (F : S) = struct
  let (%) f g x = f (g x)
  let identity x = F.map ~f:Fun.id x = x
  let composition f g x = F.map ~f:(f % g) x = (F.map ~f % F.map ~f:g) x
end

module type UnlabeledFunctor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module LabelMap (F : UnlabeledFunctor) : S with type 'a t = 'a F.t = struct
  include F
  let map ~f = map f
end

module Option = LabelMap (Option)
module List = LabelMap (List)
module Array = LabelMap (Array)
