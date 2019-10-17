module type Seed = sig
  type 'a t
  val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
end

module type S = sig
  include Seed
  val fold_map : m:(module Monoid.S with type t = 'm) -> f:('a -> 'm) -> 'a t -> 'm
  val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t ->  'b
  (* TODO Folds over semigroups *)

  val to_list : 'a t -> 'a list
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val any : f:('a -> bool) -> 'a t -> bool
  val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
  val max : compare:('a -> 'a -> int) -> 'a t -> 'a option
  (* TODO min *)
  (* TODO sum *)
  (* TODO product *)
end

module Law (C : S) = struct

end


let max_of_compare compare a b = match compare a b with
  | i when i < 0 -> b
  | i when i > 0 -> a
  | _ -> a

module Make (Seed : Seed) : S with type 'a t = 'a Seed.t = struct
  include Seed

  let fold_map (type a) ~m:(module M : Monoid.S with type t = a) ~f t =
    fold_right ~f:(fun x y -> M.op (f x) y) ~init:M.unit t
  (* TODO *)

  let fold_left ~f ~init xs =
    let f' x k z = k (f z x) in
    fold_right xs ~f:f' ~init:Fun.id init

  let to_list t = fold_right ~f:List.cons ~init:[] t
  let is_empty t = fold_right ~f:(fun _ _ -> false) ~init:true t
  let length t = fold_left ~f:(fun c _ -> c + 1) ~init:0 t
  let any ~f t = fold_map ~m:(module Monoid.Bool.Or) ~f t
  let mem t x ~equal = any ~f:(fun y -> equal x y) t

  let max (type a) ~compare t =
    let max' = max_of_compare compare in
    let module Semi = (val Semigroup.make max' : Semigroup.S with type t = a) in
    let module Opt_monoid = Monoid.Option.Make (Semi)
    in
    fold_map ~m:(module Opt_monoid) ~f:Option.some t

  (* (fold_map ~f:(_max compare) t) *)

end

(* This is currently impossible with OCamls module sytems :( since
 * first class modules do not support type sharing of types with parameters
 * (This is a symptom of having to support for higher kinded types )
 *
 * let make (type a) fold_right =
 *   let module Seed = (struct
 *     type _ t = a
 *     let fold_right = fold_right
 *   end : Seed)
 *   in
 *   (module Make (Seed) : S) *)

module Option : S with type 'a t = 'a Option.t = struct
  module Seed = struct
    include Option

    let fold_right ~f t ~init = match t with
      | None -> init
      | Some x -> f x init
  end

  include Make (Seed)
end

module List : S with type 'a t = 'a List.t = struct
  include Make (ListLabels)
end

module Array : S with type 'a t = 'a Array.t = struct
  include Make (ArrayLabels)
end
