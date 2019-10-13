(** Catamorphisms *)

module type Base = sig
  type 'a t
  val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
end

module type S = sig
  include Base
  val fold_map : m:(module Monoid.S with type t = 'm) -> f:('a -> 'm) -> 'a t -> 'm
  val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t ->  'b
  (* TODO Folds over semigroups *)

  val to_list : 'a t -> 'a list
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val any : f:('a -> bool) -> 'a t -> bool
  val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
  (* TODO max *)
  (* TODO min *)
  (* TODO sum *)
  (* TODO product *)
end

module Make (B : Base) : S with type 'a t = 'a B.t = struct
  include B

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
end

(* This is impossible currently impossible with OCamls module sytems :( since
 * first class modules do not support type sharing of types with parameters
 * (This is a symptom of having to support for higher kinded types )
 *
 * let make (type a) fold_right =
 *   let module Base = (struct
 *     type _ t = a
 *     let fold_right = fold_right
 *   end : Base)
 *   in
 *   (module Make (Base) : S) *)

module Option : S with type 'a t = 'a Option.t = struct
  module Base = struct
    include Option

    let fold_right ~f t ~init = match t with
      | None -> init
      | Some x -> f x init
  end

  include Make (Base)
end

module List : S with type 'a t = 'a List.t = struct
  include Make (ListLabels)
end
