(** Catamorphisms *)

module type Base = sig
  type 'a t
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
end

module type S = sig
  include Base
  val fold_map : m:(module Monoid.S with type t = 'm) -> f:('a -> 'm) -> 'a t -> 'm
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
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
    foldr (fun x y -> M.op (f x) y) M.unit t
  (* TODO *)

  let foldl f z0 xs =
    let f' x k z = k (f z x) in
    foldr f' Fun.id xs z0

  let to_list t = foldr List.cons [] t
  let is_empty t = foldr (fun _ _ -> false) true t
  let length t = foldl (fun c _ -> c + 1) 0 t
  let any ~f t = fold_map ~m:(module Monoid.Bool.Or) ~f t
  let mem t x ~equal = any ~f:(fun y -> equal x y) t
end

(* This is impossible currently impossible with OCamls module sytems :( since
 * first class modules do not support type sharing of types with parameters
 * (This is a symptom of having to support for higher kinded types )
 *
 * let make (type a) foldr =
 *   let module Base = (struct
 *     type _ t = a
 *     let foldr = foldr
 *   end : Base)
 *   in
 *   (module Make (Base) : S) *)

module Option : S with type 'a t = 'a Option.t = struct
  module Base = struct
    include Option

    let foldr f z = function
      | None -> z
      | Some x -> f x z
  end

  include Make (Base)
end
