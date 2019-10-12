module type S = sig
  type 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module Option : S with type 'a t = 'a Option.t = struct
  type 'a t = 'a Option.t
  let map ~f = Option.map f
end

module List : S with type 'a t = 'a List.t = struct
  type 'a t = 'a List.t
  let map ~f = List.map f
end
