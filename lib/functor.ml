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

module Stream : S with type 'a t = 'a Stream.t = struct
  type 'a t = 'a Stream.t
  let map ~f s =
    let next _ = match Stream.next s with
      | x -> Some (f x)
      | exception Stream.Failure -> None
    in
    Stream.from next
end
