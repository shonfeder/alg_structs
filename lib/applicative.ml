module type Seed = sig
  include Functor.S
  val return : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
end

module type S = sig
  include Seed
  val lift : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> )  : 'a t -> 'b t -> 'b t
  val ( <* )  : 'a t -> 'b t -> 'a t
end

module Make (B : Seed) : S with type 'a t = 'a B.t  = struct
  include B
  let lift ~f a b = app (B.map ~f a) b
  let ( <*> ) = app
  let ( *> ) a b = lift ~f:Fun.(flip const) a b
  let ( <* ) a b = lift ~f:Fun.const a b
end

module Law (A : S) = struct
  open A

  let (%) f g x = f (g x)

  let identity x = return Fun.id <*> x = x
  let composition u v w = (return (%) <*> u <*> v <*> w) = (u <*> (v <*> w))
  let homomorphism f x = return f <*> return x = return (f x)
  let interchange u y = (u <*> return y) = (return (fun f -> f y) <*> u)
end

(* Implementations *)

module List = struct
  module Seed = struct
    include Functor.List
    let return x = [x]
    let app fs xs =
      fs
      |> List.map (fun f -> List.map f xs)
      |> List.flatten
  end

  include Make (Seed)
end

module Option = struct
  module Seed = struct
    include Functor.Option
    let return x = Some x
    let app f x = match f with
      | Some f -> map ~f x
      | None -> None
  end

  include Make (Seed)
end

module Array = struct
  module Seed = struct
    include Functor.Array
    let return x = [|x|]
    let app fs xs =
      fs
      |> Array.map (fun f -> Array.map f xs)
      |> Array.to_list
      |> Array.concat
  end

  include Make (Seed)
end
