type 'a t =
  | Cons of 'a * 'a list
[@@deriving eq, ord]

let cons x (Cons (x', xs)) = Cons (x, x' :: xs)
let uncons (Cons (x, xs)) = (x, xs)

let map ~f (Cons (x, xs)) = Cons (f x, List.map f xs)

let hd (Cons (x, _))  = x
let tl (Cons (_, xs)) = xs
let append (Cons (x, xs)) (Cons (y, ys)) =
  Cons (x, List.append xs (y::ys))

let op = append

let fold op xs =
  let (x, xs) = uncons xs in
  List.fold_left op x xs

let of_list = function
  | x :: xs -> Some (Cons (x, xs))
  | []      -> None

let to_list (Cons (x, xs)) = x :: xs
