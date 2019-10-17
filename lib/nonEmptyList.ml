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

let concat op xs =
  let (x, xs) = uncons xs in
  let rec aux x = function
    | x' :: xs -> op x (aux x' xs)
    | [] -> x
  in
  aux x xs
