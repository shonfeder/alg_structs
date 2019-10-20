open QCheck

let test_law structure impl law =
  let name = Printf.sprintf "%s - %s: %s"  structure impl law in
  Test.make ~name

let make_tests test ms =
  List.fold_left (fun ms' m -> test m @ ms') [] ms

let int_fun : (int -> int) fun_ arbitrary =
  fun1 (Observable.make (fun _ -> "functor")) int
let int_list_fun : (int list -> int list) fun_ arbitrary =
  fun1 (Observable.make (fun _ -> "functor")) (list int)
