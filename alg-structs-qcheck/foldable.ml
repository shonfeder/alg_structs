open QCheck
open Utils

module Alg = Alg_structs

module type S = sig
  include Alg.Foldable.S
  val name : string
  val arbitrary : 'a arbitrary -> 'a t arbitrary
end

let test (module F : S) =
  let module Law = Alg.Foldable.Law(F) in
  []

let tests ts = make_tests test ts
