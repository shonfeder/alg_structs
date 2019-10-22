(** A solitary type *)

(** [Triv] specifies a module with a single type [t].

    It is used in composing other structures. *)
module type S = sig
  type t
end
