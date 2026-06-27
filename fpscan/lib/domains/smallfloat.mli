(** A dummy abstract domain. You can start from this file and dummy.ml
    to write your own non relational abstract domain. *)

module type Bounds =
  sig
    val name: string
    val min : float
    val max : float
  end

module Make : functor (B : Bounds) -> NonRelational.Domain

module B16  : NonRelational.Domain
