(** Interval abstract domain. *)
type 'base interval_t = Bot | Itv of 'base * 'base

module type DOMAIN =
  sig
    include NonRelational.Domain
    type elt
    val bounds: t -> elt * elt
    val of_bounds: elt -> elt -> t
  end
                    
module Int : DOMAIN with type elt = InfInt.t
(* module Int32 : NonRelational.Domain *)
(* module Int64 : NonRelational.Domain *)

  
module Rat : DOMAIN with type elt = Q.t
(* module Float : NonRelational.Domain *)
module Double : DOMAIN with type elt = float and type t = float interval_t
module DoubleRnd : DOMAIN with type elt = float and type t = float interval_t 
(* module DoubleError : NonRelational.Domain  *)
