(** Non relational domains reduction. *)

module type Reduction = sig
    type t
    val name : string
    val fprint_help: Format.formatter -> unit
    val parse_param : string -> unit 
    val rho : t -> t
end

module Make (D : NonRelational.Domain) (R : Reduction with type t = D.t) :
  NonRelational.Domain with type t = D.t
