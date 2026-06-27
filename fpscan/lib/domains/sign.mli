(** Sign abstract domain. *)
type sign = SBot | S0 | SLe0 | SGe0 | STop

module Int : NonRelational.Domain with type t = sign
module Real : NonRelational.Domain with type t = sign
