type t
val mk: Scalar.t -> Scalar.t -> t
val join: t -> t -> t
val size: t -> float
val compare: t -> t -> float
val pp: Format.formatter -> t -> unit
val get : t -> Scalar.t * Scalar.t
