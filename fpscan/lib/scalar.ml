type t =
  | Float of float
  | Rat of Q.t
  | Bool of bool

let is_infty x =
  match x with
  | Float f -> f = infinity || f = neg_infinity
  | Rat q -> not (Q.is_real q)
  | Bool _ -> false

let of_int i = Rat (Q.of_int i)
let of_q q = Rat q
let of_float f = Float f
let of_bool b = Bool b
let to_float s =
  match s with
  | Float f -> f
  | Rat q -> Q.to_float q
  | Bool true -> 1.
  | Bool false -> 0.
               
let minmax ff qf bf a b =
  match a,b with
  | Float fa, Float fb -> Float (ff fa fb)
  | Rat qa, Rat qb -> Rat (qf qa qb)
  | Bool b1, Bool b2 -> Bool ( bf b1 b2)
  | _ -> assert false

let min = minmax min Q.min min
let max = minmax max Q.max max

let pp fmt x =
  match x with
  | Float f -> Format.fprintf fmt "%f" f
  | Rat q -> Format.fprintf fmt "%s" (Q.to_string q)
  | Bool b -> Format.fprintf fmt "%b" b
