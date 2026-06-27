type t =
  {
    sign: bool;
    total: int; (* total includes sign *)
    frac: int (* both total and frac should be positive. total >= frac *)
  }
type fxp_t = t 

let pp fmt fxp =
  Format.fprintf fmt "%s%i/%i"
    (if fxp.sign then "±" else "U")
    fxp.total
    fxp.frac
                          
let mk s t f = { sign = s; total = t; frac = f }

module Set = Set.Make (struct type t = fxp_t let compare = compare end)
    
exception Overflow
exception SignError

let get_int_types fxp =
  let s = if fxp.sign then "" else "u" in
  let total = fxp.total + (if fxp.sign then 1 else 0) in
  let sz = if total <= 8 then "8"
    else if total <= 16 then "16"
    else if total <= 32 then "32"
    else if total <= 64 then "64"
    else assert false
  in
  s ^ "int" ^ sz ^ "_t" 
  
(* Compute the floor(log2) of |x|. One need 1 more bit to represent  *)
let log2_f x =
  let abs_x = abs_float x in
  if abs_x = 0. then 0 else
    int_of_float (floor((log abs_x) /. (log 2.)))
      
let log2_i x =
  let abs_x = abs x in
  if abs_x = 0 then 0 else
    int_of_float ((log (float_of_int abs_x)) /. (log 2.))
    
let from_int fxp i =
  let is_neg = i < 0 in
  if is_neg && not fxp.sign then
    raise SignError
  else
  let nb_bits_req = 1 + log2_i (abs i) in
  if nb_bits_req  + fxp.frac > fxp.total - (if fxp.sign then 1 else 0) then
    raise Overflow
  else
    (if is_neg then -1 else 1) * (Int.shift_left i fxp.frac)


let from_rat fxp q =
  let is_neg = Q.lt q Q.zero in
  if is_neg && not fxp.sign then
    raise SignError
  else  
    (* compute q * 2^frac, then convert it to float and keep the integer part *)
    let q' = Q.abs q in
    let q'' = Q.mul_2exp q' fxp.frac in
    let fx = int_of_float(floor(Q.to_float q'')) in
    (if is_neg then -1 else 1) * fx 

(* let dec fxp_old fxp_new = *)
(*   fxp_new.frac -  *)


