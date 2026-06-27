(*external set : unit -> unit = "set_rounding"*)

let eta_float = ldexp 1. (-1074) (* smallest positive float (subnormal) *)

     
let next_up f =
  match classify_float f with
  | FP_nan -> f
  | FP_infinite -> if 0. < f then f else -.max_float
  | FP_zero | FP_subnormal ->
     let f = f +. eta_float in
     if f = 0. then -0. else f (* or next_down may return -0. *)
  | FP_normal ->
     let f, e = frexp f in
     if 0. < f || f <> -0.5 || e = -1021 then
       ldexp (f +. epsilon_float /. 2.) e
     else
       ldexp (-0.5 +. epsilon_float /. 4.) e

let next_down f = -.(next_up (-.f))


(* TODO : link with the C code *)           
let up x = next_up x
let down x = -. (up (-.x))
