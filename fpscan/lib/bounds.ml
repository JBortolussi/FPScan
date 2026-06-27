type t = Scalar.t * Scalar.t
       
let pp fmt (s1, s2) =
  Format.fprintf fmt "[%a; %a]" Scalar.pp s1 Scalar.pp s2

let mk s1 s2 = s1, s2
let get (s1, s2) = s1, s2
let join (s1,s2) (s3,s4)= Scalar.min s1 s3, Scalar.max s2 s4 

let size (s1, s2) = Scalar.(to_float s2 -. to_float s1) 
(* Compute the "distance" between b1 and b2.
   - 0 if equal
   - positive if b1 \supseteq b2
   - negative if b1 \subseteq b2
   Raise error if they are not comparable
*)
let compare ((s1,s2) as x) ((s3,s4) as y) =
  if s1 = s3 && s2 = s4 then
    0.
  else
    let lb = min s1 s3 and ub = max s2 s4 in
    let is_eq x y = abs_float ((Scalar.to_float x) -. (Scalar.to_float y)) <= 10.**(-5.) in
    let sign =
      match is_eq s1  lb, is_eq s3 lb, is_eq s2 ub, is_eq s4 ub with
      | true, _, true, _ -> (* b1 \supseteq b2. Positive! *) 1.
      | _, true, _, true -> (* b1 \subseteq b2. Negative! *) -1.
      | b1,b2,b3,b4 -> (
        Format.eprintf "Comparing %a with %a. Failure! (%b,%b,%b,%b)" pp x pp y b1 b2 b3 b4; 
        assert false (* should not happen if comparable *)
      )
    in
    let distance =
      let lb = Scalar.to_float lb in
      let ub = Scalar.to_float ub in
      let lb' = max s1 s2 and ub' = min s2 s4 in
      let lb' = Scalar.to_float lb' in
      let ub' = Scalar.to_float ub' in
      (lb' -. lb) +. (ub -. ub')
    in
    sign *. distance
      
            
