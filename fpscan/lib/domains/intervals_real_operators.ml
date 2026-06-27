(** See https://arxiv.org/pdf/0708.3721.pdf for all definitions  *)
(** 
Erratum: some issues in the definition of atan in the paper:

instead of 
atan_lb = \Sigma_{i=1}^{2n+1} x^{2i+1} \frac{(-1)^i}{2i+1} 
atan_up = \Sigma_{i=1}^{2n} x^{2i+1} \frac{(-1)^i}{2i+1} 

we have 
atan_lb = \Sigma_{i=0}^{2n} x^{2i+1} \frac{(-1)^i}{2i+1} 
atan_up = \Sigma_{i=0}^{2n-1} x^{2i+1} \frac{(-1)^i}{2i+1} 

*)


(* sqrt: Newton's method. *)
let rec sqrt_n_up x n =
  if n = 0 then x +. 1.
  else
    let y = sqrt_n_up x (n-1) in
    0.5 *. (y +. x /. y) 

let sqrt_n_lb x n = x /. (sqrt_n_up x n)

let rec sqrt_rat_n_up x n =
  if n = 0 then Q.add x Q.one
  else
    let y = sqrt_rat_n_up x (n-1) in
    Q.mul (
      Q.div Q.one (Q.of_int 2)
    ) (
      Q.add y (
        Q.div x y
      )
    )

let sqrt_rat_n_lb x n = Q.div x (sqrt_rat_n_up x n)

(* Trigonometric functions *)

let rec fact n =
  if n <= 1 then 1 else n * fact (n-1) 
      
let rec sin_series x i max accu =
  let i_f = float_of_int i in
  let curr = (-1.)**(i_f -. 1.) *. ((x ** (2. *. i_f -. 1.)) /. (float_of_int (fact (2*i - 1)))) in
  if i = max then
    curr +. accu
  else
    sin_series x (i+1) max (curr +. accu)

let rec cos_series x i max accu =
  let i_f = float_of_int i in
  let curr = (-1.)**i_f *. ((x ** (2. *. i_f)) /. (float_of_int (fact (2*i)))) in
  if i = max then
    curr +. accu
  else
    cos_series x (i+1) max (curr +. accu)
  
let sin_cos_m x n =
    if x < 0. then
      2*n
    else
      2*n + 1
  
let sin_n_up x n =
  let m = sin_cos_m x n in
  sin_series x 1 m 0.

let sin_n_lb x n =
  let m = sin_cos_m x n in
  sin_series x 1 (m+1) 0.

let cos_n_up x n =
  let m = sin_cos_m x n in
  1. +. cos_series x 1 (m+1) 0.

let cos_n_lb x n =
  let m = sin_cos_m x n in
  1. +. cos_series x 1 m 0.

(* Arctangent and Pi *)  
                         
(* sup_atan_x_n \Sum_{i=1}^{2n+1} x^{2i+1} (-1)^i/(2i+1), if 0 < x <=
   1*)
(* inf_atan_x_n \Sum_{i=1}^{2n} x^{2i+1} (-1)^i/(2i+1), if 0 < x <=
   1*)
let rec atan_series_n x i max accu =
  let i_f = float_of_int i in
  let curr = (x**(2. *. i_f +. 1.)) *. ((-1.) ** i_f) /. (2. *. i_f +. 1.) in
  if i = max then
    curr +. accu
  else
    atan_series_n x (i+1) max (curr +. accu)
      
let rec atan_n_lb x n =
  if x = 0. then 0.
  else if x > 0. && x <= 1. then
    atan_series_n x 0 (2*n-1) 0.
  else if x > 1. then
    (pi_n_lb n) /. 2. -. (atan_n_up (1. /. x) n)
  else (* x < 0 *)
    -. (atan_n_up (-. x) n)
and atan_n_up x n =
  if x = 0. then 0. else
    if x > 0. && x <= 1. then
      atan_series_n x 0 (2*n) 0.
    else
      if x > 1. then
        (pi_n_up n) /. 2. -. atan_n_lb (1. /. x) n
      else (* x < 0 *)
        -. atan_n_lb (-. x) n
and pi_n_lb n =
  16. *. (atan_n_lb (1./.5.) n) -. 4. *. (atan_n_up (1./. 239.) n)
and pi_n_up n =
  16. *. (atan_n_up (1. /. 5.) n) -. 4. *. (atan_n_lb (1. /. 239.) n)

let sine_rat_series x max =
  let rec _sine_rat_series (i: int) (sign: Q.t) (x_2k1: Q.t) (fact_2k1: Q.t) (accu: Q.t) =
    let curr: Q.t = Q.mul sign (Q.div x_2k1 fact_2k1) in
    if i == max 
    then (
      Q.add accu curr
    ) else (
        let sign = Q.sub Q.zero sign in
        let x_2k1 = x_2k1 |> Q.mul x |> Q.mul x in
        let fact_2k1 = Q.mul fact_2k1 (Q.of_int ((2 * (i + 1)) * (2 * (i + 1) + 1))) in
        _sine_rat_series (i + 1) sign x_2k1 fact_2k1 (Q.add accu curr)
    )
  in
  _sine_rat_series 0 (Q.of_int 1) (x) (Q.of_int 1) (Q.of_int 0)

let cos_rat_series x max =
  let rec _cos_rat_series (i: int) (sign: Q.t) (x_2k: Q.t) (fact_2k) (accu: Q.t): Q.t =
    let curr: Q.t = Q.mul sign (Q.div x_2k fact_2k) in
    let accu = Q.add accu curr in
    if i == max
    then accu
    else (
      let sign = Q.sub Q.zero sign in
      let x_2k = x_2k |> Q.mul x |> Q.mul x in
      let fact_2k = fact_2k |> Q.mul (Q.of_int ((2*i+1) * 2*(i+1))) in
      _cos_rat_series (i + 1) sign x_2k fact_2k accu
    )
  in
  _cos_rat_series 0 (Q.one) (Q.one) (Q.one) (Q.zero)

let sin_cos_rat_m x n =
  if Q.leq x Q.zero
  then 2 * n
  else 2 * n + 1
 
let sin_rat_lb x n =
  (* even if x <= 0 *)
  let m = sin_cos_rat_m x n in
  sine_rat_series x m

let sin_rat_up x n =
  (* even if x > 0 *)
  let m = (sin_cos_rat_m x n) + 1 in
  sine_rat_series x m 

let cos_rat_lb x n =
  let m = 2*n + 1 in
  cos_rat_series x m

let cos_rat_up x n =
  let m = 2*n in
  cos_rat_series x m

let atan_rat_series x max =
  let rec _atan_rat_series (i: int) (sign: Q.t) (x_2k1: Q.t) (accu: Q.t): Q.t =
    let curr = Q.mul sign (Q.div x_2k1 (Q.of_int (2*i+1))) in
    let accu = Q.add accu curr in
    if i == max
    then accu
    else (
      let sign = Q.sub Q.zero sign in
      let x_2k1 = x_2k1 |> Q.mul x |> Q.mul x in
      _atan_rat_series (i+1) sign x_2k1 accu
    )
  in
  _atan_rat_series 0 Q.one x Q.zero


let rec atan_rat_lb (x: Q.t) (n: int): Q.t =
  if Q.equal x Q.zero then Q.zero
  else if (Q.gt x Q.zero) && (Q.leq x Q.one) then
    atan_rat_series x (2*n+1)
  else if (Q.gt x Q.one) then
    let pi_2 = Q.div (pi_rat_lb n) (Q.of_int 2) in
    Q.sub (pi_2) (atan_rat_up (Q.inv x) n)
  else (* x < 0 *)
    Q.sub Q.zero (atan_rat_up (Q.sub Q.zero x) n)
and atan_rat_up (x: Q.t) (n: int): Q.t =
  if Q.equal x Q.zero then Q.zero
  else if (Q.gt x Q.zero) && (Q.leq x Q.one) then
    atan_rat_series x (2*n)
  else if (Q.gt x Q.one) then
    let pi_2 = Q.div (pi_rat_up n) (Q.of_int 2) in
    Q.sub (pi_2) (atan_rat_lb (Q.inv x) n)
  else (* x < 0 *)
    Q.sub Q.zero (atan_rat_lb (Q.sub Q.zero x) n)
and pi_rat_lb (n: int): Q.t =
  (* 16 * atan(1/5) - 4 * atan(1/239) *)
  Q.sub
    (Q.mul 
      (Q.of_int 16)
      (atan_rat_lb (5 |> Q.of_int |> Q.inv) n))
    (Q.mul 
      (Q.of_int 4)
      (atan_rat_up (239 |> Q.of_int |> Q.inv) n))
and pi_rat_up (n: int): Q.t =
  (* 16 * atan(1/5) - 4 * atan(1/239) *)
  Q.sub
    (Q.mul 
      (Q.of_int 16)
      (atan_rat_up (5 |> Q.of_int |> Q.inv) n))
    (Q.mul 
      (Q.of_int 4)
      (atan_rat_lb (239 |> Q.of_int |> Q.inv) n))

(* Exponential *)
let rec exp_series x i max accu =
  let i_f = float_of_int i in
  let curr = (x**i_f) /. (float_of_int (fact i)) in
  if i = max then
    curr +. accu
  else
    exp_series x (i+1) max (curr +. accu)
    
let rec exp_n_lb x n  =
  if x >= -1. && x <= 0. then
    exp_series x 0 (2 * (n+1) + 1) 0.
  else if x = 0. then
    1.
  else if x < -1. then
    let m_floor_x = -. (floor x) in
    (exp_n_lb (x /. m_floor_x) n) ** m_floor_x
  else (* x > 0 *)
    1. /. ( exp_n_up (-. x) n)
and exp_n_up x n =
  if x >= -1. && x <= 0. then
    exp_series x 0 (2 * (n+1)) 0.
  else if x = 0. then
    1.
  else if x < -1. then
    let m_floor_x = -. (floor x) in
    (exp_n_up (x /. m_floor_x) n) ** m_floor_x
  else (* x > 0 *)
    1. /. ( exp_n_lb (-. x) n)
              
(* Natural Logarithm *)
let rec ln_series x i max accu =
  let i_f = float_of_int i in
  let curr = (-1.)**(i_f +. 1.) *. (((x -. 1.) ** i_f) /. i_f) in
  if i = max then
    curr +. accu
  else
    ln_series x (i+1) max (curr +. accu)

let rec lnnat x k =
  if x < k then
    0., x
  else
    let m, y = lnnat (x /. k) k in
    m+.1., y
      
let rec ln_n_lb x n =
  if x > 1. && x <= 2. then
    ln_series x 1 (2*n) 0.
  else if x = 1. then
    0.
  else if x > 0. && x < 1. then
    -. ln_n_lb (1. /. x) n
  else (* x > 2 *)
    let m, y = lnnat 2. x in
    m *. (ln_n_lb 2. n) +. (ln_n_lb y n)

let rec ln_n_up x n =
  if x > 1. && x <= 2. then
    ln_series x 1 (2*n + 1) 0.
  else if x = 1. then
    0.
  else if x > 0. && x < 1. then
    -. ln_n_up (1. /. x) n
  else (* x > 2 *)
    let m, y = lnnat 2. x in
    m *. (ln_n_up 2. n) +. (ln_n_up y n)



(* Interval operators *)
    
let sqrt_inter_fun n a b =
  if a >= 0. then
    sqrt_n_lb a n, sqrt_n_up b n
  else
    raise Utils.OutofDomain

let sqrt_rat_inter_fun n a b =
  let n = n * 3 in
  if Q.geq a Q.zero then
    sqrt_rat_n_lb a n, sqrt_rat_n_up b n
    (* sqrt_rat_n_lb a n, Q.of_float (sqrt_n_lb (Q.to_float b) n) *)
  else
    raise Utils.OutofDomain
    
let atan_inter_fun n a b = atan_n_lb a n, atan_n_up b n

let atan_rat_inter_fun n a b = atan_rat_lb a n, atan_rat_up b n
                           
let pi_inter_cst n = pi_n_lb n, pi_n_up n

let pi_rat_inter_cst n = pi_rat_lb n, pi_rat_up n
                     
let exp_inter_fun n a b = exp_n_lb a n, exp_n_up b n
                          
let ln_inter_fun n a b =
  if a > 0. then
    ln_n_lb a n, ln_n_up b n
  else
    raise Utils.OutofDomain

let rec sin_inter_fun n a b =
  (* Format.eprintf "calling sin [%f, %f]@." a b;  *)
  let ra,rb = 
    let pi_lb, pi_up = pi_inter_cst n in
    (* Format.eprintf "pi = [%f, %f]@." pi_lb pi_up;  *)
    if a >= -. pi_lb /. 2. && b <= pi_lb /. 2. then
      sin_n_lb a n, sin_n_up b n
    else if a >= pi_up /. 2. && b <= pi_lb then
      sin_n_lb b n, sin_n_up a n
    else if a >= 0. && b <= pi_lb then
      min (sin_n_lb a n) (sin_n_lb b n), 1.
    else if a >= -. pi_lb && b <= 0. then
      let r_a, r_b = sin_inter_fun n (-. b) (-. a) in
      -. r_b, -. r_a
    else
      -1. , 1.
  in
  (* Format.eprintf "res sin[%f, %f] = [%f, %f]@." a b ra rb; *)
  ra,rb

let rec sin_rat_inter_fun n a b =
  let ra, rb =
    let pi_lb, pi_up = pi_rat_inter_cst n in
    let two = Q.of_int 2 in
    (* x >= -pi/2 and x <= pi/2 *)
    if  (Q.geq a (Q.sub Q.zero (Q.div pi_lb (two)))) 
        && (Q.leq b (Q.div pi_lb (two)))
    then sin_rat_lb a n, sin_rat_up b n
    (* x >= pi/2 and x <= pi *)
    else if   (Q.geq a (Q.div (pi_up) (two)))
              && (Q.leq b pi_lb)
    then sin_rat_lb b n, sin_rat_up a n
    (* x >= 0 and x <= pi *)
    else if (Q.geq a Q.zero) && (Q.leq b pi_lb)
    then min (sin_rat_lb a n) (sin_rat_lb b n), Q.one
    (* x >= -pi and x <= 0  ==> symetry*)
    else if (Q.geq a (Q.sub Q.zero pi_lb)) && (Q.leq b Q.zero)
    then
      let ra, rb = sin_rat_inter_fun n (Q.sub Q.zero b) (Q.sub Q.zero a) in
      Q.sub Q.zero rb, Q.sub Q.zero ra
    else
      Q.sub Q.zero Q.one, Q.one
  in
  ra, rb
    
let rec cos_inter_fun n a b =
  let pi_lb, _ (*pi_up*) = pi_inter_cst n in
  if a >= 0. && b <= pi_lb then
    cos_n_lb b n, cos_n_up a n 
  else if a >= -. pi_lb && b <= 0. then
    cos_inter_fun n (-. b) (-. a)
  else if a >= -. pi_lb /. 2. && b <= pi_lb /. 2. then
    min (cos_n_lb a n) (cos_n_lb b n), 1.
  else
    -1. , 1.

let cos_rat_inter_fun n a b =
  let pi_lb = pi_rat_lb n in
  let neg = Q.sub Q.zero in
  (* x >= 0 and x <= pi_lb *)
  if (Q.geq a Q.zero) && (Q.leq b pi_lb)
  then cos_rat_lb b n, cos_rat_up a n
  (* x >= -pi_lb and x <= 0 *)
  else if (Q.geq a (neg pi_lb)) && (Q.leq b Q.zero)
  then cos_rat_lb a n, cos_rat_up b n
  (* x >= -pi_lb and x <= pi_lb *)
  else if (Q.geq a (neg pi_lb)) && (Q.leq b pi_lb)
  then min (cos_rat_lb a n) (cos_rat_lb b n), Q.one
  else neg Q.one, Q.one
        
let tan_inter_fun n a b =
  let pi_lb, _ (* pi_up *) = pi_inter_cst (n+5) in
  if a >= -. pi_lb /. 2. && b <= pi_lb /. 2. then
    (sin_n_lb a (n+5)) /. (cos_n_up a (n+5)),
    (sin_n_up b (n+5)) /. (cos_n_lb b (n+5))
  else
    assert false



(* Test functions *)
let check_fun fname forig flb fup =
  let ns = List.init 4 (fun x -> x + 1) in
  fun x -> (
    Format.printf "@[<v 3>%s(%f):@ " fname x;
    List.iter (fun n ->
        Format.printf "%f <= %f <= %f@ " (flb x n) (forig x) (fup x n)
      ) ns;
    Format.printf "@]@ "
  )
             (*
  let _ =
    List.iter (check_fun "sqrt" sqrt sqrt_n_lb sqrt_n_up) [0.; 1.; 2.];
    List.iter (check_fun "sin" sin sin_n_lb sin_n_up) [0.; 1.; 2.; 3.];
    List.iter (check_fun "cos" cos cos_n_lb cos_n_up) [0.; 1.; 2.; 3.];
    List.iter (check_fun "atan" atan atan_n_lb atan_n_up) [0.; 0.5; 1.; 2.];
    List.iter (check_fun "pi"  (fun _ -> Float.pi) (fun _ -> pi_n_lb) (fun _ -> pi_n_up)) [0.];
    ()
              *)
