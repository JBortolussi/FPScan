
module type BASE =
sig
  type t
  val parse_param: string -> unit
  val is_infinite: t -> bool
  val min_val: t
  val max_val: t
  val order: t -> t -> bool
  val to_string: t -> string

  val min_lb: t -> t -> t
  val max_ub: t -> t -> t
  val min_ub: t -> t -> t
  val max_lb: t -> t -> t

  val zero: t
  val one: t 
  val minus_one: t 
  val inj_lb: Q.t -> t
  val inj_ub: Q.t -> t

  val add_lb: t -> t -> t
  val add_ub: t -> t -> t
  val sub_lb: t -> t -> t
  val sub_ub: t -> t -> t
  val mul_lb: t -> t -> t
  val mul_ub: t -> t -> t
  val div_lb: t -> t -> t
  val div_ub: t -> t -> t

  val sqrt_lb: t -> t
  val sqrt_ub: t -> t
  val pow_lb: t -> t -> t
  val pow_ub: t -> t -> t
  val tanh_lb: t -> t
  val tanh_ub: t -> t

  val sqrt_inter_fun: t -> t -> t * t
  val atan_inter_fun: t -> t -> t * t
  val cos_inter_fun: t -> t -> t * t
  val sin_inter_fun: t -> t -> t * t
  val exp_inter_fun: t -> t -> t * t
  val ln_inter_fun: t -> t -> t * t
  val tan_inter_fun: t -> t -> t * t
  
    
  val remainder: t -> t -> t

  val to_scalar: t -> Scalar.t 
end


  (**** Bases ****)

module InfIntBase =
struct
  include InfInt
  let parse_param _ = ()
  let min_val = minfty
  let max_val = pinfty
  let is_infinite x = x = minfty || x = pinfty
  let minus_one = fin(-1)
  (* Computation are exact, _lb and _ub versions coincide *)
                
  let inj_lb x = try fin64 (Q.to_int64 x) with Z.Overflow -> min_val
  let inj_ub x = try fin64 (Q.to_int64 x) with Z.Overflow -> max_val
  let mul_lb a b = let res = mul_lb a b in Format.eprintf "%s *lb %s = %s@." (to_string a) (to_string b) (to_string res); res
  let mul_ub a b = let res = mul_ub a b in Format.eprintf "%s *ub %s = %s@." (to_string a) (to_string b) (to_string res); res
  
  let min_ub = min
  let max_ub = max
  let min_lb = min
  let max_lb = max
  (*  let min_lb a b = min (inj_lb a) (inj_lb b)
  let max_ub a b = max (inj_ub a) (inj_ub b)
  let min_ub a b = min (inj_ub a) (inj_ub b)
  let max_lb a b = max (inj_lb a) (inj_lb b)
  *)
  let lift f x =
    if InfInt.order x InfInt.minfty || InfInt.order InfInt.pinfty x then
      x
    else
      f (InfInt.to_scalar x)
    
  let sqrt_lb = lift (fun f -> f |> Scalar.to_float |> sqrt
                               |> Float.floor |> Int64.of_float |> InfInt.fin64)
  let sqrt_ub = lift (fun f -> f |> Scalar.to_float |> sqrt
                               |> Float.ceil |> Int64.of_float |> InfInt.fin64)
  let pow_lb x y =  (* if any of x and y is infty, returns minfty, if both finite, compute the value and floor it *)
    if InfInt.order x InfInt.minfty || InfInt.order InfInt.pinfty x ||
       InfInt.order y InfInt.minfty || InfInt.order InfInt.pinfty y
    then
      InfInt.minfty
    else
      let r = ((Scalar.to_float (InfInt.to_scalar x)) ** (Scalar.to_float (InfInt.to_scalar y))) in
      r |> Float.floor |> Int64.of_float |> InfInt.fin64
    
  let pow_ub x y =
    if InfInt.order x InfInt.minfty || InfInt.order InfInt.pinfty x ||
       InfInt.order y InfInt.minfty || InfInt.order InfInt.pinfty y
    then
      InfInt.pinfty
    else
      let r = ((Scalar.to_float (InfInt.to_scalar x)) ** (Scalar.to_float (InfInt.to_scalar y))) in
      r |> Float.ceil |> Int64.of_float |> InfInt.fin64
    
  let tanh_lb _ = assert false
  let tanh_ub _ = assert false
  let sqrt_inter_fun _ _ = assert false
  let atan_inter_fun _ _ = assert false
  let cos_inter_fun _ _ = assert false
  let sin_inter_fun _ _ = assert false
  let exp_inter_fun _ _ = assert false
  let ln_inter_fun _ _ = assert false
  let tan_inter_fun _ _ = assert false

  (* Integer remainder *)
  let remainder x y = match is_infinite x, is_infinite y,
                            InfInt.to_int x, InfInt.to_int y with
  | false, false, Some x, Some y -> InfInt.fin (x mod y)
  | _ -> InfInt.pinfty (* is this sound? TODO *)
  let to_scalar x = InfInt.to_scalar x

end

module RatBase = 
  struct
    let depth_n_real_operators = ref 4
    let print_mode_as_float = ref false
    let parse_param s =
      match s with
      | "as_float" -> print_mode_as_float := true
      | _ -> () (* other elements are ignored *)
        
    include Q
    let minus_one = Q.of_int (-1)
    let is_infinite x = match Q.classify x with INF | MINF -> true | _ -> false
    let min_val = Q.minus_inf
    let max_val = Q.inf
    let order = Q.leq
    let inj_lb x = x
    let inj_ub x = x
    let add_lb = add
    let add_ub = add
    let sub_lb = sub
    let sub_ub = sub
    let mul_lb = mul
    let mul_ub = mul
    let div_lb = div
    let div_ub = div
    let min_ub = min
    let max_ub = max
    let min_lb = min
    let max_lb = max
    let sqrt_lb _ = Q.inf
    let sqrt_ub _ = Q.inf (* both could be improved *)

    let tanh_lb _ = Q.of_int (-1)
    let tanh_ub _ = Q.of_int 1 (* both could be improved *)

    let pow_lb _ _ = min_val
    let pow_ub _ _ = max_val

    let sqrt_inter_fun a b = Intervals_real_operators.sqrt_rat_inter_fun !depth_n_real_operators a b
    let atan_inter_fun a b = Intervals_real_operators.atan_rat_inter_fun !depth_n_real_operators a b
    let cos_inter_fun a b = Intervals_real_operators.cos_rat_inter_fun !depth_n_real_operators a b
    let sin_inter_fun a b = Intervals_real_operators.sin_rat_inter_fun !depth_n_real_operators a b
    let exp_inter_fun _ _ = assert false
    let ln_inter_fun _ _ = assert false
    let tan_inter_fun _ _ = assert false

    let remainder _ _ = max_val (* TODO : is it safe ? *)
    let to_scalar x = Scalar.of_q x
    let to_string x =
      if !print_mode_as_float then
        let f: float = to_float x in
        (* if f = max_float then "+oo"
         * else if f = -. max_float then "-oo"
         * else *) string_of_float f
      else
        to_string x
  end

module DoubleBase =
  struct

    let depth_n_real_operators = ref 4 (* 4 seems like a sweet spots, a higher value may cause numerical errors *)
      
    let parse_param _ = ()
    type t = float
    let zero = 0.
    let one = 1.
    let minus_one = -1.
    let min_ub = min
    let max_ub = max
    let min_lb = min
    let max_lb = max
    let to_string x = if x = max_float then "+oo"
                      else if x = -. max_float then "-oo"
                      else string_of_float x
    let is_infinite x = match classify_float x with FP_infinite -> true | _ -> x = max_float || x = -. max_float
    let min_val = -. max_float
    let max_val = max_float
    let order = (<=)
    let inj_lb x = (Q.to_float x)
    let inj_ub x = (Q.to_float x)
    let add_lb =  (+.)
    let add_ub =  (+.)
    let sub_lb =  (-.)
    let sub_ub =  (-.)
    let mul_lb =  ( *.)
    let mul_ub =  ( *.)
    let div_lb =  (/.)
    let div_ub =  (/.)
    let sqrt_lb x = max 0. (sqrt x) 
    let sqrt_ub = sqrt
    let pow_lb x y  = x ** y
    let pow_ub x y  = x ** y
    let tanh_lb = tanh 
    let tanh_ub = tanh
    let remainder = mod_float
    let to_scalar x = Scalar.of_float x
                    
    let sqrt_inter_fun a b = Intervals_real_operators.sqrt_inter_fun !depth_n_real_operators a b
    let atan_inter_fun a b = Intervals_real_operators.atan_inter_fun !depth_n_real_operators a b
    let cos_inter_fun a b = Intervals_real_operators.cos_inter_fun !depth_n_real_operators a b
    let sin_inter_fun a b = Intervals_real_operators.sin_inter_fun !depth_n_real_operators a b
    let exp_inter_fun a b = Intervals_real_operators.exp_inter_fun !depth_n_real_operators a b
    let ln_inter_fun a b = Intervals_real_operators.ln_inter_fun !depth_n_real_operators a b
    let tan_inter_fun a b = Intervals_real_operators.tan_inter_fun !depth_n_real_operators a b

  end

module DoubleRndBase =
  struct
    type t = float
           
    let depth_n_real_operators = ref 4 (* 4 seems like a sweet spots, a higher value may cause numerical errors *)

    let parse_param _ = ()
    let zero = 0.
    let one = 1.
    let minus_one = -1.
    let min_ub = min
    let max_ub = max
    let min_lb = min
    let max_lb = max
    let to_string = string_of_float
    let is_infinite x = match classify_float x with FP_infinite -> true | _ -> false
    let min_val = -. max_float
    let max_val = max_float
    let order = (<=)
    let inj_lb x = Floats.next_down (Q.to_float x)
    let inj_ub x = Floats.next_up (Q.to_float x)
    let bin_op rnd op = fun x y -> rnd (op x y)
    let add_lb = bin_op Floats.next_down (+.)
    let add_ub = bin_op Floats.next_up (+.)
    let sub_lb = bin_op Floats.next_down (-.)
    let sub_ub = bin_op Floats.next_up (-.)
    let mul_lb = bin_op Floats.next_down ( *.)
    let mul_ub = bin_op Floats.next_up ( *.)
    let div_lb = bin_op Floats.next_down (/.)
    let div_ub = bin_op Floats.next_up (/.)
    let sqrt_lb x = max 0. (Floats.next_down (sqrt x))
    let sqrt_ub x = Floats.next_up (sqrt x)
    let pow_lb x y = Floats.next_down (x ** y)
    let pow_ub x y = Floats.next_up (x ** y)
    let tanh_lb x = Floats.next_down (tanh x)
    let tanh_ub x = Floats.next_up (tanh x)
    let remainder x y = mod_float x y
    let to_scalar x = Scalar.of_float x

    let sqrt_inter_fun a b = Intervals_real_operators.sqrt_inter_fun !depth_n_real_operators a b
    let atan_inter_fun a b = Intervals_real_operators.atan_inter_fun !depth_n_real_operators a b
    let cos_inter_fun a b = Intervals_real_operators.cos_inter_fun !depth_n_real_operators a b
    let sin_inter_fun a b = Intervals_real_operators.sin_inter_fun !depth_n_real_operators a b
    let exp_inter_fun a b = Intervals_real_operators.exp_inter_fun !depth_n_real_operators a b
    let ln_inter_fun a b = Intervals_real_operators.ln_inter_fun !depth_n_real_operators a b
    let tan_inter_fun a b = Intervals_real_operators.tan_inter_fun !depth_n_real_operators a b

    end
