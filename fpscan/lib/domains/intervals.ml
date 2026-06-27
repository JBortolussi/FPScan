(* Version correcte (sans débordement arithmétiques) de Intervals3. *)

type 'base interval_t = Bot | Itv of 'base * 'base

module type DOMAIN =
  sig
    include NonRelational.Domain
    type elt
    val bounds: t -> elt * elt
    val of_bounds: elt -> elt -> t
  end


module Make
  (I: sig val name_suffix: string val base_type : Ast.base_type end)
  (B: Bases.BASE) =
struct

  let name = "intervals" ^ I.name_suffix
    
  let base_type = I.base_type

  (* no option, but calls it on the underlying type representation *)
  let parse_param s =
    let s_extract =
      let re = Str.regexp (name ^ ":") in
      Str.replace_first re "" s
    in
    B.parse_param s_extract;
    ()
    
  let fprint_help fmt = Format.fprintf fmt "Interval abstraction"

  let log = false

  type t = B.t interval_t
  type elt = B.t    
    
  let fprint ff = function
    | Bot -> Format.fprintf ff "⊥"
    | Itv (a, b) -> Format.fprintf ff "%s%s, %s%s"
       (if B.is_infinite a then "(" else "[")
       (B.to_string a) (B.to_string b)
       (if B.is_infinite b then ")" else "]")

  let json = function
    | Bot -> `String "⊥"
    | Itv (a, b) -> `Assoc ["begin", `String (B.to_string a); "end", `String (B.to_string b)]

  let order x y = match x, y with
    | Bot, _ -> true
    | _, Bot -> false
    | Itv (a, b), Itv (c, d) -> B.order c a && B.order b d

  let top = Itv (B.min_val, B.max_val)
  let bottom = Bot
  let is_bottom x = x = Bot
    
  let join x y = match x, y with
    | Bot, _ -> y
    | _, Bot -> x
    | Itv (a, b), Itv (c, d) -> Itv (B.min_lb a c, B.max_ub b d)

  let meet x y = match x, y with
    | Bot, _
    | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) ->
       let e = B.max_lb a c in
       let f = B.min_ub b d in
       if B.order e f then Itv (e, f)
       else Bot

  let widening x y = match x, y with
    | Bot, _ -> y
    | _, Bot -> x
    | Itv (a, b), Itv (c, d) ->
       let e = if B.order a c then a else B.min_val in
       let f = if B.order d b then b else B.max_val in
         Itv (e, f)

             
  let bounds x = match x with
    | Bot -> raise Utils.IsBottom (*(Failure
                      ("Intervals.bounds on a bottom value"))*)
    | Itv (a,b) -> a,b
 
       
  let of_bounds a b =
    if not (B.order a b)
    then Bot
    else Itv (a, b)
                 
  let sem_itv (n1, _) (n2, _) =
    if Q.gt n1 n2 then Bot
    else Itv (B.inj_lb n1, B.inj_ub n2)

  let sem_plus x y = match x, y with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) -> Itv (B.add_lb a c, B.add_ub b d)

  let sem_minus x y = match x, y with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) -> Itv (B.sub_lb a d, B.sub_ub b c)

  let sem_times x y = match x, y with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) ->
       let e = B.min_lb
	 (B.min_lb (B.mul_lb a c) (B.mul_lb b d))
	 (B.min_lb (B.mul_lb b c) (B.mul_lb a d)) in
       let f = B.max_ub
	 (B.max_ub (B.mul_ub a c) (B.mul_ub b d))
	 (B.max_ub (B.mul_ub b c) (B.mul_ub a d)) in
       Itv (e, f)

	   

  (*********** Division **************)
  
  (* precondition: meet y [0, 0] = ⊥ *)
  let sem_div_without_0 a b  y = match y with
    | Bot -> Bot
    | Itv (c, d) ->
       let e = B.min_lb
		 (B.min_lb (B.div_lb a c) (B.div_lb b d))
		 (B.min_lb (B.div_lb b c) (B.div_lb a d)) in
       let f = B.max_ub
		 (B.max_ub (B.div_ub a c) (B.div_ub b d))
		 (B.max_ub (B.div_ub b c) (B.div_ub a d)) in
       Itv (e, f)

  let sem_div_eucl a b y = 
    let yneg = meet y (Itv (B.min_val, B.minus_one)) in
    let ypos = meet y (Itv (B.one, B.max_val)) in
    join (sem_div_without_0 a b yneg) (sem_div_without_0 a b ypos)
  


  let sem_div_real a b c d =
    if B.order c B.zero && B.order B.zero d then
      (* if [c,d] contains zero we have -oo, +oo *) 
      top
    else
      let e = B.min_lb
		(B.min_lb (B.div_lb a c) (B.div_lb b d))
		(B.min_lb (B.div_lb b c) (B.div_lb a d)) in
      let f = B.max_ub
		(B.max_ub (B.div_ub a c) (B.div_ub b d))
		(B.max_ub (B.div_ub b c) (B.div_ub a d)) in
      Itv (e, f)
	  
				      
  let sem_div x y =
    match x,y with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) ->
       if c = B.zero && d = B.zero then
	 Bot
       else
	 match base_type with
	 | Ast.IntT ->
	    sem_div_eucl a b y      
	 | Ast.RealT ->
	    sem_div_real a b c d 
	 | _ -> assert false
		     
  let sem_geq0 = meet (Itv (B.zero, B.max_val))

                       
  let sem_call f args =
    if List.exists is_bottom args then bottom else
      match f, args with
      | "cos", [Itv(a,b)] -> (
        try
          let ra, rb = B.cos_inter_fun a b in Itv (ra, rb)
        with Utils.OutofDomain -> bottom
      )
      | "sin", [Itv(a,b)] -> (
        try
           let ra, rb = B.sin_inter_fun a b in Itv (ra, rb)
         with Utils.OutofDomain -> bottom
      )
      | "tan", [Itv(a,b)] -> (
        try
           let ra, rb = B.tan_inter_fun a b in Itv (ra, rb)
         with Utils.OutofDomain -> bottom
      )
      | "atan", [Itv(a,b)] -> (
        try
           let ra, rb = B.atan_inter_fun a b in Itv (ra, rb)
         with Utils.OutofDomain -> bottom
      )
      | "exp", [Itv(a,b)] -> (
        try
           let ra, rb = B.exp_inter_fun a b in Itv (ra, rb)
         with Utils.OutofDomain -> bottom
      )
      | "ln", [Itv(a,b)] -> (
        try
           let ra, rb = B.ln_inter_fun a b in Itv (ra, rb)
         with Utils.OutofDomain -> bottom
      )
      | "tanh", [Itv(a,b)] ->
         Itv(B.tanh_lb a, B.tanh_ub b)
      | "atan2", _ -> (* should be [-\pi, pi] *)
         Itv (B.inj_lb (Q.of_int(-4)),
              B.inj_ub (Q.of_int 4))
        
      | "sqrt", [Itv (a,b)] -> (
        try
          let ra, rb = B.sqrt_inter_fun a b in Itv (ra, rb)
        with Utils.OutofDomain -> bottom
      )

      | "pow", [x;y] ->
         begin
           (* special case *)
           match x, y with
           | Itv(a,b), Itv(c,d) when c=d ->
              let sup = B.max_ub
                          (B.max_ub (B.pow_ub a c) (B.pow_ub a d))
                          (B. max_ub (B.pow_ub b c) (B.pow_ub b d)) in
              let inf = B.min_lb
                          (B.min_lb (B.pow_lb a c) (B.pow_lb a d))
                          (B.min_lb (B.pow_lb b c) (B.pow_lb b d)) in
              if B.remainder c (B.inj_lb (Q.of_int 2)) = B.zero then (* TODO add a new inj function with the ub, lb suffixes *)
                Itv(max inf B.zero, sup)
              else
                Itv(inf, sup)
           | _ -> top
         end
      | _ -> failwith ("Function " ^ f ^ " not yet implemented in intervals")		   


		      
  let backsem_add_int (a, b) (c, d) (e, f) =
    let min = B.max_lb a (B.sub_lb e d) in
    let max = B.min_ub b (B.sub_ub f c) in
    if B.order min max then Itv (min, max) else Bot

  let backsem_plus x y r = match x, y, r with
    | Itv (a, b), Itv (c, d), Itv (e, f) ->
       backsem_add_int (a, b) (c, d) (e, f),
      backsem_add_int (c, d) (a, b) (e, f)
    | _ -> Bot, Bot

  let backsem_minus_int (a, b) (c, d) (e, f) =
    let min = B.max_lb a (B.add_lb e c) in
    let max = B.min_ub b (B.add_ub f d) in
    if B.order min max then Itv (min, max) else Bot

  let backsem_minus_int' (a, b) (c, d) (e, f) =
    let min = B.max_lb c (B.sub_lb a f) in
    let max = B.min_ub d (B.sub_ub b e) in
    if B.order min max then Itv (min, max) else Bot

  let backsem_minus x y r = match x, y, r with
    | Itv (a, b), Itv (c, d), Itv (e, f) ->
       backsem_minus_int (a, b) (c, d) (e, f),
       backsem_minus_int' (a, b) (c, d) (e, f)
    | _ -> Bot, Bot

  (* Do we need to be more precise? *)
  let backsem_times x y r = match x, y, r with
    | Itv _, Itv _, Itv _ -> x, y
    | _ -> Bot, Bot

  let backsem_div = backsem_times

  let to_bounds x =
    match x with
    | Itv (a,b) -> Bounds.mk (B.to_scalar a) (B.to_scalar b)
    | Bot -> raise Utils.IsBottom(* B.to_scalar B.min_val, B.to_scalar B.max_val *)

  let to_properties _x = Value_properties.empty (* TODO *)

  
  let split x =
    match x with
    | Itv(a,b) ->
       if a = b then None else
         let mid = B.add_ub a (B.div_ub (B.sub_ub b a) (B.inj_ub (Q.of_int 2))) in
         Some (Itv(a,mid), Itv(mid,b))
    | Bot -> None
           
end

  
(**** Instanciations ****)
  
module Int = Make
               (struct
                 let name_suffix = "_infint"
                 let base_type = Ast.IntT
               end)
               (Bases.InfIntBase)

module Rat = Make
               (struct
                 let name_suffix = "_rat"
                 let base_type = Ast.RealT                               
               end)
               (Bases.RatBase)

(* Does not over-approximate floating point errors. Only use in combination with an error domain *)
module Double = Make
                  (struct
                    let name_suffix = "_double"
                    let base_type = Ast.RealT
                  end)
                  (Bases.DoubleBase)
              
(* Unprecise: will always jump to the next floats towards infinities,
   for each operator *)
module DoubleRnd = Make
                     (struct
                       let name_suffix = "_double_rnd"
                       let base_type = Ast.RealT
                     end)
                     (Bases.DoubleRndBase)
