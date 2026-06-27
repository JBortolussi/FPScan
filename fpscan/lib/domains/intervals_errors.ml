open Intervals
   
module type ERROR = sig

  (* All computions within this ERROR module should be sound real
       computations. Ie rely on proper exterior rounding when required
   *)
  
  type t
  type base_t
  type int_t = base_t interval_t

  (* lb e and ub e return lower and upper bounds of error
       represesent by e *)
  val lb: t -> base_t
  val ub: t -> base_t

  val fprint: Format.formatter -> t -> unit
  val top: t (* top error = you loose *)
  val bottom: t (* ??? *)
  val is_bottom: t -> bool

  val join: t -> t -> t
  val meet: t -> t -> t
  val widening: t -> t -> t (* widening = you loose ! *)

  val sem_itv: (Q.t * string) -> (Q.t * string) -> t
    
  (* sem_plus res e1 e2: computes the error associated to a sum
       (x+e1) + (y+e2) where x+y returned the interval res.  Precise
       error computation shall account for both the summation error of
       x+y (ie associated to the interval res) and with the
       accumulated errors e1 and e2, as weel as the higher order
       errors accounting for e1+e2 ie epsilon * (e1+e2):
       (x+e) + (y+f) * (1+epsilon) =
            (x+y) + (e + f + (x+y) epsilon + (e + f) epsilon      
   *)
  val sem_plus: t -> t -> int_t -> t

  (* sem_minus e1 e2 res: computes the error associated to a
       substraction (x+e1) - (y+e2) where x-y returned the interval
       res. Precise error computation shall account for both the difference
       error of x-y (ie associated to the interval res) and with the
       accumulated errors e1 - e2 + higher order errors 
       (x+e) - (y+f) * (1+epsilon) = 
             (x-y) + (e + f + (x-y) epsilon + (e + f) epsilon 
   *)
  val sem_minus: t -> t -> int_t -> t

  (* sem_times (x,e1) (y,e2) res: computes the error associated to
       a product (x+e1) * (y+e2) where x*y returned the interval
       res.  Precise error computation shall account for both the
       relative error of x*y (ie associated to the interval res),
       with the first order errors x e2 and y e1, the second order
       errors e1 e2 and the absolute error eta (underflow). Some of it
       can be reasonably neglected.  *)
  val sem_times: (int_t * t) -> (int_t * t) -> int_t -> t

  (* sem_div (x,e1) (y,e2) res: computes the error associated to
       a division (x+e1) / (y+e2) with res = x/y 
       = x/y + ((x+e1)/(y+e2) - x/y) 
       = x/y + ((x+e1)y - x(y+e2)) / (y(y+e2)) 
        if y\neq 0 and (y+e2) \neq 0 
        (in practice, [-1,1] should not belong to both y and y+e2, 
         otherwise it would get pretty dirty)
   *)
  val sem_div: (int_t * t) -> (int_t * t) -> int_t -> t

  (* sem_call f args res: computes the error associated to f(args)
       returning interval res. Widely depends on function f. The error
       can be computed unsing a Taylor expansion of epsilon f(args) up
       to a threshold of detection. (See Martel's paper for inverse
       function).  *)
  val sem_call: string -> (int_t * t) list -> int_t -> t

  (* To change the basic error *)
  val set_eps_simple: unit -> unit
end

module SimpleError (Itv: DOMAIN
                    with type elt = float and
                         type t = float interval_t) : ERROR
       with type t = float
        and type base_t = float
        and type int_t = float interval_t =
  struct
    (* Neglecting second order errors *)

    (* Computation have to be performed in a sound manner, ie
         overapproximating floating point errors. *)
    
    type t = float (* \gamma e = [-e, e] *)
    type base_t = Itv.elt
    type int_t = Itv.t
    let to_itv e = Itv.of_bounds (-. e) e

    let epsilon_simple = ldexp 1. (-24)  (*2^-23 / 2  to obtain half ulp(1) *)
    let epsilon_double = ldexp 1. (-53)  (* 2^-52 / 2  to obtain half ulp(1) *)
    let eps = ref epsilon_double
    let set_eps_simple () = eps := epsilon_simple
(*
      (* 2^-23 for float, 2^-52 for double *)
      (*ldexp 1. (-24) *) (* 2^-23 / 2  to obtain half ulp(1) *)
      ldexp 1. (-52)
 *)
    (*epsilon_float*)
      
    let lb x = -. x
    let ub x = x
             
    let fprint fmt x = if x = -1. then
                         Format.fprintf fmt "⊥"
                       else
                         Format.fprintf fmt "±%E" x
    let top = max_float
    let bottom = -. 1.
    let is_bottom x = x = -1.
    let join x y = max x y
    let meet x y = min x y
    let widening x y = if y > x then top else x
                     
    let sem_itv (q1, _) (q2, _) =
      let q = Q.max (Q.abs q1) (Q.abs q2) in
      !eps *. (Q.to_float q)

    (* Returns the magnitude of the interval values:
         max(abs(bounds)). Has to be non negative. *)
    let itv_order x =
      if Itv.is_bottom x then
        raise (Failure ("itv_error in error module: " ^
                          "bottom value, could not extract bounds") 
          )
      else
        let a, b = Itv.bounds x in
        (max (abs_float a) (abs_float b))

        
    let itv_err x =
      if Itv.is_bottom x then
        bottom
      else
        !eps *. (itv_order x)
      
    let sem_plus e1 e2 res =
      let e_res = itv_err res in
      if List.exists is_bottom [e1; e2; e_res] then
        bottom
      else
        e1 +. e2 +. e_res

    let sem_minus e1 e2 res =
      let e_res = itv_err res in
      if List.exists is_bottom [e1; e2; e_res] then
        bottom
      else (*[-e1,e1] - [-e2, e2] = [-e1-e2, e1+e2]*)
        e1 +. e2 +. e_res

    let sem_times (x,e1) (y,e2) res =
      let e_res = itv_err res in
      if List.exists is_bottom [e1; e2; e_res] then
        bottom
      else
        (* e1e2 are higher order errors: neglected here. Did not
             cover eta either. *)
        e_res +. e1 *. (itv_order x) +. e2 *. (itv_order y)

    let sem_div (x,e1) (y,e2) _ =
      (* No use of computed res *)
      if List.exists is_bottom [e1; e2] || List.exists Itv.is_bottom [x;y] then
        bottom
      else
        (* (x+e1) / (y+e2) = x/y + ((x+e1)y - x(y+e2)) / (y(y+e2)) *)
        (* Interval computation, then keep the max value *)
        let itv =
          let itve1 = to_itv e1 in
          let itve2 = to_itv e2 in
          let x_plus_e1 = Itv.sem_plus x itve1 in
          let y_plus_e2 = Itv.sem_plus y itve2 in
          Itv.sem_div
            (Itv.sem_minus (Itv.sem_times x_plus_e1 y) (Itv.sem_times x y_plus_e2))
            (Itv.sem_times y y_plus_e2)
        in
        itv_order itv 
        
    (* sem_call f args res: computes the error associated to f(args)
         returning interval res. Widely depends on function f. The
         error can be computed unsing a Taylor expansion of epsilon
         f(args) up to a threshold of detection. (See Martel's paper
         for inverse function).  *)
    (* For the moment, all non arithmetic function are already
         soundly approximated, however without identifying the error
         part. We keep 0 for the moment.  *)
    let sem_call f args _ (*res*) =
      match f, args with
      | _ -> (* TODO UNSOUND *)
         0. (* top *)
        
  end
    
          
module MakeItvError
         (Itv: DOMAIN with
            type elt = float and
            type t = float interval_t) 
     
  = 
  struct
    module E = SimpleError (Itv) 
    type t = Itv.t * E.t


    let name = Itv.name ^ "_err"

    let base_type = Itv.base_type

    (* One can change the base error *)
    let parse_param s =
      match s with
      | "float" -> E.set_eps_simple () 
      | "double" -> ()
      | _ -> failwith ("Unrecognized option: " ^ s)

    let fprint_help fmt = Format.fprintf fmt
                            "@[Interval with error abstraction:@ %t@ @]"
                            Itv.fprint_help (*Err.fprint_help*)
    let log = false

    let fprint ff (x,e) =
      Format.fprintf ff "%a + %a" Itv.fprint x E.fprint e

    let json t = `String (Format.asprintf "%a" fprint t)

    let lb (x,e) = match x with
      | Bot -> assert false
      | Itv(a,_ (* UNUSED: b *)) -> (+.) a (E.lb e)

    let ub (x,e) = match x with
      | Bot -> assert false
      | Itv(_ (* UNUSED: a *),b) -> (+.) b (E.ub e)

    let to_bounds t =
      Bounds.mk (Scalar.of_float (lb t)) (Scalar.of_float (ub t))
      
    let order (x,e) (y,f) =
      (* TODO UNSOUND: provides safe versions: ie rounded towards +oo *)
      let add_lb = (+.) and add_ub = (+.) in
      let order = (<=) in
      match x, y with
      | Bot, _ -> true
      | _, Bot -> false
      | Itv (a, b), Itv (c, d) ->
         order (add_lb c (E.lb f)) (add_lb a (E.lb e)) &&
           order (add_ub b (E.ub e)) (add_ub d (E.ub f)) 
        

    let top = Itv.top, E.top
    let bottom = Itv.bottom, E.bottom
    let is_bottom (x,e) = Itv.is_bottom x || E.is_bottom e
                        
    let join (x,e) (y,f) =
      Itv.join x y, E.join e f
      
    let meet (x,e) (y,f) =
      Itv.meet x y, E.meet e f
      
    let widening (x,e) (y,f) =
      Itv.widening x y, E.widening e f 
      
    let sem_itv a b =
      Itv.sem_itv a b, E.sem_itv a b

    let sem_plus (x,e) (y,f) =
      let res = Itv.sem_plus x y in
      let err = E.sem_plus e f res in
      res, err

      
    let sem_minus (x,e) (y,f) = 
      let res = Itv.sem_minus x y in
      let err = E.sem_minus e f res in
      res, err
      
    let sem_times (x,e) (y,f) =
      let res = Itv.sem_times x y in
      let err = E.sem_times (x,e) (y,f) res in 
      res, err

    let sem_div (x,e) (y,f) =
      let res = Itv.sem_div x y in
      let err = E.sem_div (x,e) (y,f) res in
      res, err
      
    (* Constraining positivity doesn't impact errors. *)
    let sem_geq0 (x,e) =
      (* TODO : To overapproximate the constraint, we add x+e, constrain it
           to be positive or null, and if non bottom, intersect x with
           the value obtain. The error stay similar *)
      (* Unsound version: just constraint x and if bottom, propagate *)
      let xpos = Itv.sem_geq0 x in
      if Itv.is_bottom xpos then
        bottom
      else
        xpos, e
      
    let sem_call f args =
      let values = List.map fst args in
      let res = Itv.sem_call f values in
      let err = E.sem_call f args res in
      res, err

    (* Don't know if one can really improve the error part in
         backsems.

         TODO: x = r - y: compute a safe approx of r and y,
         ie. safe(r, e_r), safe(y, e_y). Then since safe(x,e_x) =
         safe(r, e_r) - safe(y, e_y), we could constrain (x,e_x) by
         safe(r, e_r) - safe(y, e_y) - e_x.

         TODO: is this correct ? 
     *)
    let backsem_plus x y r =
      if is_bottom r then
        bottom, bottom
      else
        x, y

    let backsem_minus x y r =
      if is_bottom r then
        bottom, bottom
      else
        x, y

    let backsem_times x y r =
      if is_bottom r then
        bottom, bottom
      else
        x, y
      
    let backsem_div x y r =
      if is_bottom r then
        bottom, bottom
      else
        x, y

    let to_properties _x = Value_properties.empty (* TODO *)

    let split (x,e) = (* keeping all errors *)
      match Itv.split x with
      | None -> None
      | Some (x1, x2) -> Some ((x1,e), (x2,e))
  end

  module Main = MakeItvError(Double) 
