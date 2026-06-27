(* Interval based floating-point analyse including errors *)

(* Float format *)
module type RawFloatFormat =
  sig
    val name: string
    val exponent: int
    val mantissa: int
  end

module type FloatFormat = 
  sig
    val name: string
    val exponent: int
    val p: int
    val emin: int
    val emax: int
    val min: Q.t
    val min_norm: Q.t
    val max: Q.t
    val epsilon: Q.t
    val eta: Q.t
  end

module MakeFloatFormat(F: RawFloatFormat) : FloatFormat = struct
  let mul_2exp q e =
    if e < 0
    then
      Q.div q (Q.mul_2exp Q.one (-e))
    else
      Q.mul_2exp q e
  let name = F.name
  let exponent = F.exponent
  let p = F.mantissa + 1
  let emin = - (Int.shift_left 1 (exponent - 1)) + 2
  let emax = (Int.shift_left 1 (exponent - 1)) -1
  let min = mul_2exp Q.one (emin - p)
  let min_norm = mul_2exp Q.one emin
  let max = Q.mul 
              (mul_2exp Q.one (emax + 1)) 
              (
                Q.sub
                  Q.one
                  (mul_2exp Q.one (- p)) 
              )

  (* epsilon = 2^-p *)
  let epsilon = mul_2exp (Q.one) (- p)

  (* eta = 2^(emin-p) *)
  let eta = mul_2exp (Q.one) (emin - p)
end
  
module MakeIntervalFloatError
          (F: FloatFormat) 
  =
  struct
    module Itv = Intervals.Rat

    let name = "interval_float_" ^ F.name

    let parse_param _ = ()
    let fprint_help fmt = Format.fprintf fmt "Parametrizable floating-point numbers abstraction with interval & special values"
    let log = false

    let json _ = assert false

    (* Abstract values 
     * isNan, isMInf, isPInf, Itv.t
    *)
    type t = bool * bool * bool * Itv.t

    let bottom = (false, false, false, Itv.bottom)
    let top_itv = Itv.sem_itv (Q.neg F.max, "") (F.max, "")
    let top = (true, true, true, top_itv)

    let is_bottom (isNan1, isMInf1, isPInf1, itv1) =
      (not isNan1) && (not isMInf1) && (not isPInf1) && (Itv.is_bottom itv1)

    let base_type = Ast.RealT

    let fprint ff = function
      | (isNan, isMInf, isPInf, itv) ->
        begin
          if isNan  then Format.fprintf ff "NaN ⊔ ";
          if isMInf then Format.fprintf ff "-∞ ⊔ ";
          if isPInf then Format.fprintf ff "+∞ ⊔ ";
          Itv.fprint ff itv
        end

    let _json t = `String (Format.asprintf "%a" fprint t)

    let rounding_err x = 
      let x = Q.abs x in
      (* denormalized number *)
      if Q.lt x F.min_norm
      then F.eta
      else Q.mul x F.epsilon
    
    let itv_expand_error itv =
      (* itv *)
      if Itv.is_bottom itv
      then
        itv
      else
        let l, u = Itv.bounds itv in
        let el = rounding_err l in
        let eu = rounding_err u in
        Itv.of_bounds (Q.sub l el) (Q.add u eu)
    
    let order (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
          (isNan1 <= isNan2) 
      &&  (isMInf1 <= isMInf2) 
      &&  (isPInf1 <= isPInf2) 
      &&  Itv.order itv1 itv2

    let join (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      (
        isNan1 || isNan2, 
        isMInf1 || isMInf2, 
        isPInf1 || isPInf2, 
        Itv.join itv1 itv2
      )

    (* let meet (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      (
        isNan1 && isNan2, 
        isMInf1 && isMInf2, 
        isPInf1 && isPInf2, 
        Itv.meet itv1 itv2
      ) *)
    
    (* let itv_sem_itv n1 n2 = Itv.sem_itv (Q.of_float n1, "") (Q.of_float n2, "") *)

    let is_value (_, _, _, itv) = not (Itv.is_bottom itv)
    
    let is_pos_value (_, _, _, itv1) =
      if not (Itv.is_bottom itv1)
      then
        let l, _ = Itv.bounds itv1 in
        Q.geq l Q.zero
      else
        false

    (* teste si une valeur négative finie est représentée dans l'élément abstrait *)
    let is_neg_value (_, _, _, itv1) =
      if not (Itv.is_bottom itv1)
      then
        let _, u = Itv.bounds itv1 in
        Q.leq u Q.zero
      else
        false
        
    let is_pos ((_, _, isPInf1, _) as x) =
      isPInf1 || (is_pos_value x)

    (* teste si une valeur négative (ou -oo) est représentée dans l'élément abstrait *)
    let is_neg ((_, isMInf1, _, _) as x) =
      isMInf1 || (is_neg_value x)

    (* teste si zéro est représenté dans l'élément abstrait *)
    let is_zero (_, _, _, itv1) =
      if Itv.is_bottom itv1
      then
        false
      else
        let l, u = Itv.bounds itv1 in
        ((Q.sign l) * (Q.sign u)) <= 0
    
    let is_inf (_, isMInf, isPInf, _) =
      isMInf || isPInf

    let sem_itv n1 n2 =
      let v1 = fst n1 in
      let v2 = fst n2 in
      (* Bound are reversed *)
      if Q.gt v1 v2
      then bottom
      else (
        (* NaN *)
        false,
        (* MInf *)
        Q.lt v1 (Q.neg F.max),
        (* PInf *)
        Q.gt v2 F.max,
        (* Itv *)
        Itv.meet (Itv.sem_itv n1 n2) top_itv
      )
    
    let inj_itv itv =
      if Itv.is_bottom itv
      then (false, false, false, itv)
      else (
        let l, u = Itv.bounds itv in
        let isMinf = Q.lt l (Q.neg F.max) in
        let isMax = Q.gt u F.max in
        (false, isMinf, isMax, Itv.meet itv top_itv)
      )

    let widening (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      join
        (inj_itv (Itv.widening itv1 itv2))
        (isNan1 || isNan2, isMInf1 || isMInf2, isPInf1 || isPInf2, Itv.bottom)

    let join (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      (isNan1 || isNan2, isMInf1 || isMInf2, isPInf1 || isPInf2, Itv.join itv1 itv2)

    let meet (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      (isNan1 && isNan2, isMInf1 && isMInf2, isPInf1 && isPInf2, Itv.meet itv1 itv2)

    let sem_plus
          ((isNan1, isMInf1, isPInf1, itv1) as x)
          ((isNan2, isMInf2, isPInf2, itv2) as y)
      =
      let isNan = 
        isNan1 || isNan2 || (isPInf1 && isMInf2) || (isMInf1 && isPInf2) in
      let isMInf =
        (isMInf1 && isMInf2) || (isMInf1 && is_value y) || (isMInf2 && is_value x) in
      let isPInf =
        (isPInf1 && isPInf2) || (isPInf1 && is_value y) || (isPInf2 && is_value x) in
      join
        ((Itv.sem_plus itv1 itv2) |> itv_expand_error |> inj_itv)
        (isNan, isMInf, isPInf, Itv.bottom)
    
    let sem_minus ((isNan1, isMInf1, isPInf1, itv1) as x) ((isNan2, isMInf2, isPInf2, itv2) as y) =
      let isNan =
        isNan1 || isNan2 || (isPInf1 && isPInf2) || (isMInf1 && isMInf2) in
      let isMInf =
        (isMInf1 && isPInf2) || (isMInf1 && is_value y) || (isPInf2 && is_value x) in
      let isPInf =
        (isPInf1 && isMInf2) || (isPInf1 && is_value y) || (isMInf2 && is_value x) in
      join
        ((Itv.sem_minus itv1 itv2) |> itv_expand_error |> inj_itv)
        (isNan, isMInf, isPInf, Itv.bottom)

    let sem_times ((isNan1, isMInf1, isPInf1, itv1) as x) ((isNan2, isMInf2, isPInf2, itv2) as y) =
      let isNan =
        isNan1 || isNan2 || (is_inf x && is_zero y) || (is_zero x && is_inf y) in
      let isMInf =
        (is_pos x && isMInf2) || (is_pos y && isMInf1) || (is_neg x && isPInf2) || (is_neg y && isPInf1) in
      let isPInf =
        (is_pos x && isPInf2) || (is_pos y && isPInf1) || (is_neg x && isMInf2) || (is_neg y && isMInf1) in
      join
        ((Itv.sem_times itv1 itv2) |> itv_expand_error |> inj_itv)
        (isNan, isMInf, isPInf, Itv.bottom)

    let sem_div ((isNan1, isMInf1, isPInf1, itv1) as x) ((isNan2, _, _, itv2) as y) =
      let isNan =
        isNan1 || isNan2 || (is_zero x && is_zero y) || (is_inf x && is_inf y) in
      let isMInf =
        (isMInf1 && is_pos_value y) || (isPInf1 && is_neg_value y) || (is_neg x && is_zero y) in
      let isPInf =
        (isPInf1 && is_pos_value y) || (isMInf1 && is_neg_value y) || (is_pos x && is_zero y) in
      let is_zero =
        is_value x && is_inf y in
      join
        ((Itv.sem_div itv1 itv2) |> itv_expand_error |> inj_itv)
        (isNan, isMInf, isPInf, if is_zero then Itv.of_bounds Q.zero Q.zero else Itv.bottom)
    
    let sem_geq0 (_, _, isPInf, itv)=
      (false, false, isPInf, Itv.sem_geq0 itv)

    let sem_call f args =
      if List.exists is_bottom args then bottom else
      match f, args with
        | "cos", [isNan, isMInf, isPInf, itv] -> (
          let a, b = Itv.bounds itv in
          let a, b = Bases.RatBase.cos_inter_fun a b in
          (isNan, isMInf, isPInf, Itv.of_bounds a b)
        )
        | "sin", [isNan, isMInf, isPInf, itv] -> (
          let a, b = Itv.bounds itv in
          let a, b = Bases.RatBase.sin_inter_fun a b in
          (isNan, isMInf, isPInf, Itv.of_bounds a b)
        )
        | "atan", [isNan, isMInf, isPInf, itv] -> (
          let a, b = Itv.bounds itv in
          let a, b = Bases.RatBase.atan_inter_fun a b in
          (isNan, isMInf, isPInf, Itv.of_bounds a b)
        )
        | "sqrt", [isNan, isMInf, isPInf, itv] -> (
          let a, b = Itv.bounds itv  in
          let a, b = Bases.RatBase.sqrt_inter_fun a b in
          (isNan, isMInf, isPInf, Itv.of_bounds a b)
        )
        | _ -> failwith ("Function " ^ f ^ " not yet implemented in intervals")

    let backsem_plus x y r =
      (meet x (sem_minus r y), meet y (sem_minus r x))

    let backsem_minus x y r =
      (meet x (sem_plus r y), meet y (sem_minus x r))

    let backsem_times x y r =
      (meet x (sem_div r y), meet y (sem_div r x))

    let backsem_div x y r =
      (meet x (sem_times r y), meet y (sem_div x r))

    let to_bounds (_, _, _, itv) = Itv.to_bounds itv (* TODO *)
    let split _ = assert false (* TODO *)

    let to_properties ((isNan, isMInf, isPInf, _itv):t) : Value_properties.t =
      let module VP = Value_properties in
      let props = VP.empty in
      let props =
        if isNan then
          VP.add ["NaN"] VP.(Bool true) props
        else
          props
      in
      let props =
        if isMInf then
          VP.add ["MInf"] VP.(Bool true) props
        else
          props
      in
      let props =
        if isPInf then
          VP.add ["PInf"] VP.(Bool true) props
        else
          props
      in
      props

  end

  module F16 = MakeFloatFormat (struct
    let name="f16" let exponent = 5 let mantissa = 10
  end)

  module F32 = MakeFloatFormat (struct
    let name = "f32" let exponent = 8 let mantissa = 23
  end)

  module IntervalF16Error = MakeIntervalFloatError(F16)
  module IntervalF32Error = MakeIntervalFloatError(F32)

  (* module IntervalF16ErrorDomain = Relational.MakeRelational(IntervalF16Error) *)
