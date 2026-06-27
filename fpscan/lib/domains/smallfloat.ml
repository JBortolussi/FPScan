module type Bounds =
  sig
    val name: string
    val min : float
    val max : float
  end

module Make(B : Bounds) =
  struct
    module Itv = Intervals.Double
    (* Template to write your own non relational abstract domain. *)

    let name = "smallfloats_" ^ B.name
    let parse_param _ = ()
    let fprint_help fmt = Format.fprintf fmt "Parametrizable floating-point numbers abstraction with interval & special values"
    let log = false
    let base_type = Ast.RealT
    let json _ = assert false
               
    (* To implement your own non relational abstract domain,
     * first give the type of its elements, *)
    (* an element is a tuple (isNan, isMInf, isPInf, interval) *)
    type t = bool * bool * bool * Itv.t

    let _ = assert (B.min < 0. && 0. <= B.max)

    (* a printing function (useful for debuging), *)
    let fprint ff = function
      | (isNan, isMInf, isPInf, itv) ->
         begin
           if isNan  then Format.fprintf ff "NaN ⊔ ";
           if isMInf then Format.fprintf ff "-∞ ⊔ ";
           if isPInf then Format.fprintf ff "+∞ ⊔ ";
           Itv.fprint ff itv
         end

    let order (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      (isNan1 <= isNan2) && (isMInf1 <= isMInf2) && (isPInf1 <= isPInf2) && Itv.order itv1 itv2

    (* All the functions below are safe overapproximations.
     * You can keep them as this in a first implementation,
     * then refine them only when you need it to improve
     * the precision of your analyses. *)

    let join (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      (isNan1 || isNan2, isMInf1 || isMInf2, isPInf1 || isPInf2, Itv.join itv1 itv2)

    let meet (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      (isNan1 && isNan2, isMInf1 && isMInf2, isPInf1 && isPInf2, Itv.meet itv1 itv2)

    let itv_sem_itv n1 n2 = Itv.sem_itv (Q.of_float n1, "") (Q.of_float n2, "")

    let b_top = itv_sem_itv B.min B.max
    let bottom = (false, false, false, Itv.bottom)
    let top = (true, true, true, b_top)

    let is_bottom (isNan1, isMInf1, isPInf1, itv1) =
      (not isNan1) && (not isMInf1) && (not isPInf1) && (Itv.is_bottom itv1)

    (* injection d'un intervalle normal dans le type t *)
    (* on écrête les valeurs qui débordent de [B.min, B.max] en les assimilant à -oo et +oo *)
    let sem_itv n1 n2 =
      let v1 = fst n1 in
      let v2 = fst n2 in
      if Q.gt v1 v2 then bottom else (false, Q.lt v1 (Q.of_float B.min), Q.gt v2 (Q.of_float B.max), Itv.meet (Itv.sem_itv n1 n2) b_top)

    (* idem pour le type intervalle abstrait Itv.t *)
    let inj_itv itv =
      let isMInf = not (Itv.is_bottom (Itv.meet itv (itv_sem_itv min_float (B.min -. 1.)))) in
      let isPInf = not (Itv.is_bottom (Itv.meet itv (itv_sem_itv (B.max +. 1.) max_float))) in
      (false, isMInf, isPInf, Itv.meet itv b_top)

    let widening (isNan1, isMInf1, isPInf1, itv1) (isNan2, isMInf2, isPInf2, itv2) =
      join
        (inj_itv (Itv.widening itv1 itv2))
        (isNan1 || isNan2, isMInf1 || isMInf2, isPInf1 || isPInf2, Itv.bottom)

    (* teste si une valeur positive finie est représentée dans l'élément abstrait *)
    let is_pos_value (_, _, _, itv1) =
      not (Itv.is_bottom (Itv.meet itv1 (itv_sem_itv 1. B.max)))

    (* teste si une valeur négative finie est représentée dans l'élément abstrait *)
    let is_neg_value (_, _, _, itv1) =
      not (Itv.is_bottom (Itv.meet itv1 (itv_sem_itv B.min (-.1.))))

    (* teste si une valeur positive (ou +oo) est représentée dans l'élément abstrait *)
    let is_pos ((_, _, isPInf1, _) as x) =
      isPInf1 || (is_pos_value x)

    (* teste si une valeur négative (ou -oo) est représentée dans l'élément abstrait *)
    let is_neg ((_, isMInf1, _, _) as x) =
      isMInf1 || (is_neg_value x)

    (* teste si zéro est représenté dans l'élément abstrait *)
    let is_zero (_, _, _, itv1) =
      not (Itv.is_bottom (Itv.meet itv1 (itv_sem_itv 0. 0.)))
      
    (* teste si une valeur ordinaire (non infinie ou Nan) est représentée dans l'élément abstrait *)
    let is_value (_, _, _, itv1) =
      not (Itv.is_bottom itv1)

    (* teste si un infini (+/-) est représenté dans l'élément abstrait *)
    let is_inf (_, isMInf, isPInf, _) =
      isMInf || isPInf

    (* règles des valeurs spéciales pour l'addition :
       Nan     + _       = _       + Nan     = Nan
       (+/-)oo + v       = v       + (+/-)oo = (+/-)oo (en fonction des signes)
       +oo     + +oo     = +oo
       -oo     + -oo     = -oo
       +oo     + -oo     = -oo     + +oo     = Nan
     *)
    let sem_plus ((isNan1, isMInf1, isPInf1, itv1) as x) ((isNan2, isMInf2, isPInf2, itv2) as y) =
      let isNan =
        isNan1 || isNan2 || (isPInf1 && isMInf2) || (isMInf1 && isPInf2) in
      let isMInf =
        (isMInf1 && isMInf2) || (isMInf1 && is_value y) || (isMInf2 && is_value x) in
      let isPInf =
        (isPInf1 && isPInf2) || (isPInf1 && is_value y) || (isPInf2 && is_value x) in
      join
        (inj_itv (Itv.sem_plus itv1 itv2))
        (isNan, isMInf, isPInf, Itv.bottom)

    (* règles des valeurs spéciales pour la soustraction :
       Nan     - _       = _       - Nan     = Nan
       (+/-)oo - v       = v       - (+/-)oo = (+/-)oo (en fonction des signes)
       +oo     - -oo     = +oo
       -oo     - +oo     = -oo
       +oo     - +oo     = -oo     - -oo     = Nan
     *)
    let sem_minus ((isNan1, isMInf1, isPInf1, itv1) as x) ((isNan2, isMInf2, isPInf2, itv2) as y) =
      let isNan =
        isNan1 || isNan2 || (isPInf1 && isPInf2) || (isMInf1 && isMInf2) in
      let isMInf =
        (isMInf1 && isPInf2) || (isMInf1 && is_value y) || (isPInf2 && is_value x) in
      let isPInf =
        (isPInf1 && isMInf2) || (isPInf1 && is_value y) || (isMInf2 && is_value x) in
      join
        (inj_itv (Itv.sem_minus itv1 itv2))
        (isNan, isMInf, isPInf, Itv.bottom)

    (* règles des valeurs spéciales pour la multiplication :
       Nan     * _       = _       * Nan     = Nan
       (+/-)oo * 0       = 0       * (+/-)oo = Nan
       v       * (+/-)oo = (+/-)oo * v       = (+/-)oo (en fonction des signes, pour v!=0)
       (+/-)oo * (+/-)oo = (+/-)oo (en fonction des signes)
     *)
    let sem_times ((isNan1, isMInf1, isPInf1, itv1) as x) ((isNan2, isMInf2, isPInf2, itv2) as y) =
      let isNan =
        isNan1 || isNan2 || (is_inf x && is_zero y) || (is_zero x && is_inf y) in
      let isMInf =
        (is_pos x && isMInf2) || (is_pos y && isMInf1) || (is_neg x && isPInf2) || (is_neg y && isPInf1) in
      let isPInf =
        (is_pos x && isPInf2) || (is_pos y && isPInf1) || (is_neg x && isMInf2) || (is_neg y && isMInf1) in
      join
        (inj_itv (Itv.sem_times itv1 itv2))
        (isNan, isMInf, isPInf, Itv.bottom)

    (* règles des valeurs spéciales pour la division :
       Nan     / _       = _       / Nan = Nan
       0       / 0       = Nan
       v       / 0       = (+/-)oo (en fonction du signe de v, pour v!=0)
       (+/-)oo / v       = (+/-)oo (en fonction des signes)
       v       / (+/-)oo = 0
       (+/-)oo / (+/-)oo = Nan
     *)
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
        (inj_itv (Itv.sem_div itv1 itv2))
        (isNan, isMInf, isPInf, if is_zero then itv_sem_itv 0. 0. else Itv.bottom)

    (* un nombre est >=0 s'il est +oo ou une valeur >=0 *)
    let sem_geq0 (_, _, isPInf1, itv1) =
      (false, false, isPInf1, Itv.sem_geq0 itv1)

    let sem_call _ _ = assert false

    let backsem_plus x y r =
      (meet x (sem_minus r y), meet y (sem_minus r x))

    let backsem_minus x y r =
      (meet x (sem_plus r y), meet y (sem_minus x r))

    let backsem_times x y r =
      (meet x (sem_div r y), meet y (sem_div r x))

    let backsem_div x y r =
      (meet x (sem_times r y), meet y (sem_div x r))

    let to_bounds _ = assert false (* TODO *)
    let to_properties _x = Value_properties.empty (* TODO *)
    let split _ = assert false (* TODO *)
  end
(*
UNUSED
type float_t = { exponent : int; mantissa: int }
let float32 = { exponent= 8; mantissa = 23 }
let float16 = { exponent= 5; mantissa = 10 }
let bfloat16 = { exponent= 8; mantissa = 7 }
let msfp11 = { exponent= 5; mantissa = 5 }
let msfp8 = { exponent= 5; mantissa = 2 }
*)              

module B16 = Make (struct let name = "b16" let max = 65504. let min = -. max end )
                 
