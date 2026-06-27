(*

TODO 29/10

   voir comment gerer le typage

   soit on enlever tous les tests d'egalite entre type et on definit une fonction equals qui trouve que tous les Fxp sont egaux entre eux (on pourrait juste imposer que le signe soit le meme, et encore ...)

   soit on trouve un moyen de recuperer le type de l'argument. Par exemple, le donner explicitement dans la conversion: fxp_conv(fxp_source, fxp_target, argument)
*)


type typing_env = (Name.t * (Location.t * Ast.base_type)) list
let empty_env = []

let get_type_sign t =
    match t with
    | Ast.FixedT fxp -> fxp.sign
    | MIntT(s, _) -> s
    | _ -> assert false

let get_type_size t =
  match t with
  | Ast.FixedT fxp -> fxp.total + (if fxp.sign then 1 else 0)
  | MIntT (_,sz) -> sz
  | _ -> assert false
      
let are_types_eq t1 t2 =
  match t1, t2 with
  | Ast.IntT, Ast.IntT
  | RealT, RealT
  | BoolT, BoolT -> true
  | (FixedT _ | MIntT _) , (FixedT _ | MIntT _) ->
    get_type_sign t1 = get_type_sign t2 &&
    get_type_size t1 = get_type_size t2 
  | _ -> false


let join_types t1 t2 =
  if are_types_eq t1 t2 then
    match t1, t2 with
    | Ast.IntT, _ -> t2
    | _, IntT -> t1
    | _ -> t1
  else
    assert false
  
let decl_var_type t env (v, l) =
  if not (List.mem_assoc v env) then
    (v, (l, t))::env
  else
    (failwith ("Variable " ^ v ^ " already declared"))
      
let get_env_type loc env n = 
  try
    List.assoc n env
  with Not_found -> 
    (Format.eprintf "Typing error. Could not find declaration for variable %s at location %a:@.@?"
       n Location.fprint loc;
     exit 1)

  
let vars_of_env env =
  List.fold_left (fun accu (v, (_,t)) -> Ast.Var.Set.add (v, t) accu) Ast.Var.Set.empty env
    
let type_error msg loc_error expected_t found_t = 
Format.eprintf " %a: Type error -- Found type %a, expecting type %a (%s)@.@?"
  Location.fprint loc_error
  Ast.pp_base_type found_t
  Ast.pp_base_type expected_t
  msg;
  exit 1

(* let type_cst l typ c = *)
(*   if not (typ = Ast.IntT) || (Z.equal (Q.den c) Z.one) then *)
(*     c *)
(*   else *)
(*     type_error l typ Ast.RealT *)

(* UNUSED 
let rec fprint_uexpr ff ue =
  match ue with
    | Ast.UCst (l, (_(*q*),s, _)) -> Format.fprintf ff "%s" s
    | UVar (l,n) -> Format.fprintf ff "%s" n
    | UBinop (l,bop, e1, e2) -> 
       Format.fprintf ff "(@[%a@ %s %a@])"
        fprint_uexpr e1
        (Ast.string_of_bop bop)
        fprint_uexpr e2
    | UUnop (l,uop, e) ->
       Format.fprintf ff "(%s@ %a)"
         (Ast.string_of_uop uop)
         fprint_uexpr e
    | URand (l,bt,(_,c1), (_,c2)) ->
       if bt = BoolT then
         Format.fprintf ff "rand_bool()" 
       else
         Format.fprintf ff "rand_%a(@[%s,@ %s@])" Ast.pp_base_type bt c1 c2 
    | UCall(l,n, el) ->
      Format.fprintf ff "@[<h>%s(%a)@]"
	n
	(Utils.fprintf_list ~sep:", " fprint_uexpr) el
    | UCond(l, e, cmp) ->
      Format.fprintf ff "@[(%a)@ %s 0@]"
	fprint_uexpr e (Ast.string_of_cmp cmp)
    | UFxpConv (_ (* l *), old_fxp, new_fxp, e) ->
      Format.fprintf ff "([conv %a -> %a]%a)"
        Fxp.pp old_fxp
        Fxp.pp new_fxp
        fprint_uexpr e 
*)

let rec get_used_type env ue =
  (* Format.eprintf "get used type %a?@." fprint_uexpr ue; *)
  
  let res = 
  match ue with
  | Ast.UCst (_, (_, _, t)) -> t
  | Ast.URand (_, t, _, _) -> Some t
  | Ast.UCall (_,f,_) -> let t, _ = List.assoc f Basic_library.functions in Some t
  | Ast.UVar (l, n) -> let _, n_t = get_env_type l env n in Some n_t
  | Ast.UBinop (l, op, e1, e2) -> (  
    match get_used_type env e1, get_used_type env e2 with
    | Some t, None 
      | None, Some t -> (
      match op with Ast.Eq | Ast.Or | Ast.And -> Some Ast.BoolT
                    | Ast.Plus| Minus | Times| Div  -> Some t
    )
    | None, None -> None
    | Some t1, Some t2 -> (
      if are_types_eq t1 t2 then
        match op with
        | Eq | Or | And -> Some BoolT
        | Plus| Minus | Times| Div  -> Some (join_types t1 t2)
      else
        type_error (Ast.string_of_bop op) l t1 t2
    )
  )
  | Ast.UUnop (_, _, e1) -> get_used_type env e1
  | Ast.UCond _ -> Some Ast.BoolT
  | Ast.UFxpConv (_, _, fxp_new, _) -> Some (FixedT (fxp_new))
  | Ast.UShiftLeft (_, e, _)
  | Ast.UShiftRight (_, e, _) -> get_used_type env e
  in
  (* Format.eprintf "get used type %a: %a@." fprint_uexpr ue (fun fmt r -> match r with None -> () | Some t -> Ast.pp_base_type fmt t) res; *)
  res

let get_cst_var_fxp_typ env ue =
  match ue with
 | Ast.UCst (_, (_, _, t)) -> (
     match t with
     | Some (FixedT fxp) -> fxp
     | _ -> assert false (* no type provided for constant or not a fxp *)

   )
 | Ast.UVar (l, n) -> (
   let (* n_loc *) _ , n_t = get_env_type l env n in
   match n_t with
   | Ast.FixedT fxp -> fxp
   | _ -> assert false
 )
 | _ -> assert false (* not a var or a constant *)
   
let rec type_expr ?(no_rec=false) (env:typing_env) typ (ue: Ast.uexpr) =
  let te = type_expr ~no_rec env typ in
  let typ_unop l op e1 = 
    Ast.mk_expr l typ (Ast.Unop (op, te e1))
  in
  let typ_binop l op e1 e2  =
    (* Most basic operations are homogenous in type. But == (Eq) *)
    match op with
    | Ast.Eq -> (
        try
          let desome x = match x with Some x -> x | None -> raise Not_found in
          let typ1 = desome (get_used_type env e1) in
          let typ2 = desome (get_used_type env e2) in
          if are_types_eq typ1 typ2 && typ = Ast.BoolT then
            Ast.mk_expr l typ
              (Ast.Binop (op,
                          type_expr env typ1 e1,
                          type_expr env typ1 e2))
          else
          if typ = Ast.BoolT then type_error (Ast.string_of_bop op) l typ1 typ2 else
            type_error (Ast.string_of_bop op) l typ Ast.BoolT
        with Not_found -> type_error "Unable to type expression. Cannot compute type of subexpressions" l typ typ
      )
    | And | Or ->
      if typ = Ast.BoolT then
        Ast.mk_expr l typ (Ast.Binop (op, te e1, te e2))
      else
        type_error (Ast.string_of_bop op) l typ Ast.BoolT
    | Plus | Minus | Times | Div -> (
        match typ with
          Ast.FixedT fxp -> (
            (*************************************************)
            (* Special treatment for fixed point arithmetics *)
            (*   the shifts are introduced as well as cast   *)
            (*   into greater formats to avoid overflows     *)
            (*************************************************)
            let fxp1 = get_cst_var_fxp_typ env e1 in
            let fxp2 = get_cst_var_fxp_typ env e2 in (
              match op, fxp, fxp1, fxp2 with
                (Plus|Minus), _, _, _ ->
                if Fxp.(fxp.frac = fxp1.frac && fxp.frac = fxp2.frac) then
                  Ast.mk_expr l typ (Ast.Binop (op, te e1, te e2))
                else (
                  Format.eprintf " %a: Type error -- Fractional part of operands and result should be aligned.@.@?"
                    Location.fprint l;
                  exit 1
                )
              | Times, _, _, _ ->
                let dec_right =  Fxp.(fxp1.frac + fxp2.frac - fxp.frac) in
                (* computation shall be done in a greater datatype, like the size of e1+e2.
                   then, we shift as requested
                   then, we project back to the target format
                *)
                let te1 = te e1 in
                let te2 = te e2 in
                let mul_exp = Ast.mk_expr l typ (Ast.Binop (op, te1, te2)) in
                if dec_right = 0 then
                  mul_exp
                else
                if dec_right > 0 (* we shift right *) then
                  let dec_right = Ast.mk_expr
                      l
                      Ast.IntT
                      (Ast.Cst (Q.of_int dec_right, string_of_int dec_right))
                  in
                  Ast.mk_expr l typ (Ast.ShiftRight (mul_exp, dec_right))
                else  (* we shift left by -dec *)
                  let dec_left = Ast.mk_expr
                      l
                      Ast.IntT
                      (Ast.Cst (Q.of_int (-dec_right), string_of_int (-dec_right)))
                  in
                  Ast.mk_expr l typ (Ast.ShiftLeft (mul_exp, dec_left))
              | Div, _, _, _ -> 
                let dec_left =  Fxp.(fxp.frac - fxp1.frac + fxp2.frac) in
                if dec_left < 0 then (
                  Format.eprintf " %a: Type error -- Not enough Fractional part in result of division: update the result fractional part by at least %i.@.@?"
                    Location.fprint l
                    (-dec_left);
                  exit 1
                )
                else
                  (* Computing
                     (int32_t)(((int64_t)(x1) << dec + ((((int32_t)x1)<0? -|x2|  : |x2|)/2) ) / x2)
                     where int64_t of x1 is used to compute product with increase precision before projecting to int32_t eventually
                     and where (int32_t)x1 seems to be used to obtain the sign of x1 is x1 is uint32_t

                  *)
                  let dec_left = Ast.mk_expr
                      l
                      Ast.IntT
                      (Ast.Cst (Q.of_int dec_left, string_of_int dec_left))
                  in         
                  let e1' = te e1 in
                  let e2' = te e2 in
                  let e1_shifted = (* il manque le cast *)
                    Ast.mk_expr l typ (Ast.ShiftLeft (e1', dec_left)) 
                  in (* on oublie la partie avec |x2| oiour l'instant  
                        let abs_e2_div_2 =

                        Ast.mk_expr l typ (Ast.ShiftLeft (e1', dec_left)) 
                        in *)
                  Ast.mk_expr l typ (Ast.Binop (op, e1_shifted, e2'))
            
            | _ -> assert false (* why would you compute other functions on fixed point numbers ? *)
          ))
        | _ -> 
          Ast.mk_expr l typ (Ast.Binop (op, te e1, te e2))
      )
  in
  match ue with
  | Ast.UCst (l, (c, cs, t)) -> (
      match t with
        None -> (
          match typ with
          | Ast.IntT | Ast.FixedT _ | Ast.MIntT _ | Ast.BoolT ->
            if (Z.equal (Q.den c) Z.one) then
	      Ast.mk_expr l typ (Ast.Cst (c, cs))
            else 
              failwith "Unable to compute type"
          | Ast.RealT -> Ast.mk_expr l Ast.RealT (Ast.Cst (c, cs))
        )
      | Some t -> 
        if are_types_eq typ t then
	  Ast.mk_expr l typ (Ast.Cst (c, cs))
        else 
	  type_error ("cst("^cs^")") l typ t
    )
  | Ast.UVar (l, n) -> 
    let (* n_loc *) _ , n_t = get_env_type l env n in
    if are_types_eq n_t typ then
      Ast.mk_expr l typ (Ast.Var n)
    else
      type_error "var"  l typ n_t	
  | Ast.UUnop (l, op, ((Ast.UCst _ | Ast.UVar _) as e1)) when no_rec ->
    typ_unop l op e1
  | Ast.UUnop (l, op, e1) when not no_rec -> 
    typ_unop l op e1
  | Ast.UBinop (l, op, ((Ast.UCst _ | Ast.UVar _) as e1),
                       ((Ast.UCst _ | Ast.UVar _) as e2)) when no_rec ->
    typ_binop l op e1 e2 
  | Ast.UBinop (l, op, e1, e2) when not no_rec ->
    typ_binop l op e1 e2
  | Ast.UUnop (l, _, _) | Ast.UBinop (l, _, _, _) -> (
      Format.eprintf " %a: Type error -- Expecting only 3 address code for fixed point datatypes.@.@?"
        Location.fprint l;
      exit 1
    )
  | Ast.URand (l, t, c1, c2) -> 
    if are_types_eq typ t then
      Ast.mk_expr l typ (Ast.Rand (c1, c2))
    else
      type_error "rand" l typ t	
  | Ast.UCall (l, f, args) -> ((* we check the type in the library *)
      if List.mem_assoc f Basic_library.functions then
        let t, args_t = List.assoc f Basic_library.functions in
        if are_types_eq typ t then (
	  let args' = (
            try
              List.map2 (type_expr env) args_t args
            with Invalid_argument _ -> failwith ("Mistyped function call " ^ f)
          )
          in
          Ast.mk_expr l t (Ast.Call(f, args'))
        )
        else 
	  type_error ("call("^ f ^")") l typ t	
      else (
        Format.eprintf "Unable to find function %s in defined fcns@." f;
        assert false
      )
    )
  | Ast.UCond (l, e, sl) ->
    if typ = Ast.BoolT then
      let typ_e = 
	match get_used_type env e with 
        | Some t -> t 
        | None -> Format.eprintf "Unable to type expression@.@?"; assert false

      in
      Ast.mk_cond l (type_expr env typ_e e) sl
    else 
      type_error "cond" l typ Ast.BoolT
  | Ast.UFxpConv(l, fxp_old, fxp_new, e) ->
    let new_typ = Ast.FixedT fxp_new in
    let old_typ = Ast.FixedT fxp_old in
    if are_types_eq typ new_typ && are_types_eq typ old_typ then
      Ast.mk_expr l new_typ (FxpConv (fxp_old, fxp_new, te e))
    else
      type_error ("fxp_conv") l typ new_typ
  | Ast.UShiftLeft (l, e, dec) -> (
      let e_typ = get_used_type env e in
      let dec_typ = get_used_type env dec in
      match e_typ, dec_typ with
      | Some e_typ, _ when not (Ast.is_int_type e_typ) -> 
        type_error "shift left" l e_typ Ast.IntT
      | None, Some dec_typ when not (Ast.is_int_type dec_typ) -> 
        type_error "shift left" l dec_typ Ast.IntT
      | _ -> 
        let e' = te e in
        let dec' = te dec in
        Ast.mk_expr l typ (ShiftLeft (e', dec'))
    )
  | Ast.UShiftRight (l, e, dec) -> (
      let e_typ = get_used_type env e in
      let dec_typ = get_used_type env dec in
      match e_typ, dec_typ with
      | Some e_typ, _ when not (Ast.is_int_type e_typ) -> 
        type_error "shift left" l e_typ Ast.IntT
      | None, Some dec_typ when not (Ast.is_int_type dec_typ) -> 
        type_error "shift left" l dec_typ Ast.IntT
      | _ -> 
        let e' = te e in
        let dec' = te dec in
        Ast.mk_expr l typ (ShiftRight (e', dec'))
    )
       
    
let type_guard env e = 
  type_expr env Ast.BoolT e

let rec type_stm env s = 

  let te no_rec loc n = type_expr ~no_rec:no_rec env (snd (get_env_type loc env n)) in
  let tg = type_guard env in
  let ts = type_stm env in

  match s with
  | Ast.UAsn (loc, n, e) -> (
      let typ_n = get_env_type loc env n in
      let te = 
        match typ_n with
        | _, (Ast.FixedT _) -> te true
        | _ -> te false
      in
      Ast.Asn (loc, n, te loc n e)
  )
  | Ast.UAsrt (l, g) -> Ast.Asrt (l, tg g)
  | Ast.USeq (l, s1, s2) -> Ast.Seq (l, ts s1, ts s2) 
  | Ast.UIte (l, g, s1, s2) -> Ast.Ite (l, tg g, ts s1, ts s2) 
  | Ast.UWhile (l, g, s) -> Ast.While (l, tg g, ts s)
  | Ast.UReadInput (l, lv) ->
      let _ = List.iter (fun var_name ->
        ignore (get_env_type l env var_name)
      ) lv in
      Ast.ReadInput (l, lv)
  | Ast.UReadState (l, lv) ->
      let _ = List.iter (fun var_name ->
        ignore (get_env_type l env var_name)
      ) lv in
      Ast.ReadState (l, lv)

  | Ast.UNop l -> Ast.Nop l
  | Ast.UNN (l,ins,layers,outs) ->
     let layers =
       List.map (fun (file,act) -> file, Nn.type_act env Ast.RealT act) layers 
     in 
     Ast.NN (l,ins,layers,outs)
       
let get_vars (stm: Ast.stm) =
  let vars = Ast.get_assigned_vars stm in
  let env = Ast.Var.Set.fold (
    fun (v, t) env -> decl_var_type t env (v, Location.dummy ())
  ) vars empty_env in
  vars_of_env env