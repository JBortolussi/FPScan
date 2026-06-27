(* 
We record 
- for each bool variable the associated pair (env_when_true, env_when_false)

All assigns are enforced at each element


 *)

module BoolPair (D: Relational.Domain) =
  struct
    let name = "BoolParitioned " ^ D.name
    let nonrel_base = D.nonrel_base
    let parse_param s = D.parse_param s
    let fprint_help _ (*ff*) = ()             
    type t = D.t * D.t
    let fprint ff (whentrue, whenfalse) = Format.fprintf ff "@[<v 1>{true:  %a;@ false: %a}@]@ " D.fprint whentrue D.fprint whenfalse
    let json _ = assert false (* TODO *)
    let order (t1,f1) (t2, f2) = D.order t1 t2 && D.order f1 f2
    let top varset = D.top varset , D.top varset
    let bottom varset = D.bottom varset, D.bottom varset
    let is_bottom (whentrue, whenfalse) = D.is_bottom whentrue && D.is_bottom whenfalse (* since the pair acts as a union *) 
    let get_vars (whentrue, whenfalse) = Ast.Var.Set.union (D.get_vars whentrue)  (D.get_vars whenfalse)
    let join (t1,f1) (t2, f2) = D.join t1 t2, D.join f1 f2
    let meet (t1,f1) (t2, f2) = D.meet t1 t2, D.meet f1 f2

    (* inter_D d (whentrue, whenfalse): constrain both whentrue and whenfalse elements to be in d (using meet) *)
    let inter_D d (t,f) = D.meet d t, D.meet d f
                        
    let widening (t1,f1) (t2, f2) = D.widening t1 t2, D.widening f1 f2
    let assignment x expr (t,f) = D.assignment x expr t, D.assignment x expr f
    let backward_assignment b s x expr (t,f) = D.backward_assignment b s x expr t, D.backward_assignment b s x expr f
    let read_input (t,f) vars = D.read_input t vars, D.read_input f vars
    let read_state (t,f) vars = D.read_state t vars, D.read_state f vars
    let guard e (t,f) = D.guard e t, D.guard e f
    let project_values (t1, f1) (t2, f2) l_relation =
      D.project_values t1 t2 l_relation, D.project_values f1 f2 l_relation
  end
  
module MakeR (D: Relational.Domain) : Relational.Domain =
  struct
    module P = BoolPair (D)
    let nlogf n = 
        Report.nlogf n
                
    let name = D.name
    let nonrel_base = D.nonrel_base
    let is_partitioned () = true
    let parse_param _ = ()
    let fprint_help _ (*fmt*) = ()

    (* An environement is partitioned by boolean variables: each bvar
       is associed to an abstract environment of type P.t. If no
       boolean variable is present, back to the default D domain.

      We use a map but assume (and guarantee by construction) that all
       lements share the same list of boolean variables *) 
    type t = Env of ((P.t) Ast.Var.Map.t) | NoBool of D.t

    let fprint ff t = 
      match t with
      | NoBool env -> D.fprint ff env
      | Env bool_m ->
         if Ast.Var.Map.is_empty bool_m then (
           Format.fprintf ff "@[<0>@ ";
           Format.fprintf ff "⊤@]@ "
         )
         else
           let first = ref true in
           Ast.Var.Map.iter
             (fun n v ->
               if !first then begin
                   Format.fprintf ff "@[<0>{%s@ %a : %a" name Ast.Var.pp_name n P.fprint v;
                   first := false
                 end else
                 Format.fprintf ff ",@ %a : %a" Ast.Var.pp_name n P.fprint v)
             bool_m;
           Format.fprintf ff " }@]@ "
 
      
    let json _ = assert false

(*    let find_or_top (n:Name.t) (m:P.t Name.Map.t) vars : P.t =
      try
        Name.Map.find n m
      with Not_found -> (P.top vars)
 *)                    
    (* Let t(bvar) be the abstract env D.t associated to boolean variable bvar in t
       t1 <= t2 iff forall bvar, t1(bvar) leq_D t2(bvar) 
     *)
    let order t1 t2 = match t1, t2 with
    | NoBool e1, NoBool e2 -> D.order e1 e2
    | NoBool _, Env _ | Env _, NoBool _ -> assert false (* Shall not happen *)
    | Env m_bool1, Env m_bool2 ->
         Ast.Var.Map.fold
           (fun n v2 b -> b &&
                            P.order (Ast.Var.Map.find n m_bool1) v2)
           m_bool2
      true

    let mk_bot_top _ (* UNUSED: name *) vars default_val_d default_val_p =
      let bool_vars, non_bool_vars = Ast.Var.Set.partition_by_type Ast.BoolT vars in
      (* Special case: if no boolean variable used, introduce a dummy one *)
      if Ast.Var.Set.is_empty bool_vars then (
        NoBool (default_val_d vars) 
      )
      else
        let list =
          Ast.Var.Set.fold (
              fun bv accu -> Ast.Var.Map.add bv (default_val_p non_bool_vars) accu
            ) bool_vars Ast.Var.Map.empty 
        in
        Env list
      
    let top vars = mk_bot_top "top" vars D.top P.top
    let bottom vars = mk_bot_top "bot" vars D.bottom P.bottom
                    
    (* Produce the environement D.t that gathers the elements *)
    let gamma env : D.t =
      let rec aux list =
        match list with
        | [] -> assert false
        | [_, (when_true, when_false)] -> D.join when_true when_false
        | (_, (wt,wf))::tl -> D.meet (D.join wt wf) (aux tl)
      in
      match env with
      | NoBool env -> env
      | Env env -> aux (Ast.Var.Map.bindings env)

    let reduce env =
      match env with
      | NoBool _ -> env
      | Env m_bool ->
         let glob = gamma env in
         let m_bool' = Ast.Var.Map.map (P.inter_D glob) m_bool in
         Env m_bool'
         
    let is_bottom env =
      let env = reduce env in
      match env with
      | NoBool env -> D.is_bottom env
      | Env m_bool -> Ast.Var.Map.exists (fun _ v -> P.is_bottom v) m_bool

    let get_vars env = match env with
      | NoBool env -> D.get_vars env
      | Env env -> Ast.Var.Map.fold
                     (fun id pair set -> Ast.Var.Set.add id (Ast.Var.Set.union set (P.get_vars pair)))
                     env
                     Ast.Var.Set.empty

    let lattice_binop fun_name d_fun p_fun env1 env2 =
      match env1, env2 with
      | NoBool env1, NoBool env2 -> NoBool (d_fun env1 env2)
      | NoBool _, Env _ | Env _, NoBool _ ->
         Format.eprintf "%a %s %a failed" fprint env1 fun_name fprint env2; 
         assert false
      | Env env1, Env env2 ->
         let env' = Ast.Var.Map.fold (fun b1 v1 res2 ->
                        let b1_in_res2 = Ast.Var.Map.find b1 res2 in
                        let new_v = p_fun v1 b1_in_res2 in
                        Ast.Var.Map.add b1 new_v res2 
                      ) env1 env2 in
         Env env'
         
    let join = lattice_binop "join" D.join P.join
    let meet = lattice_binop "meet" D.meet P.meet
    let widening = lattice_binop "widen" D.widening P.widening

    let rec bool_eval expr env =
      match expr.Ast.expr_desc with
      | Cst (_, bs) ->
         let vars = get_vars env in
         let env = gamma env in (
             match bs with
             | "true" -> env , D.bottom vars
             | "false" -> D.bottom vars, env
             | _ -> assert false
           )
      | Rand _ -> let env = gamma env in env, env
      | Var n -> (
        match env with
        | NoBool _ -> assert false (* should only be used when no
                                        boolean variable exist. How
                                        can you then have expressions
                                        with boolean variables?  *)
        | Env env -> Ast.Var.Map.find (n, Ast.BoolT) env
      )
      | Binop(bop, e1, e2) -> (
        match bop with
        | And|Or ->
           let t1,f1 = bool_eval e1 env in
           let t2,f2 = bool_eval e2 env in
           let res = (
               match bop with
               | Ast.And -> D.meet t1 t2, D.join f1 f2
               | Ast.Or -> D.join t1 t2, D.meet f1 f2
               | _ -> assert false
             )
           in
           nlogf 5 "bool eval %s@ %a@ %a@ = %a." (Ast.string_of_bop bop) P.fprint (t1,f1) P.fprint (t2,f2) P.fprint res;
           res
        | Eq when e1.expr_type = BoolT ->
           let t1, f1 = bool_eval e1 env in
           let t2, f2 = bool_eval e2 env in
           let res = D.join (D.meet t1 t2) (D.meet f1 f2), D.meet (D.join f1 f2) (D.join t1 t2) in
           nlogf 5 "bool eval %s@ %a@ %a@ = %a." (Ast.string_of_bop bop) P.fprint (t1,f1) P.fprint (t2,f2) P.fprint res;
           res
        | Eq (* when e1.expr_type <> BoolT *) ->
           let diff = Ast.mk_expr expr.expr_loc e1.expr_type (Binop(Ast.Minus, e1, e2)) in
           let cmp = Ast.mk_cond expr.expr_loc diff Ast.Zero in
           bool_eval cmp env
        | Plus|Minus|Times|Div -> assert false (* not a boolean expression *) 
           
      )
      | Unop (Not, e) -> let t,f = bool_eval e env in f,t
      | Cond (g, cmp) ->
         let gammaenv = gamma env in
         let when_true = D.guard expr gammaenv in
         let when_false = D.guard (Ast.neg_guard expr) gammaenv in
         nlogf 5 "bool eval cmp %a %s 0 in %a (proj from %a)@ = %a." Ast.fprint_expr g (Ast.string_of_cmp cmp) D.fprint gammaenv fprint env P.fprint (when_true, when_false);
         when_true, when_false
      | Call _ when expr.expr_type = BoolT -> Format.eprintf "Unable to evaluate boolean call %a" Ast.fprint_expr expr; assert false
      | Call _ -> assert false (* Not a boolean expression *)
      | FxpConv _ -> assert false  (* Not a boolean expression *)
      | ShiftLeft _ | ShiftRight _ -> assert false  (* Not a boolean expression *)
   
                           
    let assignment v expr env =
      match env with
      | NoBool env -> NoBool (D.assignment v expr env)
      | Env env ->
         let env' = Ast.Var.Map.map (fun p_env -> P.assignment v expr p_env) env in
         let res = (* if bool assign then bool_eval and update or add the new cell *)
           if expr.expr_type = BoolT then
             let t,f = bool_eval expr (Env env') in
             Ast.Var.Map.add (v, expr.expr_type) (t,f) env'
           else
             env'
         in
         Env res

    (* TO DO *)
    let backward_assignment b s v expr env =
      match env with
      | NoBool d ->
          NoBool (D.backward_assignment b s v expr d)

      | Env m ->
          if expr.expr_type = BoolT then
            env
          else
            let m' =
              Ast.Var.Map.map (fun pe -> P.backward_assignment b s v expr pe) m
            in
            Env m'

    (* We ensure that expr is true within env *)
    let rec back_eval ?(neg=false) expr env =
      match expr.Ast.expr_desc with
      | Cst _ | Rand _ -> env
      | Call _ -> (* TODO *) env 
      | Var n -> (* Since var n is supposed to be true (within env)
                    then substitute n:(whentrue, bottom) *)
         let env = match env with NoBool _ -> assert false | Env env -> env in
         let t,f = Ast.Var.Map.find (n, Ast.BoolT) env in
         let bot = D.bottom (P.get_vars (t,f)) in
         let env' =
           if neg then
             Ast.Var.Map.add (n,Ast.BoolT) (bot,f) env
           else
             Ast.Var.Map.add (n,Ast.BoolT) (t,bot) env
         in
         Env env'
      | Unop (Not, e) -> back_eval ~neg:(not neg) e env
      | Cond (_ (* UNUSED: g *), _ (* UNUSED: cmp *)) ->
         (* enforce the condition to each partition *)
         let env = match env with NoBool _ -> assert false | Env env -> env in
         let env' =
           if neg then
             Ast.Var.Map.map (P.guard (Ast.neg_guard expr)) env
           else
             Ast.Var.Map.map (P.guard expr) env
         in
         Env env'
      (* OLD TODO Any numerical condition should have been
                         addressed before in the P.guard call *)
      | Binop (And, e1, e2) ->
         if neg then (* This is indeed an Or. Hard to impose anything. Leave the env as is *)
           meet env (join (back_eval ~neg e1 env) (back_eval ~neg e2 env))
         else
           meet (back_eval e1 env) (back_eval e2 env)
      | Binop (Or, e1, e2) ->
         if neg then (* This is indeed a And *)
           meet (back_eval ~neg e1 env) (back_eval ~neg e2 env)
         else
           meet env (join (back_eval e1 env) (back_eval e2 env) )
      | Binop _ 
      | FxpConv _
      | ShiftRight _
      | ShiftLeft _
        -> env (* hard to say anything *) 
                             
    let guard expr env =
      (* Enforcing the guard in each cell. We also "decompose" the
         expression and constraint the result to satisfy the when_true
         value *)
      match env with
      | NoBool env -> NoBool (D.guard expr env)
      | Env env ->
         let env' = Ast.Var.Map.map (fun p_env -> P.guard expr p_env) env in
         let when_true, _ (* UNUSED: wf *) = bool_eval expr (Env env') in
         (* Format.eprintf "Guard de %a: on obtient la paire %a@." Ast.fprint_expr expr P.fprint (when_true, wf);  *)
         let env'' = Ast.Var.Map.map (P.inter_D when_true) env' in
         back_eval expr (Env env'')
         
    (* We could just join the two sets, but if (x,Some(b,true)) =
       (x,Some(b,false)) we can remove the information *)
    let merge_bounds l1 l2 =
      List.fold_left (fun accu ((v,ctx),bound2) ->
        (* Search if v, not ctx is in l1 *)
          match ctx with
            None -> assert false (* should be called on when_true, when_false values *)
          | Some (bv,b) ->
             let neg_id = (v, Some (bv, not b)) in 
             if List.mem_assoc neg_id accu then
               let bound1 = List.assoc neg_id accu in
               if bound1 = bound2 then (* We can compare pairs of Scalar with regular equality *)
                 List.remove_assoc neg_id accu
               else
                 ((v,ctx), bound2)::accu
             else
               ((v,ctx), bound2)::accu
        ) l1 l2

  let read_input (t : t) (vars : Ast.Var.t list) : t =
    match t with
     | NoBool d -> NoBool (D.read_input d vars)
     | Env m -> let m' = Ast.Var.Map.map (fun p -> P.read_input p vars) m in Env m'

  let read_state (t : t) (vars : Ast.Var.t list) : t =
    match t with
     | NoBool d -> NoBool (D.read_state d vars)
     | Env m -> let m' = Ast.Var.Map.map (fun p -> P.read_state p vars) m in Env m'

  let to_properties env =
    let num_env = gamma env in
    D.to_properties num_env 

  
    let to_bounds env =
      let vars = get_vars env in
      let bool_vars, _ (* UNUSED: num_vars *) = Ast.Var.Set.partition_by_type Ast.BoolT vars in
      let is_true_or_false v env =
        match env with
        | NoBool _ -> assert false
        | Env env ->
           let (when_true, when_false) = Ast.Var.Map.find v env in
           let true_bot = D.is_bottom when_true in
           let false_bot = D.is_bottom when_false in
           let bounds_bool = (not true_bot), (not false_bot) in
           bounds_bool
      in
      
      let num_env = gamma env in
      let num_bounds = D.to_bounds num_env in
      num_bounds @ (
        List.fold_left (fun accu ((n,t) as v) ->
            let (when_true, when_false) =
              Ast.Var.Map.find v (match env with Env env -> env | _ -> assert false)
            in
            let select_bounds b =
              let bounds = D.to_bounds (if b then when_true else when_false) in
              List.map
                (fun ((v,ctx), bounds_n) ->
                  let ctx' = 
                    match ctx with
                    | None -> Some ((n, t), b)
                    | Some _ -> assert false (* cannot partition over partitioned values *)
                  in
                  (v,ctx'), bounds_n)
                bounds
            in
            let t,f = is_true_or_false v env in
            let b_res, nbounds = 
              match t,f with
              | true, true ->
                 let bbounds = Bounds.mk (Scalar.of_bool false) (Scalar.of_bool true) in
                 let tbounds = merge_bounds (select_bounds true) (select_bounds false) in
                 bbounds, tbounds
              | true, false ->
                 let x= Scalar.of_bool true in
                 let bbounds = Bounds.mk x x in
                 (*let tbounds = select_bounds true in*)
                 bbounds, []
              | false, true ->
                    let x= Scalar.of_bool false in
                 let bbounds = Bounds.mk x x in
                 (*let tbounds = select_bounds false in*)
                 bbounds, []
              | false, false -> assert false
            in
            ((v, None), b_res)::nbounds@accu
          ) [] (Ast.Var.Set.elements bool_vars))

  let project_values t1 t2 l_relation =
    let open Ast.Var.Map in

    (* Some functions *)
    let proj_D t f = D.project_values t f l_relation in
    let proj_P t f = P.project_values t f l_relation in
    let proj_on_t_f t m = map (fun (t', f') -> proj_P (t,t) (t',f')) m in
    (* let proj_t_f_on t m = map (fun (t',f') -> proj_P (t',f') (t,t)) m in *)
    (* Create a new map containing the value of every bool to add *)
    (* The key is the variable in t2 and the value is the value of the expression in t1 *)
    let bool_to_add = List.fold_left (fun m (e, (n,typ)) -> 
      if typ = Ast.BoolT then 
        let v = bool_eval e t1 in
        add (n, typ) v m
      else
        m
      ) empty l_relation in

    match t1, t2 with
    | NoBool t1, NoBool t2 -> NoBool (proj_D t1 t2)
    | NoBool t1, Env m2 -> Env (proj_on_t_f t1 m2) (* Project t1 to each t and f of m2 *)
    | Env _, NoBool t2 ->
      (* if is_empty bool_to_add then *)
        NoBool (proj_D (gamma t1) t2)
      (* else (* This case should never happend because if t2 is NoBool, we should not add a bool *)
        (* Project each t and f of m1 to t2 *)
        Env (proj_t_f_on t2 bool_to_add) *)
    | Env _, Env m2 -> let gam1 = gamma t1 in
      (* if is_empty bool_to_add then (* This case is handle by the else case *)
        Env (proj_on_t_f gam1 m2)
      else *)
        let m2' = 
          fold (fun key (t2,f2) m -> 
            match find_opt key bool_to_add with
            (* The value of the boolean key isn't to update *)
            (* Just project gamma t1 to t and f to stay correct on non boolean variable values *)
            | None -> add key (proj_P (gam1,gam1) (t2,f2)) m

            (* The value of the boolean key has to be updated *)
            | Some (t1,f1) -> (
              (* If t2 was bottom, reset it to top *)
              let t2 = if D.is_bottom t2 then D.top (D.get_vars t2) else t2 in
              (* If f2 was bottom, reset it to top *)
              let f2 = if D.is_bottom f2 then D.top (D.get_vars f2) else f2 in
              add key (proj_P (t1,f1) (t2,f2)) m
            ))
          m2
          empty in
        Env (m2')
      
  end


   
