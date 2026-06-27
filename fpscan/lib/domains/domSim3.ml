(* "Domain" for set-based simulation with injection of scenarios (intervals of values and constraints depending on k (iterator)) for input variables
The widening is not a real widening, its role is to pass to the next iteration. 
Compared to DomSim, for DomSim3 the variables in the environement represent the transitions between iterations*)

module DomSim3 (DomRel : Relational.Domain) (S : Scenario.Scenario_sig) : Relational.Domain = struct

  let name = "simulation"

  let nonrel_base = None
  let is_partitioned () = false

  let parse_param _ = ()

  let fprint_help fmt = Format.fprintf fmt "Scenario-based simulation domain.\n"

  module IntMap = Map.Make(Int) 
  module StrMap = Map.Make(String)

  type t = {
    states : DomRel.t IntMap.t;
    iter : int;
    varIter : int StrMap.t; (* varIter allows to keep track of the last iteration for each variables, it is used to rename correctly the variables in the expressions given to assignment and guard *)
    prevExprs : Ast.expr StrMap.t; (* prevExprs allows to have acces to the values taken by the input variables at the previous iteration, it is used to compute the difference between the current value and the previous one*)
  }

  let fprint fmt {states; iter; varIter=_; _} =
    Format.fprintf fmt "Simulation domain states by iteration:@\n";
    Format.fprintf fmt "  Iteration #%d:@ %a@\n" iter DomRel.fprint (IntMap.find iter states)

  let json {states; iter; varIter=_; _} = DomRel.json (IntMap.find iter states)

  (* Function used to initialize the map varIter, with the all the variables of the program as keys, associated to the value 0 *)
  let initialize_varIter s = 
    let add_key_var name acc =
        (* We need to exctract the prefix before '_' to only keep the variable name for the key *)
        let prefix =
            match String.index_opt name '_' with
                | Some i -> String.sub name 0 i
                | None -> name
        in
        (* Make sure that the key is unique *)
        if StrMap.mem prefix acc then acc
        else StrMap.add prefix 0 acc
    in
    Ast.Var.Set.fold (fun (name, _) acc -> add_key_var name acc) s StrMap.empty

  (* function to create Ast.expr with location dummy *)
  let create_expr_dummy expr ty = 
    { Ast.expr_desc = expr;
        Ast.expr_loc = Location.dummy ();
        Ast.expr_type = ty }

  (* Default Ast.expr to initialize prev_var *)
  let default_expr = create_expr_dummy (Ast.Cst (Q.of_float (0.), string_of_float (0.))) RealT

  let top s =
  (* varIter is initialized : the keys are all the variables without the iteration numbers (x, y, k etc.) *)
  (* prevExprs is initialized : the keys are all the input variables (without the iteration numbers) *)
  { states = IntMap.singleton 0 (DomRel.top s); iter = 0; varIter = initialize_varIter s; prevExprs = Scenario.NameSet.fold (fun var_name acc_map -> StrMap.add var_name default_expr acc_map) S.input_vars StrMap.empty}

  let bottom s =
  (* varIter is initialized : the keys are all the variables without the iteration number (x, y, k etc.) *)
  (* prevExprs is initialized : the keys are all the input variables (without the iteration numbers) *)
  {states = IntMap.singleton 0 (DomRel.bottom s); iter = 0; varIter = initialize_varIter s; prevExprs = Scenario.NameSet.fold (fun var_name acc_map -> StrMap.add var_name default_expr acc_map) S.input_vars StrMap.empty}

  let is_bottom {states; iter; varIter=_; _} = DomRel.is_bottom (IntMap.find iter states)

  let get_vars {states; iter; varIter=_; _} = DomRel.get_vars (IntMap.find iter states)

  let order _ _ = false (* False order function : always false as fixed point will never be reached *)
  
  let join t1 t2 =
    (*The join sould be used only on abstract states of the same iteration*)
    if t1.iter <> t2.iter then
      failwith (Format.sprintf "DomSim.join: mismatched iterations (%d vs %d)" t1.iter t2.iter)
    else
      let last_s1 = IntMap.find t1.iter t1.states in
      let last_s2 = IntMap.find t2.iter t2.states in
      let joined_s1s2 = DomRel.join last_s1 last_s2 in
      { states = IntMap.add t1.iter joined_s1s2 t1.states; iter = t1.iter; varIter = t1.varIter; prevExprs = t1.prevExprs }
  
  (* Intersection should not be used*)
  let meet _ _ = assert false

  (* Gives the name for the transition variable coresponding to the variable and the current iteration *)
  let get_new_name v_name iter =
    if iter = 0 then v_name ^ "_" ^ string_of_int iter
    else v_name ^ "_" ^ string_of_int (iter-1) ^ "_" ^ string_of_int iter

  (* Function to renamed correctly (with the correct iteration), the variables in an expression.
  To do so, we use the map varIter, who indicated the last iteration for each variable *)
  let rename_vars_in_expr t expr =
    let open Ast in
    let rec aux expr =
        match expr.expr_desc with
        | Var v_name ->
            let new_var_name =
              if v_name = "k" then get_new_name v_name t.iter
            else let iter = StrMap.find v_name t.varIter in get_new_name v_name iter
            in
            { expr with expr_desc = Var new_var_name }
        | Cst _ -> expr
        | Binop (op, e1, e2) ->
            let new_e1 = aux e1 in
            let new_e2 = aux e2 in
            { expr with expr_desc = Binop (op, new_e1, new_e2) }
        | Unop (op, e) ->
            let new_e = aux e in
            { expr with expr_desc = Unop (op, new_e) }
        | Rand (e1, e2) ->
            { expr with expr_desc = Rand (e1, e2) }
        | Call (name, es) ->
            let new_es = List.map aux es in
            { expr with expr_desc = Call (name, new_es) }
        | Cond (e, cmp) ->
            let new_e = aux e in
            { expr with expr_desc = Cond (new_e, cmp) }
        | FxpConv (fxp1, fxp2, e) ->
            let new_e = aux e in
            { expr with expr_desc = FxpConv (fxp1, fxp2, new_e) }
        | ShiftLeft (e1, e2) ->
            let new_e1 = aux e1 in
            let new_e2 = aux e2 in
            { expr with expr_desc = ShiftLeft (new_e1, new_e2) }
        | ShiftRight (e1, e2) ->
            let new_e1 = aux e1 in
            let new_e2 = aux e2 in
            { expr with expr_desc = ShiftRight (new_e1, new_e2) }
    in
    aux expr

  let rename_var v_name i =
      get_new_name v_name i
      |> fun new_name ->
        { Ast.expr_desc = Ast.Var new_name;
          Ast.expr_loc = Location.dummy ();
          Ast.expr_type = RealT }

  (* Function to create the sum of the var until the current iteration : k_0_1 + k_1_2 + … + k_{iter-1,iter} *)
  let sum_var_until var iter =
    let rec aux i acc =
      if i = 0 then acc
      else
        let var_i = rename_var var i in
        let new_acc =
          match acc with
          | None -> Some var_i
          | Some acc_expr -> Some (create_expr_dummy (Ast.Binop(Ast.Plus, acc_expr, var_i)) RealT)
      in
      aux (i-1) new_acc
    in
    match aux iter None with
    | Some e -> e
    | None -> rename_var var 0

  let print_prevExprs prevExprs =
    StrMap.iter (fun var expr ->
      let s = Format.asprintf "%a" Ast.fprint_expr expr in
      Format.printf "prevExprs[%s] = %s@." var s
    ) prevExprs

  (* Function to transform a constraint on input vars *)
  let transform_expr iter prevExprs expr =
    let rec aux e =
      match e.Ast.expr_desc with

      | Ast.Var v_name ->
          if v_name = "k" then
            sum_var_until "k" iter
          else if StrMap.mem v_name prevExprs then
            let new_var = rename_var v_name iter in
            let prev_expr = StrMap.find v_name prevExprs in
            { e with expr_desc = Ast.Binop (Ast.Plus, new_var, aux prev_expr) }
          else
            (* Variable has already been renamed *)
            e

      | Ast.Cst _ -> e
      | Ast.Binop (op, e1, e2) ->
          { e with expr_desc = Ast.Binop (op, aux e1, aux e2) }
      | Ast.Unop (op, e1) ->
          { e with expr_desc = Ast.Unop (op, aux e1) }

      | Ast.Rand _ -> e 
      
      | Ast.Call (n, args) ->
          { e with expr_desc = Ast.Call (n, List.map aux args) }
      | Ast.Cond (c, cmp) -> { e with expr_desc = Ast.Cond (aux c, cmp) }
      | Ast.FxpConv (f1, f2, e1) -> { e with expr_desc = Ast.FxpConv (f1, f2, aux e1) }
      | Ast.ShiftLeft (e1, e2) -> { e with expr_desc = Ast.ShiftLeft (aux e1, aux e2) }
      | Ast.ShiftRight (e1, e2) -> { e with expr_desc = Ast.ShiftRight (aux e1, aux e2) }
    in
    aux expr


  (* Gives the expression for the input variables when not using read_input instruction.
  We assume that rand expr are only used for input variables and we have : i_n-1_n = i_n - i_n-1, with i an input variable.
  The expression assigned to i_n-1 is given by prevExprs *)
  let transform_randexpr_inputs v_assigned expr prevExprs =
    match expr.Ast.expr_desc with 
    | Ast.Rand (a, b) -> (
          match StrMap.find_opt v_assigned prevExprs with
          | Some pe -> (
              match pe.Ast.expr_desc with
              | Ast.Rand (c, d) -> (
                  let lb = Bases.RatBase.sub_lb (fst a) (fst d) in
                  let ub = Bases.RatBase.sub_ub (fst b) (fst c) in
                  let new_expr = create_expr_dummy (Ast.Rand ((lb, Q.to_string lb), (ub, Q.to_string ub))) RealT in
                  let prevExprs_updated = StrMap.add v_assigned expr prevExprs in
                  (new_expr, prevExprs_updated) )
              | _ -> assert false )
          | None -> 
              if (StrMap.is_empty prevExprs) then (* if prevExprs, it means that it is iteration 0 *)
                (* Assumes that i_0 is equal to 0 so i_0_1 = i_1 *)
                let prevExprs_updated = StrMap.add v_assigned expr prevExprs in
                (expr, prevExprs_updated)
              else assert false (* this case should never happen *) )
    | _ -> (expr, prevExprs)



  (* assignement modified to use the expression with renamed variables *)
  let assignment x e t =
    let last_state = IntMap.find t.iter t.states in
    let new_x = get_new_name x t.iter in
    let renamed_expr = rename_vars_in_expr t e in
    let renamed_expr_transformed, prevExprs_updated = transform_randexpr_inputs x renamed_expr t.prevExprs in (* Only change the expression and prevExprs if its a rand_expr (so an assignement for a input variable)*)
    let new_state = DomRel.assignment new_x renamed_expr_transformed last_state in
    (* We have to increment the iteration value for the x variable in the varIter map *)
    (* This part is NOT very CLEAN : the issue is that we don't want to count the initialiation of the states variable with a constant as an iteration in the varIter Map 
    So maybe the strategy with the varIter Map is not the best one... *)
    match e.expr_desc with
        | Ast.Cst _ -> { states = IntMap.add t.iter new_state t.states; iter = t.iter; varIter = StrMap.add x (StrMap.find x t.varIter) t.varIter; prevExprs = prevExprs_updated }
        | _ -> { states = IntMap.add t.iter new_state t.states; iter = t.iter; varIter = StrMap.add x ((StrMap.find x t.varIter) + 1) t.varIter; prevExprs = prevExprs_updated }

  let backward_assignment b s x e t = (* TO DO : should be modified to rename variables*)
    let last_state = IntMap.find t.iter t.states in
    let new_state = DomRel.backward_assignment b s x e last_state in
    { t with states = IntMap.add t.iter new_state t.states}

  (* guard modified to use the expression with renamed variables *)
  let guard e t =
    let last_state = IntMap.find t.iter t.states in
    let new_state = DomRel.guard e last_state in
    { states = IntMap.add t.iter new_state t.states; iter = t.iter; varIter = t.varIter; prevExprs = t.prevExprs }

  let project_values t1 t2 r =
    let last_s1 = IntMap.find t1.iter t1.states in
    let last_s2 = IntMap.find t2.iter t2.states in
    let projected = DomRel.project_values last_s1 last_s2 r in
    { states = IntMap.add t1.iter projected t1.states; iter = t1.iter; varIter = t1.varIter; prevExprs = t1.prevExprs }

  (* For read_input : Creates a state and an Ast.expr for a variable at a given iteration *)
  let create_expr_input var iter prev_expr new_expr acc =
    let var_name, ty = var in
    let str_prev_expr = Format.asprintf "%a" Ast.fprint_expr prev_expr in
    let str_new_expr = Format.asprintf "%a" Ast.fprint_expr new_expr in
    Format.printf "read_input: injection of %s - %s in %s at the iteration %d@.%!" str_prev_expr str_new_expr var_name iter;
    let expr_assign = create_expr_dummy (Ast.Binop (Ast.Minus, new_expr, prev_expr)) ty
    in
    DomRel.assignment (get_new_name var_name iter) expr_assign acc

  (* For read_input : Updates t with new state and new prev_var *)
  let update_t_for_var iter t_acc var prev_expr new_expr =
    let current_state = IntMap.find iter t_acc.states in
    let new_state = create_expr_input var iter prev_expr new_expr current_state in
    {
      t_acc with
      states = IntMap.add iter new_state t_acc.states;
      prevExprs = StrMap.add (fst var) new_expr t_acc.prevExprs;
    }

  (* For read_input : Injection of constraints which applies on at least one of the input vars given to read_input instruction, and at the current iteration, to a state *)
  let inject_input_constraints scenarios iter prevExprs prevExprs_updated vars state t =
    let env = DomRel.get_vars state in
    let top_state = DomRel.top env in
    let projections =
      List.map (fun (var_name, var_ty) ->
        let var_name = get_new_name var_name (StrMap.find var_name t.varIter) in
        let expr = create_expr_dummy (Ast.Var var_name) var_ty in
        (expr, (var_name, var_ty))
      ) vars
    in

    List.fold_left (fun (st, prevExprs_acc) scenario ->
      if iter >= scenario.Scenario.Constraint.k_min
      && iter <= scenario.Scenario.Constraint.k_max then
        let lv_names = List.map fst vars in
        let lv_names_set = Scenario.NameSet.of_list lv_names in
        let filtered_constraints =
          List.filter (fun (_, var_names) ->
            Scenario.NameSet.exists (fun name -> Scenario.NameSet.mem name lv_names_set) var_names ) scenario.constraints
        in
        if filtered_constraints <> [] then
          let state_inputs_top = DomRel.project_values top_state st projections in
          (* Transformation contraints so that it use only transition variables : if we have a constraint i_2 + 2*j_2 <= 2*k_2 then it can be translated as : i_1_2 + 2*j_1_2  <= 2*(k_1_2 + k_1_0 + k_0) - i_1 - 2*j_1, and i_1 for example is equal to (i_0_1 + i_0) *)
          let updated_constraints = List.map (fun (c,_) -> transform_expr iter prevExprs c) filtered_constraints in
          (* Get the variables input variables that are present in the constraints (all lv variables might not be present) *)
          let vars_in_constraints filtered_constraints = List.fold_left (fun acc (_, varset) -> Scenario.NameSet.union acc varset) Scenario.NameSet.empty filtered_constraints
          in
          let vars_to_update = Scenario.NameSet.remove "k" (vars_in_constraints filtered_constraints)
          in
          (* When the value of a input variable is defined by a constraint and not intervals, its corresponding previous expression in prevExprs
          has to be calculated based on the transition variables and can not be direclty calculted by substracting intervals.
          Therefore, for the variable i at the iteration n, if a constraint has been applied to i at iteration n-1, the previous expression given by prevExprs (so at iteration n-1) will be : i_(n-2)_(n-1) + i_(n-3)_(n-2) ... + i_0 *)
          let list_exprs = Scenario.NameSet.fold (fun var acc -> let expr = sum_var_until var iter in (var, expr) :: acc) vars_to_update []
          in
          let new_prevExprs = List.fold_left (fun acc (var, expr) -> StrMap.add var expr acc) prevExprs_acc list_exprs
          (****)
          in
          (* Applying constraints *)
          let t_with_constraints =
            let t0 = { t with states = IntMap.add iter state_inputs_top t.states } in
            List.fold_left (fun acc c -> guard c acc) t0 updated_constraints
          in
          (IntMap.find iter t_with_constraints.states, new_prevExprs)
        else
          (st, prevExprs_acc)
      else
        (st, prevExprs_acc)
    ) (state, prevExprs_updated) scenarios

  (* Function called when a read_input() instruction is used.
  The paramteter "vars" is a Ast.Var list corresponding to the input variables given to the read_input instruction.
  This function inject the specified intervals of values and constraints for the variables in vars for the current iteration. *)
  let read_input t vars =
    let iter = t.iter in
    let scenario = S.scenario in
    let prevExprs = t.prevExprs in (* We keep a copy of the expressions for iter-1 *)
    (* Apply upadates to t for each input variable (creating expr for the input variable given in the scenario and adding new expr to prevExprs) *)
    let t' =
      List.fold_left (fun acc var ->
        match S.get scenario var iter with
        | Some (lb,ub) ->
            let new_expr_desc = Ast.Rand((Q.of_float lb, string_of_float lb), (Q.of_float ub, string_of_float ub)) in
            let new_expr = create_expr_dummy new_expr_desc RealT (*expression asigned to var_iter*)
            in
            (*let itv1 =*)
            let prev_expr =
              if iter = 1 then
                let expr_desc_0 = Ast.Cst(Q.of_float 0.0, "0.0") in
                create_expr_dummy expr_desc_0 RealT (*Assumes that the expression asigned to var_0 is the constant 0*)
              else
                let var_name, _ = var in
                StrMap.find var_name prevExprs (* Get the previous expression assigned to the variable (expression assigned to var_(iter-1)) *)
            in
            update_t_for_var iter acc var prev_expr new_expr
        | None -> acc
      ) t vars
    in

    (* Incrementation of varIter (values have been assigned to the input variables) *)
    let t' =
      { t' with varIter =
          List.fold_left (fun varIter (var_name, _) ->
            StrMap.add var_name ((StrMap.find var_name varIter) + 1) varIter
          ) t'.varIter vars
      }
    in

    let prevExprs_updated = t'.prevExprs in 

    (* Applying the constraints on input variables, and updating prevExprs *)
    let (new_state_with_constraints, new_prevExprs) =
      inject_input_constraints S.scenario_constraints iter prevExprs prevExprs_updated vars
        (IntMap.find iter t'.states) t'
    in

    { t' with states = IntMap.add iter new_state_with_constraints t'.states;
      prevExprs = new_prevExprs }

  
  (* Function to inject constraints on states variable and then perform backpropagation of the constraint *)
  let read_state t _ = t (* TO DO *)



  (*False widening (DomRel.widening is never used). Allows to move on to the next iteration (incrementation of k and iter)*)
  let widening {states= _; iter; varIter = _; prevExprs = _} {states = states'; iter = iter'; varIter = varIter'; prevExprs = prevExprs'} =

    let iter = max iter iter' in

    (* Printing of the total result (for all iterations) when the iteration limit is reached *)
    if iter = S.max_iter then begin
      Format.printf "---------------------------------------------------------------------------------\n" ;
      Format.printf " Results of the set-based simulation for %d iterations : \n" S.max_iter ;
      Format.printf "---------------------------------------------------------------------------------\n" ;
      IntMap.iter (fun i state ->
        Format.printf "  Iteration #%d:@ %a@\n\n" i DomRel.fprint state
      ) states';
      exit 0 end

    else
      let state' = IntMap.find iter' states' in

      (* Initialize k with 0 at the iteration 0 *)
      let state' =
      match iter with
        | 0 -> let k_expr = create_expr_dummy (Ast.Cst (Q.of_float (0.), string_of_float (0.))) RealT in
              DomRel.assignment "k_0" k_expr state'
        | 1 -> let k_expr = create_expr_dummy (Ast.Cst (Q.of_float (1.), string_of_float (1.))) RealT in
              DomRel.assignment "k_0_1" k_expr state'
        | _ ->  state'

      in
      (*********)

      (* Updating of the value of k : assignement of the expression k_n-2_n-1 to k_n-1_n *)
      let expr = create_expr_dummy (Var (get_new_name "k" iter)) RealT in
      let state_with_k = DomRel.assignment (get_new_name "k" (iter+1)) expr state' in

      (****)

      (* incrementation of iter and adding the last calculated state to the Map as the result of for the new iteration*)
      { states = IntMap.add (iter + 1) state_with_k states'; iter = iter + 1; varIter = varIter'; prevExprs = prevExprs' }

      
      (*********)

      

  let to_bounds {states; iter; _} = DomRel.to_bounds (IntMap.find iter states)

  let to_properties _ (* {states; iter; _} *) = []

end
