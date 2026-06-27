(* "Domain" for set-based simulation with injection of scenarios (intervals of values and constraints depending on k (iterator)) for input variables
The widening is not a real widening, its role is to pass to the next iteration. 
Compared to DomSim, DomSim2 uses (k+1)*n variables with k being the number of iterations and n being the number of variables in the program*)

module DomSim2 (DomRel : Relational.Domain) (S : Scenario.Scenario_sig) : Relational.Domain = struct

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
    varIter : int StrMap.t; (* varIter allows to keep track of the last iteration for each variables, it is used to rename correctly the variables in the expressions
    given to assignment and guard *)
  }

  let fprint fmt {states; iter; _} =
    Format.fprintf fmt "Simulation domain states by iteration:@\n";
    Format.fprintf fmt "  Iteration #%d:@ %a@\n" iter DomRel.fprint (IntMap.find iter states)

  let json {states; iter; _} = DomRel.json (IntMap.find iter states)

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

  let top s =
  (* varIter is initialized : the keys are all the variables without the iteration number (x, y, k etc.) *)
  { states = IntMap.singleton 0 (DomRel.top s); iter = 0; varIter = initialize_varIter s }

  let bottom s =
  (* varIter is initialized : the keys are all the variables without the iteration number (x, y, k etc.) *)
  {states = IntMap.singleton 0 (DomRel.bottom s); iter = 0; varIter = initialize_varIter s }


  let is_bottom {states; iter; _} = DomRel.is_bottom (IntMap.find iter states)

  let get_vars {states; iter; _} = DomRel.get_vars (IntMap.find iter states)

  let order _ _ = false (* False order function : always false as fixed point will never be reached *)
  
  let join t1 t2 =
    (*The join sould be used only on abstract states of the same iteration*)
    if t1.iter <> t2.iter then
      failwith (Format.sprintf "DomSim.join: mismatched iterations (%d vs %d)" t1.iter t2.iter)
    else
      let last_s1 = IntMap.find t1.iter t1.states in
      let last_s2 = IntMap.find t2.iter t2.states in
      let joined_s1s2 = DomRel.join last_s1 last_s2 in
      { states = IntMap.add t1.iter joined_s1s2 t1.states; iter = t1.iter; varIter = t1.varIter }
  
  (* Intersection should not be used*)
  let meet _ _ = assert false


  (* Function to renamed correctly (with the correct iteration), the variables in an expression.
  To do so, we use the map varIter, who indicated the last iteration for each variable *)
  let rename_vars_in_expr t expr =
    let open Ast in
    let rec aux expr =
        match expr.expr_desc with
        | Var v_name ->
            let new_var_name =
              if v_name = "k" then v_name ^ "_" ^ string_of_int t.iter
            else v_name ^ "_" ^ string_of_int (StrMap.find v_name t.varIter)
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


  (* assignement modified to use the expression with renamed variables *)
  let assignment x e t =
    let last_state = IntMap.find t.iter t.states in
    let new_x = x ^ "_" ^ string_of_int t.iter in
    let renamed_expr = rename_vars_in_expr t e in
    let new_state = DomRel.assignment new_x renamed_expr last_state in
    (* We have to increment the iteration value for the x variable in the varIter map *)
    (* This part is NOT very CLEAN : the issue is that we don't want to count the initialiation of the states variable with a constant as an iteration in the varIter Map 
    So maybe the strategy with the varIter Map is not the best one... *)
    match e.expr_desc with
        | Ast.Cst _ -> { states = IntMap.add t.iter new_state t.states; iter = t.iter; varIter = StrMap.add x (StrMap.find x t.varIter) t.varIter }
        | _ -> { states = IntMap.add t.iter new_state t.states; iter = t.iter; varIter = StrMap.add x ((StrMap.find x t.varIter) + 1) t.varIter }

  let backward_assignment b s x e t = (*A modifier pour renommer ??*)
    let last_state = IntMap.find t.iter t.states in
    let new_state = DomRel.backward_assignment b s x e last_state in
    { t with states = IntMap.add t.iter new_state t.states}

  (* guard modified to use the expression with renamed variables *)
  let guard e t =
    let last_state = IntMap.find t.iter t.states in
    let renamed_expr = rename_vars_in_expr t e in
    let new_state = DomRel.guard renamed_expr last_state in
    { states = IntMap.add t.iter new_state t.states; iter = t.iter; varIter = t.varIter }

  let project_values t1 t2 r =
    let last_s1 = IntMap.find t1.iter t1.states in
    let last_s2 = IntMap.find t2.iter t2.states in
    let projected = DomRel.project_values last_s1 last_s2 r in
    { states = IntMap.add t1.iter projected t1.states; iter = t1.iter; varIter = t1.varIter }


  (*Function called when a read_input() instruction is used.
  The paramteter "vars" is a Ast.Var list corresponding to the input variables given to the read_input instruction.
  This function inject the specified intervals of values and constraints for the variables in vars for the current iteration.*)

  let read_input t vars =

    let iter = t.iter in
    (* Get the state for the last iteration *)
    let current_state = IntMap.find iter t.states in
    (* For each variable given in a read_input instruction, we get the corresponding interval in the scenario and assign it to the variable *)
    let scenario = S.scenario in
    let new_state =
      List.fold_left (fun acc var ->
        (*partie injection de scénario*)
        match S.get scenario var iter with
        | Some (a, b) ->
            let var_name, ty = var in
            Format.printf "read_input: injection of [%f, %f] in %s at the iteration %d@.%!"
              a b var_name iter;
            let expr = S.create_rand_expr a b ty in
            (*DomRel.assignment var_name expr acc*)
            DomRel.assignment (var_name ^ "_" ^ string_of_int iter) expr acc
        | None -> acc
      ) current_state vars
    in
    
    let t = {t with varIter = 
      (List.fold_left (fun varIter (var_name, _) -> StrMap.add var_name ((StrMap.find var_name varIter) + 1) varIter) t.varIter vars)}
    in

    (*Function to inject the constraints on input variables --> check if it applies to the current value of k --> and then apply all the constraints that does*)
      let inject_input_constraints scenarios iter vars state =
        (*Ou alors récupérer plutôt les variables d'inputs qu'on a dans les contraintes au cas ou contraites pas sur toutes les vars données en argument de read_input ??*)
        
        (*Get all the variables of the environment and set them to top*)
        let env = DomRel.get_vars state in
        let top_state = DomRel.top env in
        
        (* Mapping function for the project_values() function : Construction of the (expr, var) with the vars (variables given to the the read_input() instruction)
        so that only those variables will be set to top using the project_values() function *)
        let projections = List.map (fun (var_name, var_ty) ->
          (* Variable must also be renamed here *)
          let var_name = var_name ^ "_" ^ string_of_int (StrMap.find var_name t.varIter) in 
          let expr = { Ast.expr_desc = Ast.Var var_name; expr_loc = Location.dummy (); expr_type = var_ty } in
          (expr, (var_name, var_ty))
        ) vars in

        List.fold_left (fun st scenario ->
          (* check if the constraints apply to the current iteration *)
          if iter >= scenario.Scenario.Constraint.k_min && iter <= scenario.Scenario.Constraint.k_max then
            (*Applying only the constraints that include AT LEAST ONE of the variables given to read_input instruction (in lv)*)
            let lv_names = Scenario.NameSet.of_list (List.map fst vars) in
            let filtered_constraints = List.filter (fun (_, var_names) -> Scenario.NameSet.exists (fun name -> Scenario.NameSet.mem name lv_names) var_names) scenario.constraints
            in
            if filtered_constraints <> [] then
              (*Set the variables given to read_input to top*)
              let state_inputs_top = DomRel.project_values top_state st projections in
              (* Inject constraints *)
              let t_with_constraints = List.fold_left (fun acc (c, _) -> guard c acc) {t with states = IntMap.add t.iter state_inputs_top t.states} scenario.constraints
              in IntMap.find t.iter t_with_constraints.states
            else
              st
          else
            st
        ) state scenarios
    in
    let new_state_with_constraints =      
      inject_input_constraints S.scenario_constraints t.iter vars new_state
    in
    { t with states = IntMap.add iter new_state_with_constraints t.states }

  
  (* Function to inject constraints on states variable and then perform backpropagation of the constraint *)
  let read_state t lv =


    (*Est ce que pour ce type de contrainte ce serait forcemment sur une seule iter ?*)
    let inject_state_constraints scenarios iter state =
      List.fold_left (fun st scenario ->
            (* check if the constraints apply to the current iteration *)
            if iter >= scenario.Scenario.Constraint.k_min && iter <= scenario.Scenario.Constraint.k_max then
              (*Applying only the constraints that include at least one of the variables given to read_state instruction (in lv)*)
              let lv_names = Scenario.NameSet.of_list (List.map fst lv) in
              let filtered_constraints = List.filter (fun (_, var_names) -> Scenario.NameSet.exists (fun name -> Scenario.NameSet.mem name lv_names) var_names) scenario.constraints
              in
              (* Inject constraints *)
              if not (List.length filtered_constraints = 0) then
                let t_with_constraints = List.fold_left (fun acc (c, _) -> guard c acc) {t with states = IntMap.add t.iter st t.states} filtered_constraints
                in IntMap.find iter t_with_constraints.states
              else
                st
            else
              st
          ) state scenarios
    in
    let new_state_with_constraints =
      let current_state = IntMap.find t.iter t.states in
      inject_state_constraints S.scenario_constraints_states t.iter current_state
    in
    { t with states = IntMap.add t.iter new_state_with_constraints t.states }

    (*PROPAGATION ARRIERE : suffisant actuellement ou doit propager sur les itérations précédentes ? *)





  (*False widening (DomRel.widening is never used). Allows to move on to the next iteration (incrementation of k and iter)*)
  let widening {states= _; iter; varIter= _} {states = states'; iter = iter'; varIter = varIter'} =

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
        | 0 -> let k_expr = { Ast.expr_desc = Ast.Cst (Q.of_float (0.), string_of_float (0.)); expr_loc = Location.dummy (); expr_type = RealT } in
              DomRel.assignment "k_0" k_expr state'
        | _ ->  state'

      in
      (*********)

      (* Updating of the value of k : assignement of the expression k+1 to k *)
      let k_expr = Ast.Binop(Ast.Plus, { expr_desc = Var ("k"^ "_" ^ string_of_int iter); expr_loc = Location.dummy(); expr_type = RealT }, { expr_desc = Cst (Q.of_float 1.0, "1.0"); expr_loc = Location.dummy(); expr_type = RealT }) in
      let expr = { Ast.expr_desc = k_expr; Ast.expr_loc = Location.dummy(); Ast.expr_type = RealT } in
      let state_with_k = DomRel.assignment ("k" ^ "_" ^ string_of_int (iter+1)) expr state' in


      (* Performs union between previous and current state to have domain relational in k *)
      (*let union_state =
      match iter with
        | 0 -> state_with_k
        | _ ->  let prev_state = IntMap.find (iter - 1) states in
                DomRel.join prev_state state_with_k in*)
      (****)

      (* incrementation of iter and adding the last calculated state to the Map as the result of for the new iteration*)
      { states = IntMap.add (iter + 1) state_with_k states'; iter = iter + 1; varIter = varIter' }

      
      (*********)

      

  let to_bounds {states; iter; _} = DomRel.to_bounds (IntMap.find iter states)
  let to_properties {states; iter; _} = DomRel.to_properties (IntMap.find iter states)


end
