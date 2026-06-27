(* "Domain" for set-based simulation with injection of scenarios (intervals of values and constraints depending on k (iterator)) for input variables
The widening is not a real widening, its role is to pass to the next iteration *)

module DomSim (DomRel : Relational.Domain) (S : Scenario.Scenario_sig) : Relational.Domain = struct

  let name = "simulation"

  let nonrel_base = None
  let is_partitioned () = false

  let parse_param _ = ()

  let fprint_help fmt = Format.fprintf fmt "Scenario-based simulation domain.\n"

  module IntMap = Map.Make(Int) 

  (*Changer le type pour enlever scenario, --> passer par get ???*)

  type t = {
    states : DomRel.t IntMap.t;
    iter : int;
    assignments : (Name.t * Ast.expr) list IntMap.t;
    iter_injected : int;
  }

  let fprint fmt {states; iter; assignments=_; _} =
    Format.fprintf fmt "Simulation domain states by iteration:@\n";
    Format.fprintf fmt "  Iteration #%d:@ %a@\n" iter DomRel.fprint (IntMap.find iter states)

  let json {states; iter; assignments=_; _} = DomRel.json (IntMap.find iter states)

  let top s = { states = IntMap.singleton 0 (DomRel.top s); iter = 0; assignments = IntMap.empty; iter_injected = (-2) }

  let bottom s = {states = IntMap.singleton 0 (DomRel.bottom s); iter = 0; assignments = IntMap.empty; iter_injected = (-2) }

  let is_bottom {states; iter; assignments=_; _} = DomRel.is_bottom (IntMap.find iter states)

  let get_vars {states; iter; assignments=_; _} = DomRel.get_vars (IntMap.find iter states)

  let order _ _ =  false (* Fixpoint will never be reached *)

  let join t1 t2 =
    (*The join sould be used only on abstract states of the same iteration*)
    if t1.iter <> t2.iter then
      failwith (Format.sprintf "DomSim.join: mismatched iterations (%d vs %d)" t1.iter t2.iter)
    else
      let last_s1 = IntMap.find t1.iter t1.states in
      let last_s2 = IntMap.find t2.iter t2.states in
      let joined_s1s2 = DomRel.join last_s1 last_s2 in
      { states = IntMap.add t1.iter joined_s1s2 t1.states; iter = t1.iter; assignments = t1.assignments; iter_injected = t1.iter_injected}
  
  (* Intersection should not be used*)
  let meet _ _  = assert false

  let assignment x e t =
    let last_state = IntMap.find t.iter t.states in
    let new_state = DomRel.assignment x e last_state in
    let new_assigns =
      let assigns_k =
        match IntMap.find_opt t.iter t.assignments with
        | Some l -> (x, e) :: l
        | None -> [ (x, e) ]
      in
      IntMap.add t.iter assigns_k t.assignments
    in
    { states = IntMap.add t.iter new_state t.states; iter = t.iter; assignments = new_assigns; iter_injected = t.iter_injected}

  let backward_assignment b s x e t =
    let last_state = IntMap.find t.iter t.states in
    let new_state = DomRel.backward_assignment b s x e last_state in
    { t with states = IntMap.add t.iter new_state t.states}

  let guard e t =
    let last_state = IntMap.find t.iter t.states in
    let new_state = DomRel.guard e last_state in
    { t with states = IntMap.add t.iter new_state t.states }

  let project_values t1 t2 r =
    let last_s1 = IntMap.find t1.iter t1.states in
    let last_s2 = IntMap.find t2.iter t2.states in
    let projected = DomRel.project_values last_s1 last_s2 r in
    { states = IntMap.add t1.iter projected t1.states; iter = t1.iter; assignments = t1.assignments; iter_injected = t1.iter_injected}


  (*Function called when a read_input() instruction is used.
  The paramteter "vars" is a Ast.Var list corresponding to the input variables given to the read_input instruction.
  This function inject the specified intervals of values and constraints for the variables in vars for the current iteration.*)

  let read_input t vars =

    let iter = t.iter in
    (* Récupère l'état à la dernière iter *)
    (*let current_state = IntMap.find iter t.states in*)
    (* For each variable given in a read_input instruction, we get the corresponding interval in the scenario and assign it to the variable *)
    let scenario = S.scenario in
    let new_t =
      List.fold_left (fun acc var ->
        (*partie injection de scénario*)
        match S.get scenario var iter with
        | Some (a, b) ->
            let var_name, ty = var in
            Format.printf "read_input: injection of [%f, %f] in %s at the iteration %d@.%!"
              a b var_name iter;
            let expr = S.create_rand_expr a b ty in
            (*DomRel.assignment var_name expr acc (*ICI*)*)
            assignment var_name expr acc
        | None -> acc
      ) t vars
    in
    let new_state = IntMap.find iter new_t.states
    in

    (*Function to inject the contraints --> check if it applies to the current value of k --> and then apply all the constraints that does*)
      let inject_input_constraints scenarios iter vars state =
        (*Ou alors récupérer plutôt les variables d'inputs qu'on a dans les contraintes au cas ou contraites pas sur toutes les vars données en argument de read_input ??*)
        
        (*Get all the variables of the environment and set them to top*)
        let env = DomRel.get_vars state in
        let top_state = DomRel.top env in

        (* Mapping function for the project_values() function : Construction of the (expr, var) with the vars (variables given to the the read_input() instruction)
        so that only those variables will be set to top using the project_values() function *)
        let projections = List.map (fun (var_name, var_ty) ->
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
            if not (List.length filtered_constraints = 0) then
              (*Set the variables given to read_input to top*)
              let state_inputs_top = DomRel.project_values top_state st projections in
              (* Inject constraints *)
              List.fold_left (fun acc (c, _) -> DomRel.guard c acc) state_inputs_top filtered_constraints
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


  (* Performs injection of constraints on state variables given to read_state instruction (in the lv list) and does backpropagation on the previous states *)
  let read_state t lv =

    (* Performs backward propagation from a state at iteration k0 to which constraints have been appplied on states variables *)
    (* When this function is called, the constraints have already been applied to iteration k0 *)
    let backward_propagate k0 t =
      let rec propagate k acc_states =
        if k < 0 then acc_states
        else
          let state_kplus1 = IntMap.find (k+1) acc_states in
          let assignments =
            match IntMap.find_opt k t.assignments with
              | Some a -> a
              | None -> []
            in
            let input_k_vars = Scenario.NameSet.add "k" S.input_vars
            in
            (*filtering était ici*)
            (*****)
            (* Set k and inputs vars to their initial states using projection *)
            let projections = List.map (fun var_name ->
              let expr = { Ast.expr_desc = Ast.Var var_name; expr_loc = Location.dummy (); expr_type = Ast.RealT } in
              (expr, (var_name, Ast.RealT))
            ) (Scenario.NameSet.elements input_k_vars)
            in
            let state_kplus1_proj = DomRel.project_values (IntMap.find k acc_states) state_kplus1 projections
            in
            
            Format.printf "\n--------------------------------------------------------------------------\n\n";
            Format.printf "[BACKPROPAGATION] backpropagation for iteration %d :\n" k;

            let refined_k =
              if k = (k0-1) then
                List.fold_left (fun acc (var, expr) -> DomRel.backward_assignment true S.input_vars var expr acc) state_kplus1_proj assignments
              else
                List.fold_left (fun acc (var, expr) -> DomRel.backward_assignment false S.input_vars var expr acc) state_kplus1_proj assignments
            in
            (*let refined_k = DomRel.assignment "y" { expr_desc = Var "ny"; expr_loc = Location.dummy(); expr_type = RealT } refined_k in*)
            let intersection = DomRel.meet refined_k (IntMap.find k acc_states) in
            let acc_states = IntMap.add k intersection acc_states in
            propagate (k - 1) acc_states
      in
      let final_states = propagate (k0 - 1) t.states in
      { t with states = final_states }
    in
    (*****)

    (* Performs injection of constraints on state variables given to read_state instruction AND BACKPROPAGATION *)
    let inject_state_constraints scenarios iter t =

      let t =
        if iter = (t.iter_injected + 1) then
          let t_backprop = backward_propagate (iter - 1) t in
          t_backprop  
        else
          t
      in

      List.fold_left (fun t_acc scenario ->
        (* check if the constraints apply to the current iteration *)
        if iter >= scenario.Scenario.Constraint.k_min && iter <= scenario.Scenario.Constraint.k_max then
          (* Apply only constraints involving variables in lv *)
          let lv_names = Scenario.NameSet.of_list (List.map fst lv) in
          let filtered_constraints =
            List.filter (fun (_, var_names) -> Scenario.NameSet.exists (fun name -> Scenario.NameSet.mem name lv_names) var_names) scenario.constraints
          in
          if filtered_constraints <> [] then (
            let t_with_constraints = List.fold_left (fun acc (c, _) -> guard c acc) t_acc filtered_constraints
            in
            Format.printf "Injection at iter %d\n" iter;
            {t_with_constraints with iter_injected = iter}
          ) else
            t_acc
        else
          t_acc
      ) t scenarios
  in
  Format.printf "Injection of constraint on state variable at iter %d\n" t.iter;
  inject_state_constraints S.scenario_constraints_states t.iter t


  
  (*False widening (DomRel.widening is never used). Allows to move on to the next iteration (incrementation of k and iter)*)
  let widening {states=_; iter; assignments=_; iter_injected =_} {states = states'; iter = iter'; assignments = assignments'; iter_injected = iter_injected'} =
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
              DomRel.assignment "k" k_expr state' (*ICI !! peut être modifier pour qu'il soit pris en compte dans les assignments*)
        | _ ->  state'

      in

      (* Updating of the value of k : assignement of the expression k+1 to k *)
      let k_expr = Ast.Binop(Ast.Plus, { expr_desc = Var "k"; expr_loc = Location.dummy(); expr_type = RealT }, { expr_desc = Cst (Q.of_float 1.0, "1.0"); expr_loc = Location.dummy(); expr_type = RealT }) in
      let expr = { Ast.expr_desc = k_expr; Ast.expr_loc = Location.dummy(); Ast.expr_type = RealT } in
      let state_with_k = DomRel.assignment "k" expr state' in (*ICI*)

      (* incrementation of iter and adding the last calculated state to the Map as the result of for the new iteration*)
      { states = IntMap.add (iter + 1) state_with_k states'; iter = iter + 1; assignments = assignments'; iter_injected = iter_injected' }

      

  let to_bounds {states; iter; _} = DomRel.to_bounds (IntMap.find iter states)

  let to_properties _ (* {states; iter; _} *) = []

end
