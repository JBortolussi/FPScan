open Ast
open Nn_types
   
                   
(** Activation functions *)

let zero l = mk_cst_expr l Ast.RealT (Q.zero, "0.") 
    
let map_expr_list ?(bind_fresh_vars=true) f lhs_list expr_list =
  (* if bind_fresh_vars then
       xnew_i = expr_i;
       lhs_i = f(xnew_i); 
     else
       lhs_i = f(expr_i);
     
     Note that f(expr) could be an imperative ite.
*)
    List.fold_right2 (
      fun (lhs, _ (* UNUSED: lhs_typ *)) expr (accu_new_vars, accu_astl) ->
        if bind_fresh_vars then
          let l = expr.expr_loc in
          let newvar_id = lhs ^ "_linexpr" in
          let newvar_expr = mk_expr l Ast.RealT (Var newvar_id) in
          (newvar_id, Ast.RealT)::accu_new_vars,
          Asn(l, newvar_id, expr)::(f lhs newvar_expr)::accu_astl
        else
          accu_new_vars,
          (f lhs expr)::accu_astl
      ) lhs_list expr_list ([],[])
          
(* returns (ast_list, vars) with a list of updated ast. *)
let relu ?(bind_fresh_vars=true) =
  (* if x <= 0, then 0 else x *)
  let relu_fun lhs expr =
    let l = Location.dummy () in (* TODO recuperer la bonne loc *)
    let cond = mk_cond l (opposite expr) Strict in 
    let lhs_expr_0 = Asn(l, lhs, zero l) in
    let lhs_expr = Asn(l, lhs, expr) in
    Ite(l, cond, lhs_expr_0, lhs_expr)
  in
  map_expr_list ~bind_fresh_vars relu_fun

let sat (min_bound, max_bound) ?(bind_fresh_vars=true) =
  (* if x < min, min else if x > max, max else x *)
  let sat_fun lhs expr =
    let l = Location.dummy () in (* TODO recuperer la bonne loc *)
    let cst_min = mk_cst_expr l Ast.RealT min_bound in 
    let cst_max = mk_cst_expr l Ast.RealT max_bound in 
    let cond_min = mk_cond l (sub cst_min expr) Strict in 
    let cond_max = mk_cond l (sub expr cst_max) Strict in 
    let lhs_cst cst = Asn(l, lhs, cst) in
    let lhs_expr = Asn(l, lhs, expr) in
    Ite(l, cond_min, lhs_cst cst_min, Ite(l, cond_max, lhs_cst cst_max, lhs_expr))
  in
  map_expr_list ~bind_fresh_vars sat_fun
    

let tanh ?(bind_fresh_vars=true) =
  let tanh_fun lhs expr =
    let l = Location.dummy () in (* TODO recuperer la bonne loc *)
    Asn(l, lhs, mk_expr l Ast.RealT (Call("tanh", [expr])))
  in
  map_expr_list ~bind_fresh_vars tanh_fun


(* checking dimension *)
let rec check layers dim_in dim_out =
  match layers with
  | [] -> false (* should not occur *)
  | (coeffs, _ (* act *))::other_layers -> (
    (* all coeffs have same number of elements = dim_in *)
    let ok = List.for_all (fun c -> List.length c = dim_in) coeffs in
    ok &&
      let nb_neurons = List.length coeffs in
      match other_layers with
      | [] -> (* last layer, check out dim *)
         nb_neurons = dim_out
      | _ -> check other_layers nb_neurons dim_out
  )

let rec lin_comb coeffs (vars: Var.t list) =
  let l = Location.dummy () in
  let typ = Ast.RealT in
  let c_times_v c v =
    let c_expr = mk_cst_expr l typ c in
    let v_expr = mk_expr l Ast.RealT (Var v)  in
    mk_expr l typ (Binop(Times, c_expr, v_expr))
  in
  match coeffs, vars with
  | [c], [v, _] -> c_times_v c v
  | c::cl, (v, _)::vl -> mk_expr l typ (Binop(Plus, c_times_v c v, lin_comb cl vl))
  | _ -> assert false
     
(* nn_vars, nn_ast = build_nn input_vars output_vars local_var_prefix layers
   Build an AST corresponding to the function
   output_vars = Sum_{l \in layers}
   where each layer is a list of list of coefficients + an activation function
*)  
let build_nn input_vars output_vars local_var_prefix (layers: nn_t) =
  let dim_in = List.length input_vars in
  let dim_out = List.length output_vars in
  
  (* Basic check *)
  let ok = check layers dim_in dim_out in
  if not ok then assert false;

  let rec build layers id vars_in  =
    match layers with
    | [] -> assert false
    | (coeffs_l, act)::other_layers -> (
      let expr_l = List.map (fun coeffs -> lin_comb coeffs vars_in) coeffs_l in
      let nb_neurons = List.length expr_l in
      let is_last_layer = other_layers = [] in
      let lhs, new_vars =
        if is_last_layer then
          (* last layer. No need to bind new vars. Use the output vars *)
          output_vars, []
        else
          let new_vars = List.init
                           nb_neurons
                           (fun neuron_ith ->
                             local_var_prefix ^ (string_of_int id) ^ "_" ^ (string_of_int neuron_ith), Ast.RealT)
          in
          new_vars, new_vars
      in
      
      let new_vars', ast_l =
        match act with
        | Relu -> relu lhs expr_l
        | Sat (bound_min, bound_max) -> sat (bound_min, bound_max) lhs expr_l
        | TanH -> tanh lhs expr_l
      in
      if is_last_layer then
        (new_vars @ new_vars'), ast_l
      else
        let tail_vars, tail_ast = build other_layers (id+1) lhs in
        (new_vars @ new_vars' @ tail_vars), (ast_l @ tail_ast)
    )
  in  
  let vars, stm_l = build layers 0 input_vars in
  if stm_l = [] then assert false else
    vars,
    List.fold_left (fun accu stm ->
        Seq(Location.dummy(), accu, stm)
      ) (List.hd stm_l) (List.tl stm_l)
      
                   (*** Loading CSV ***)
      
let build_nn_from_csv file_act_list =
  let layers =
    List.map (
        fun (csv_file, act) ->
        let csv_content = Csv.load csv_file in
        let layer_coeffs = List.map (fun row -> List.map (fun col -> Q.of_string col, col) row) csv_content in
        layer_coeffs, act
      ) file_act_list
  in
  (* *)
  let fst_layer, _ = List.hd layers in
  let nb_inputs = List.length (List.hd fst_layer) in
  let lst_layer, _ = List.hd (List.rev layers) in
  let nb_outputs = List.length lst_layer in

   let input_vars = List.init nb_inputs (fun i -> "i" ^ string_of_int i, Ast.RealT) in
   let output_vars = List.init nb_outputs (fun i -> "o" ^ string_of_int i, Ast.RealT) in
   let new_vars, nn_prog = build_nn input_vars output_vars "neuron" layers in
   input_vars, output_vars, new_vars, nn_prog

let type_act _ (* UNUSED: env *) _ (* UNUSED: typ *) act =
  match act with
  | Ast.UVar (_ (* UNUSED: l *), "relu") -> Relu
  | Ast.UVar (_ (* UNUSED: l *), "tanh") -> TanH
  | _ -> (failwith "Not a valid activation function")
   
let process_ast vars ast =
  let rec process_stmt vars stm =
    match stm with
    | Asn _ | Asrt _ | Nop _ | Nde _ -> vars, stm
    | Seq (l, s1, s2) ->
       let vars, s1 = process_stmt vars s1 in
       let vars, s2 = process_stmt vars s2 in
       vars, Seq(l, s1, s2)
    | Ite(l,e,s1,s2) ->
       let vars, s1 = process_stmt vars s1 in
       let vars, s2 = process_stmt vars s2 in
       vars, Ite(l, e, s1, s2)
    | While (l, e, s) ->
       let vars, s = process_stmt vars s in
       vars, While(l, e, s)

    | ReadInput (l, lv) -> vars, ReadInput(l, lv)

    | ReadState (l, lv) -> vars, ReadState(l, lv)

    | NN (_ (* UNUSED: l *), ins, layers, outs) ->
       let ins = List.map (fun n -> n , Ast.RealT) ins in
       let outs = List.map (fun n -> n , Ast.RealT) outs in
       let layers =
         List.map (
             fun (csv_file, act) ->
             let csv_content = Csv.load csv_file in
             let layer_coeffs = List.map (fun row -> List.map (fun col -> Q.of_string col, col) row) csv_content in
             layer_coeffs, act
           ) layers
       in
  
       let new_vars, nn_prog =
         build_nn ins outs "nn" layers in
       Ast.Var.Set.union (Ast.Var.Set.of_list new_vars) vars, nn_prog
  in
  process_stmt vars ast
