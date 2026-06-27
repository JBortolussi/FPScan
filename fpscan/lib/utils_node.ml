(*
 * TINY (Tiny Is Not Yasa (Yet Another Static Analyzer)):
 * a simple abstract interpreter for teaching purpose.
 * Copyright (C) 2012, 2014  P. Roux
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let suffix_args_var = "_from_args"

(* Return the Ast.Var.Set of variables present in the arguments of the node *)
let vars_of_args args inputs = 
  List.fold_left2 (fun s e (_,typ) -> 
    Name.Set.fold (fun n s -> Ast.Var.Set.add (n,typ) s ) (Ast.vars_of_expr e) s) 
  Ast.Var.Set.empty args inputs


(* Suffixe the name of a set of var *)
let suffixe_vars_name vars str = 
  Ast.Var.Set.map (fun (n,typ) -> (n^str,typ)) vars


(* to_list function for Ast.Var.Set *)
let var_set_to_list vars = 
  Ast.Var.Set.fold (fun v l -> v::l) vars []


(* Reverse a map containing expressions only representing variables *)
let rev_map_of_var map l =
  List.map (fun (j, (i,typ)) ->
    match j.Ast.expr_desc with
    | Ast.Var (n) -> Ast.mk_expr l typ (Ast.Var(i)), (n,typ)
    | _ -> Format.eprintf "Expression %a@ is not a variable@." Ast.fprint_expr j; assert false
    ) 
  map 


(* Return the Ast.Var.Set of external memory variables for the call to node name *)
let vars_of_mem map_shared name l =
  let rel_mems = List.assoc name map_shared in
  let rel_rev = rev_map_of_var rel_mems l in
  List.fold_left (fun s (_,v) -> Ast.Var.Set.add v s) Ast.Var.Set.empty rel_rev


(* Get environment associated to a node according to the variables present
    in the arguments of the call *)
let get_env_node_in e_id vars_node name args l = 
  try
    let env , inputs, _, map_shared = List.assoc e_id vars_node in 
    let vars_of_args = vars_of_args args inputs in
    let vars_of_mem = vars_of_mem map_shared name l in
    let env_with_vars_of_args = Ast.Var.Set.union env (suffixe_vars_name vars_of_args suffix_args_var) in
    let env_with_mem = Ast.Var.Set.union env_with_vars_of_args vars_of_mem in
    env_with_mem, inputs, map_shared
  with Not_found ->
    Format.eprintf "Unable to find node %s@." name;
    assert false


(* Create relation between inputs and args *)
let rec rel_arg_input inputs args name =
  match args, inputs with
  | [], [] -> []
  | e::qargs, (n,typ)::qinputs -> (
    let rel = rel_arg_input qinputs qargs name in
    (e,(n,typ))::rel 
  )
  | e::_ , [] -> (
    Format.eprintf "This arguments %a is surplus for node %s@." Ast.fprint_expr e name;
    assert false
  )
  | [], (m,_)::_ -> (
    Format.eprintf "Missing arguments %s for node %s@." m name ;
    assert false
  )


(* Transform a list of var to a relational identity list of (Ast.Var(n), (n ^ s,typ))
with left element suffixed by suffix_args_var *)
let map_rel_args var_list l = 
  List.map (fun (n,typ) -> (Ast.mk_expr l typ (Ast.Var(n)), (n ^ suffix_args_var,typ))) var_list


(* Transform a list of var into a relational identity list of (Ast.Var(n), (n,typ)) *)
let ident_rel var_list l =
  List.map (fun (n,typ) -> (Ast.mk_expr l typ (Ast.Var(n)), (n,typ))) var_list


(* Build the input relation from the inputs, the variables present in the arguments 
   of the call and the shared variables *)
let get_rel_in args inputs map_shared name node_id l =
  let vars_of_mem_rel = 
    let vars_of_mem = vars_of_mem map_shared name l in
    let l_vars_of_mem = var_set_to_list vars_of_mem in
    ident_rel l_vars_of_mem l in
  let vars_of_args_rel = 
    let vars_of_args = vars_of_args args inputs in
    let l_vars_of_args = var_set_to_list vars_of_args in
    map_rel_args l_vars_of_args l in
  let inputs_relations = rel_arg_input inputs args node_id in
  let map_assoc_to_super_node = List.assoc name map_shared in
  inputs_relations @ vars_of_args_rel @ map_assoc_to_super_node @ vars_of_mem_rel


(* Return the environment and the entry relation for the node name *)
let get_env_rel_in node_id env_nodes name args l =
  let env, inputs, map_shared = get_env_node_in node_id env_nodes name args l in
  let rel = get_rel_in args inputs map_shared name node_id l in
  env, rel


(* Create relation between outputs and returned values *)
let rec rel_returned_output outputs ln l name = 
  match outputs, ln with
  | [], [] -> []
  | (o,typ)::qout, n::qn -> (
    let rel = rel_returned_output qout qn l name in
    (Ast.mk_expr l typ (Ast.Var(o)), (n,typ))::rel 
  )
  | _::_ , [] -> (
    Format.eprintf "Missing assigned variabes for node %s@," name;
    assert false
  )
  | [], _::_ -> (
    Format.eprintf "Too many variables assign for node %s@," name ;
    assert false
  )


(* Build the output relation from the inputs, the outputs and the shared variables *)
let get_rel_out node_id env_node args name ln l = 
  let _ , inputs, outputs, map_shared = List.assoc node_id env_node in 
  let vars_of_args = vars_of_args args inputs in
  let l_vars_of_args = var_set_to_list vars_of_args in
  let args_relations = rev_map_of_var (map_rel_args l_vars_of_args l) l in
  let outputs_relations = rel_returned_output outputs ln l name in
  let map_assoc_to_super_node = rev_map_of_var (List.assoc name map_shared) l in
  outputs_relations @ args_relations @ map_assoc_to_super_node