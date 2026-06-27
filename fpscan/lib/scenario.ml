(*********************)
(*  Module Scenario  *)
(*********************)

module VarMap = Map.Make(Ast.Var.OrderedVar)

module Scenario = struct
  type t = (int * (float * float)) list VarMap.t

  let empty = VarMap.empty

  let add var (id, interval) s =
    let current = match VarMap.find_opt var s with Some l -> l | None -> [] in
    VarMap.add var ((id, interval) :: List.remove_assoc id current) s

  let get s var i =
    match VarMap.find_opt var s with
    | Some l -> List.assoc_opt i l
    | None -> None

  let create_rand_expr a b ty =
    let q_a = Q.of_float a in
    let q_b = Q.of_float b in
    let rand_expr_desc = Ast.Rand ((q_a, string_of_float a), (q_b, string_of_float b)) in
    { Ast.expr_desc = rand_expr_desc; Ast.expr_loc = Location.dummy (); Ast.expr_type = ty }

  let print s =
    Format.printf "Content of the scenario :@.";
    VarMap.iter (fun var lst ->
      let name = Format.asprintf "%a" Ast.Var.pp_name var in
      Format.printf "  %s:@." name;
      List.iter (fun (i, (a, b)) -> Format.printf "    Iter %d: [%f, %f]@." i a b) lst
    ) s
end

(*********************************************************************)
(*  Module BuildScenario : parse scenario files and create scenario  *)
(*********************************************************************)

module NameSet = Set.Make(String)

module BuildScenario = struct
  let load_scenario_from_file file_path =
    try
      let ic = open_in file_path in
      let rec read acc =
        match input_line ic with
        | line ->
          let parts = String.split_on_char ' ' line in
          let iter = int_of_string (List.nth parts 0) in
          let a = float_of_string (List.nth parts 1) in
          let b = float_of_string (List.nth parts 2) in
          read ((iter, (a, b)) :: acc)
        | exception End_of_file -> close_in ic; List.rev acc
      in
      read []
    with Sys_error _ ->
      Format.printf "No scenario file called : %s\n%!" file_path;
      []

  let update_scenario_file scen var =
    let name = Format.asprintf "%a" Ast.Var.pp_name var in
    let path = "scenarios/scenario_" ^ name ^ ".txt" in
    let intervals = load_scenario_from_file path in
    List.fold_left (fun acc (id, interval) -> Scenario.add var (id, interval) acc) scen intervals

  let rec construct_scenario ast vars scen decl_vars read_inputs_vars =
    let find_var_type n vars =
      Ast.Var.Set.fold (fun (name, ty) acc ->
        match acc with
        | Some _ -> acc
        | None -> if name = n then Some ty else None
      ) vars None
    in
    match ast with
    | Ast.Asn (_, n, _) when not (NameSet.mem n decl_vars) ->
        let ty = match find_var_type n vars with Some t -> t | None -> Ast.RealT in
        let scen = update_scenario_file scen (n, ty) in
        let decl_vars = NameSet.add n decl_vars in
        scen, decl_vars, read_inputs_vars

    | Ast.ReadInput (_, lv) ->
        let scen, decl_vars =
          List.fold_left (fun (scen, decl_vars) n ->
            if not (NameSet.mem n decl_vars) then
              let ty = match find_var_type n vars with Some t -> t | None -> Ast.RealT in
              let scen = update_scenario_file scen (n, ty) in
              match VarMap.find_opt (n, ty) scen with
              | None -> failwith (Printf.sprintf "Missing scenario file for variable %s" n)
              | Some _ -> (scen, NameSet.add n decl_vars)
            else (scen, decl_vars)
          ) (scen, decl_vars) lv
        in
        (* Ajoute toutes les variables de read_input au set *)
        let read_inputs_vars = List.fold_left (fun acc n -> NameSet.add n acc) read_inputs_vars lv in
        scen, decl_vars, read_inputs_vars

    | Ast.Seq (_, s1, s2)
    | Ast.Ite (_, _, s1, s2) ->
        let scen, decl_vars, read_inputs_vars = construct_scenario s1 vars scen decl_vars read_inputs_vars in
        construct_scenario s2 vars scen decl_vars read_inputs_vars

    | Ast.While (_, _, body) ->
        construct_scenario body vars scen decl_vars read_inputs_vars

    | _ -> scen, decl_vars, read_inputs_vars

end

(***********************)
(*  Module Constraint  *)
(***********************)

module Constraint = struct

  type scenario_constraint = {
    k_min : int;
    k_max : int;
    constraints : (Ast.expr * NameSet.t) list; (* List of constraints and set of vars used in the constraints *)
  }

end


(****************************)
(*  Module BuildConstraint  *)
(****************************)

module BuildConstraint = struct

  type t =
    | Eq of string * string * float
    | Leq of string * (string * float) list * float
    | Geq of string * (string * float) list * float

  (* Convertir BuildConstraint.t en Ast.expr *)
  let t_to_astexpr (c:t) =
    let open Ast in

    let mk_expr desc =
      { expr_desc = desc;
        expr_loc = Location.dummy ();
        expr_type = RealT;
      } 
    in

    match c with

    | Leq (v1, lst, const) ->
        let sum_lst =
          List.fold_left (fun acc (v, coeff) ->
            let term = mk_expr (Binop (Times, mk_expr (Cst (Q.of_float coeff, string_of_float coeff)), mk_expr (Var v))) in
            mk_expr (Binop (Plus, acc, term))
          ) (mk_expr (Cst (Q.of_float 0.0, "0.0"))) lst
        in
        let left = mk_expr (Binop (Minus, sum_lst, mk_expr (Var v1))) in
        let right = mk_expr (Cst (Q.of_float const, string_of_float const)) in
        mk_expr (Cond (mk_expr (Binop (Minus, left, right)), Loose))

    | Geq (v1, lst, const) ->
        let sum_lst =
          List.fold_left (fun acc (v, coeff) ->
            let term = mk_expr (Binop (Times, mk_expr (Cst (Q.of_float coeff, string_of_float coeff)), mk_expr (Var v))) in
            mk_expr (Binop (Plus, acc, term))
          ) (mk_expr (Cst (Q.of_float 0.0, "0.0"))) lst
        in
        let left = mk_expr (Binop (Minus, mk_expr (Var v1), sum_lst)) in
        let right = mk_expr (Cst (Q.of_float const, string_of_float const)) in
        mk_expr (Cond (mk_expr (Binop (Minus, left, right)), Loose))  

    | Eq (v1, v2, const) ->
        let lhs = mk_expr (Binop (Minus, mk_expr (Var v1), mk_expr (Var v2))) in
        let rhs = mk_expr (Cst (Q.of_float const, string_of_float const)) in
        mk_expr (Cond (mk_expr (Binop (Minus, lhs, rhs)), Zero))  

  let rec get_vars_of_constraint c  =
    match c.Ast.expr_desc with
    | Var name -> NameSet.singleton name
    | Cst _ -> NameSet.empty
    | Binop (_, e1, e2) -> NameSet.union (get_vars_of_constraint e1) (get_vars_of_constraint e2)
    | Cond (e, _) -> get_vars_of_constraint e
    | _ -> failwith "Unexpected expr_desc in constraint"

  let create_scenario k_min k_max constraints : Constraint.scenario_constraint = { k_min; k_max; constraints = List.map (fun c -> (c, get_vars_of_constraint c)) constraints }

  let check_constraints constraints read_inputs_vars =

    let collect_vars c =
      match c with
      | Eq (v1, v2, _) -> NameSet.of_list [v1; v2]
      | Leq (v, lst, _) | Geq (v, lst, _) -> let lst_vars = List.map fst lst in NameSet.of_list (v :: lst_vars)
    in

    let allowed_vars = NameSet.add "k" read_inputs_vars in
    let vars_in_constraints =
      List.fold_left (fun acc c ->
        NameSet.union acc (collect_vars c)
      ) NameSet.empty constraints
    in
    let illegal_vars =
      NameSet.filter (fun v -> not (NameSet.mem v allowed_vars)) vars_in_constraints
    in
    if not (NameSet.is_empty illegal_vars) then
      let vars_str = String.concat ", " (NameSet.elements illegal_vars) in
      failwith (Printf.sprintf "Forbidden variables in constraints: %s. Only inputs variables given to read_inputs instructions and the iterator k should be used." vars_str)
    else
      ()

end


(***********)
(* Functor *)
(***********)


module type Scenario_sig = sig
  type t
  type scenario_constraint = Constraint.scenario_constraint
  val empty : t
  val add : Ast.Var.t -> int * (float * float) -> t -> t
  val get : t -> Ast.Var.t -> int -> (float * float) option
  val create_rand_expr : float -> float -> Ast.base_type -> Ast.expr
  val print : t -> unit
  val max_iter : int
  val input_vars : NameSet.t
  val scenario : t
  val scenario_constraints : scenario_constraint list
  val scenario_constraints_states : scenario_constraint list
end

module Make (Init : sig
  val max_iter : int
  val input_vars : NameSet.t
  val scenario : Scenario.t
  val scenario_constraints : Constraint.scenario_constraint list
  val scenario_constraints_states : Constraint.scenario_constraint list
end) : (Scenario_sig with type t = Scenario.t) = struct

  module VarMap = VarMap
  type t = Scenario.t
  type scenario_constraint = Constraint.scenario_constraint

  let empty = Scenario.empty
  let add = Scenario.add
  let get = Scenario.get
  let create_rand_expr = Scenario.create_rand_expr
  let print = Scenario.print

  let max_iter = Init.max_iter
  let input_vars = Init.input_vars
  let scenario = Init.scenario

  let scenario_constraints = Init.scenario_constraints
  let scenario_constraints_states = Init.scenario_constraints_states

end