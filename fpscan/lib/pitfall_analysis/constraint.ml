module StringSet = Set.Make (String)

type gbop = Plus | Minus | Times

type constraint_val =
  | Var of string
  | Cst of int
  | Ulp of string
  | Ufp of string
  | Nsb of string
  | P of string
  | Max of constraint_val * constraint_val
  | Min of constraint_val * constraint_val
  | BinOp of gbop * constraint_val * constraint_val
  | Div of constraint_val * int

type lop = Geq | Gt | Leq | Lt | Eq

type constraints =
  | True
  | False
  (* Logical operators *)
  | And of constraints list
  | Or of constraints list
  | Not of constraints
  | Eq of constraints * constraints
  (* Arithmetic operators *)
  | LOp of lop * constraint_val * constraint_val
  (* is zero in the computed bounded ? *)
  | ZeroInBound of string
  (* is zero reachable ? *)
  | ZeroReachable of string

let rec get_vars_from_val (cstr_val : constraint_val) : StringSet.t =
  match cstr_val with
  | Var v | Ulp v | Ufp v | Nsb v | P v -> StringSet.singleton v
  | Max (x, y) | Min (x, y) ->
      StringSet.union (get_vars_from_val x) (get_vars_from_val y)
  | BinOp (_, x, y) ->
      StringSet.union (get_vars_from_val x) (get_vars_from_val y)
  | Cst _ -> StringSet.empty
  | Div (x, _) -> get_vars_from_val x

let rec get_vars (cstr : constraints) : StringSet.t =
  match cstr with
  | And c_list | Or c_list ->
      List.fold_left
        (fun acc c -> StringSet.union acc (get_vars c))
        StringSet.empty c_list
  | Not c -> get_vars c
  | Eq (x, y) -> StringSet.union (get_vars x) (get_vars y)
  | LOp (_, x, y) -> StringSet.union (get_vars_from_val x) (get_vars_from_val y)
  | True | False | ZeroInBound _ | ZeroReachable _ -> StringSet.empty
