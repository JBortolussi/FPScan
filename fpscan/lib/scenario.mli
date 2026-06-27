module VarMap : Map.S with type key = Ast.Var.t
module NameSet : Set.S with type elt = string


(**************************************************)
(*  CONSTRAINTS on inputs depending on iteration  *)
(**************************************************)

module Constraint : sig

  (* k_min : minimum iteration at which the constraints must be applied
  k_min : maximum iteration at which the constraints must be applied *)

  type scenario_constraint = {
    k_min : int;
    k_max : int;
    constraints : (Ast.expr * NameSet.t) list; (* List of constraints and set of vars used in the constraints *)
  }
end

module BuildConstraint : sig
  (* Type that makes it easier to create constraints by hand *)
  type t =
    | Eq of string * string * float
    | Leq of string * (string * float) list * float
    | Geq of string * (string * float) list * float

  (* Traduction of a constraint of type t to Ast.expr *)
  val t_to_astexpr : t -> Ast.expr

  (* To collect the variables in a constraint *)
  val get_vars_of_constraint : Ast.expr -> NameSet.t

  (* Creates a Constraint.scenario_constraint from the interval of iterations (values of k_min and k_max) and a list of constraints as Ast.expr *)
  val create_scenario : int -> int -> Ast.expr list -> Constraint.scenario_constraint

  (* Check if only inputs variables given to read_inputs instruction and the iterator k are used in the constraints *)
  val check_constraints : t list -> NameSet.t -> unit

end

(*****)

(****************************************************************************************************************************)
(*  SCENARIO specifying intervals of values at each iteration for the inputs variables (given to read_inputs instructions)  *)
(****************************************************************************************************************************)

module Scenario : sig
  type t

  val empty : t

  (* Add an interval of values for a specified variable and iteration to the scenario *)
  val add : Ast.Var.t -> int * (float * float) -> t -> t

  (* Get the inteval of values corresponding to the given variable and iteration from the scenario *)
  val get : t -> Ast.Var.t -> int -> (float * float) option

  (* Create a rand_expr (random expression) from two floats (corresponding to an interval) *)
  val create_rand_expr : float -> float -> Ast.base_type -> Ast.expr

  (* print a scenario *)
  val print : t -> unit
end

module BuildScenario : sig

  val load_scenario_from_file : string -> (int * (float * float)) list

  val update_scenario_file : Scenario.t -> Ast.Var.t -> Scenario.t

  (* Create the full scenario (for the whole programm) using update_scenario and check if a file specifying a scenario for a variable is missing.
  Gives the full scenario, a set of declared variables and the set of variables given to read_inputs instructions *)
  val construct_scenario : Ast.stm -> Ast.Var.Set.t -> Scenario.t -> NameSet.t -> NameSet.t -> Scenario.t * NameSet.t * NameSet.t
end

(*****)

module type Scenario_sig = sig
  include module type of Scenario
  type scenario_constraint = Constraint.scenario_constraint
  val max_iter : int (* maximum iteration : the simulation stops as soon as the max_iter has been reached *)
  val input_vars : NameSet.t (* inputs variables *)
  val scenario : t
  val scenario_constraints : scenario_constraint list
  val scenario_constraints_states : scenario_constraint list
end

module Make : functor (Init : sig
  val max_iter : int
  val input_vars : NameSet.t 
  val scenario : Scenario.t
  val scenario_constraints : Constraint.scenario_constraint list
  val scenario_constraints_states : Constraint.scenario_constraint list
end) -> Scenario_sig with type t = Scenario.t
