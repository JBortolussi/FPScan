open Constraint
open Subset_ast_expr
module StringSet = Set.Make (String)

module type ConstraintGenerator = sig
  type constraint_t

  module PitfallModel :
    Pitfall_model.PitfallModel with type constraint_t = constraint_t

  val asn_to_constraint :
    ?p_cstr:bool ->
    string ->
    Subset_ast_expr.expr ->
    Machine_state.MachineState.t ->
    constraint_t

  val free_var_cstr :
    StringSet.t -> Machine_state.MachineState.t -> constraint_t

  val mk_true : unit -> constraint_t
  val mk_false : unit -> constraint_t
  val mk_and : constraint_t list -> constraint_t
  val mk_or : constraint_t list -> constraint_t
  val mk_branch : bool_expr -> constraint_t -> constraint_t -> constraint_t
  val range_cstr : Machine_state.MachineState.t -> string -> constraint_t
end

module MakeConstraintGenerator
    (F : Float_format.FloatFormat)
    (C : Constraint_model.ConstraintModel with type constraint_t = constraints)
    (P : Pitfall_model.PitfallModel with type constraint_t = C.constraint_t) :
  ConstraintGenerator with type constraint_t = constraints = struct
  module FloatFormat = F
  module ConstraintModel = C
  module PitfallModel = P
  open Machine_state

  type constraint_t = constraints

  let mk_true _ = True
  let mk_false _ = False
  let mk_and (cstr_list : constraints list) : constraints = And cstr_list
  let mk_or (cstr_list : constraints list) : constraints = Or cstr_list

  let mk_branch (_ : bool_expr) (c_then : constraint_t) (c_else : constraint_t)
      =
    mk_or [ c_then; c_else ]

  let p_cstr (z : string) = LOp (Eq, P z, Cst FloatFormat.precision)

  let valid_var_cstr (z : string) : constraints =
    And
      [
        LOp (Geq, Ufp z, Cst FloatFormat.e_min);
        LOp (Eq, Ulp z, BinOp (Minus, Ufp z, BinOp (Minus, P z, Cst 1)));
        LOp (Geq, P z, Cst 0);
      ]

  let range_cstr (m : MachineState.t) (z : string) : constraints =
    match MachineState.find_opt z m with
    | None -> True
    | Some b ->
        let l, u = Bounds.get b in
        (* only manage Rational *)
        let l, u =
          match (l, u) with
          | Rat ql, Rat qu -> (ql, qu)
          | _, _ -> failwith "Unsuported value in machine state"
        in
        (*
        Problems occurs only when log2(x) is round to k whereas
        |x| < 2^k. It that case, it should be k-1.
        |x| < 2^k cannot be tested directly. Given x=a/b, check
        |a| < |b| * 2^k.
        Use big int to avoid overflow
        2^|k| * |a| < |b| * 2^{k + |k|}
        To have positive exponent
      *)
        let get_ufp q =
          let ufp =
            q |> Q.to_float |> abs_float
            |> (fun x -> Float.log x /. Float.log 2.)
            |> Float.to_int
          in
          let a, b = (q |> Q.num |> Z.abs, q |> Q.den |> Z.abs) in
          let pow_k = Z.pow (Z.of_int 2) (abs ufp) in
          let pow_k_k = Z.pow (Z.of_int 2) (abs ufp + ufp) in
          if Z.mul pow_k a < Z.mul pow_k_k b then ufp - 1 else ufp
        in

        let ufp_l, ufp_u = (get_ufp l, get_ufp u) in
        (* If 0 is in the interval *)
        if Q.sign l != Q.sign u || Q.sign l == 0 || Q.sign u == 0 then
          And [ LOp (Leq, Ufp z, Cst (max ufp_u ufp_l)); ZeroInBound z ]
        else
          And
            [
              LOp (Leq, Ufp z, Cst (max ufp_u ufp_l));
              LOp (Leq, Cst (min ufp_u ufp_l), Ufp z);
              Not (ZeroInBound z);
            ]

  let free_var_cstr (var_set : StringSet.t) (m : MachineState.t) : constraints =
    StringSet.fold (fun v cstr -> And [ range_cstr m v; cstr ]) var_set True

  let asn_to_constraint ?p_cstr:(add_p = true) (z : string)
      (expr : Subset_ast_expr.expr) (m : MachineState.t) : constraints =
    let cstr =
      match expr with
      | Cst (_, _) | Rand (_, _) ->
          (* nsb of the variable is equal to precision *)
          LOp (Geq, Nsb z, P z)
      | Var x ->
          (* The two var *)
          LOp (Eq, Var z, Var x)
      | Call (f, args) -> ConstraintModel.call_to_constraint z f args
      | Unop (uop, x) -> ( match uop with Minus -> LOp (Eq, Var z, Var x))
      | Binop (bop, x, y) -> ConstraintModel.op_to_constraint bop z x y
    in
    let range_cstr = range_cstr m z in
    let valid_var = valid_var_cstr z in
    let p_cstr = p_cstr z in
    let cstr_list = [ cstr; range_cstr; valid_var ] in
    let cstr_list = if add_p then p_cstr :: cstr_list else cstr_list in
    And cstr_list
end
