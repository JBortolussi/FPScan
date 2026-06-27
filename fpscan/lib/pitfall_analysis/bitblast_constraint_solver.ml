open Bitbalst_constraint

module type Z3Constraint = sig
  open Z3
  open Z3.Expr
  open Z3.Sort

  val ctx : context
  val pow_2_p : float
  val round : expr
  val fp_sort : sort
  val mk_var_fp : context -> string -> expr
end

module MakeZ3Constraint (FloatFormat : Float_format.FloatFormat) :
  Z3Constraint = struct
  open Z3
  open Z3.FloatingPoint
  open Z3.Symbol
  open Z3.Boolean

  let cfg = [ ("model", "true"); ("proof", "false") ]
  let ctx = mk_context cfg
  let pow_2_p = Float.pow 2.0 (float FloatFormat.precision)
  let round = RoundingMode.mk_round_nearest_ties_to_even ctx

  let fp_sort =
    FloatingPoint.mk_sort ctx FloatFormat.e_bit_number FloatFormat.precision

  (* Create a new var *)
  let mk_var_fp ctx s_name = Expr.mk_const ctx (mk_string ctx s_name) fp_sort
end

module MakeBitblastConstraintSolver
    (FloatFormat : Float_format.FloatFormat)
    () :
  Constraint_solver.ConstraintSolver
    with type constraint_t = bitblast_constraint_t = struct
  type constraint_t = bitblast_constraint_t

  module Z3Constraint = MakeZ3Constraint (FloatFormat)
  open Z3
  open Z3.Boolean
  open Z3.FloatingPoint
  open Z3.Arithmetic
  include Z3Constraint

  let cancellation_to_constraint z x y =
    mk_eq ctx (mk_var_fp ctx z) (FloatingPoint.mk_numeral_i ctx 0 fp_sort)

  let absoprtion_to_constraint z x _ =
    mk_and ctx
      [
        (* |z| >  2^p |x| *)
        mk_geq ctx
          (mk_abs ctx (mk_var_fp ctx z))
          (FloatingPoint.mk_mul ctx round
             (mk_numeral_f ctx pow_2_p fp_sort)
             (mk_abs ctx (mk_var_fp ctx x)));
        (* x != 0 *)
        mk_not ctx (mk_eq ctx (mk_var_fp ctx x) (mk_numeral_f ctx 0.0 fp_sort));
      ]

  let range_cstr v l u =
    mk_and ctx
      [
        mk_leq ctx (mk_var_fp ctx v) (mk_numeral_f ctx (Q.to_float u) fp_sort);
        mk_geq ctx (mk_var_fp ctx v) (mk_numeral_f ctx (Q.to_float l) fp_sort);
      ]

  let expr_to_z3 (expr : Subset_ast_expr.expr) : Expr.expr option =
    match expr with
    | Cst (f, _) -> Some (mk_numeral_f ctx (Q.to_float f) fp_sort)
    | Var v -> Some (mk_var_fp ctx v)
    | Rand (_, _) -> None
    | Unop (unop, v) ->
        let op_fp =
          match unop with
          | Minus ->
              fun x ->
                FloatingPoint.mk_sub ctx round (mk_zero ctx fp_sort false) x
        in
        Some (op_fp (mk_var_fp ctx v))
    | Binop (bop, lhs, rhs) ->
        let op_fp =
          match bop with
          | Plus -> FloatingPoint.mk_add ctx round
          | Minus -> FloatingPoint.mk_sub ctx round
          | Times -> FloatingPoint.mk_mul ctx round
          | Div -> FloatingPoint.mk_div ctx round
        in
        Some (op_fp (mk_var_fp ctx lhs) (mk_var_fp ctx rhs))
    | Call (f, args) -> (
        match f with
        | "sqrt" ->
            let x =
              match args with
              | x :: [] -> x
              | _ -> failwith "sqrt requires exactly one argument"
            in
            Some (FloatingPoint.mk_sqrt ctx round (mk_var_fp ctx x))
        | _ ->
            failwith
              (Printf.sprintf "Impossible to use function '%s' with bitblasting"
                 f))

  let bool_cond_to_z3 (var : string) (cmp : Subset_ast_expr.cmp) : Expr.expr =
    let cmp =
      match cmp with
      | Strict -> FloatingPoint.mk_gt
      | Loose -> FloatingPoint.mk_geq
      | Zero -> FloatingPoint.mk_eq
      | NonZero ->
          fun ctx e1 e2 -> Z3.Boolean.mk_not ctx (FloatingPoint.mk_eq ctx e1 e2)
    in
    cmp ctx (mk_var_fp ctx var) (FloatingPoint.mk_numeral_i ctx 0 fp_sort)

  let rec constraint_to_z3 (cstr : constraint_t) =
    match cstr with
    | True -> mk_true ctx
    | False -> mk_false ctx
    | And l -> mk_and ctx (List.map constraint_to_z3 l)
    | Or l -> mk_or ctx (List.map constraint_to_z3 l)
    | Cancellation (z, x, y) -> cancellation_to_constraint z x y
    | Absorption (z, x, y) -> absoprtion_to_constraint z x y
    | Bound (z, l, u) -> range_cstr z l u
    | Asn (z, e) -> (
        match expr_to_z3 e with
        | None -> mk_true ctx
        | Some e -> mk_eq ctx (mk_var_fp ctx z) e)
    | BoolCond (var, cmp) -> bool_cond_to_z3 var cmp
    | NotBoolCond (var, cmp) -> Z3.Boolean.mk_not ctx (bool_cond_to_z3 var cmp)

  let solve_sat (cstr : constraint_t) : bool =
    let z3_cstr = constraint_to_z3 cstr in
    let solver = Solver.mk_simple_solver ctx in
    Solver.add solver [ z3_cstr ];
    match Solver.check solver [] with
    | UNSATISFIABLE -> false
    | SATISFIABLE -> true
    | UNKNOWN -> true
end
