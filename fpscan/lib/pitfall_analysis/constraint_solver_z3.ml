open Constraint_solver
open Constraint

module Z3ConstraintSolver () :
  ConstraintSolver with type constraint_t = constraints = struct
  open Z3
  open Z3.Arithmetic
  open Z3.Symbol
  open Boolean
  open FuncDecl

  type constraint_t = constraints

  (* Declare the proof context *)
  let cfg = [ ("model", "true"); ("proof", "false") ]
  let ctx = mk_context cfg

  (* Declare "float" type *)
  let int_t = Integer.mk_sort ctx
  let real_t = Arithmetic.Real.mk_sort ctx
  let bool_t = Boolean.mk_sort ctx

  let val_t =
    Datatype.mk_sort_s ctx "Val"
      [
        Datatype.mk_constructor_s ctx "val" (mk_string ctx "symbol")
          [
            mk_string ctx "ufp";
            mk_string ctx "nsb";
            mk_string ctx "p";
            mk_string ctx "zero_in_bound";
            mk_string ctx "zero_reachable";
          ]
          [ Some int_t; Some int_t; Some int_t; Some bool_t; Some bool_t ]
          [ 0; 0; 0; 0; 0 ];
      ]

  let acc_lst = List.hd (Datatype.get_accessors val_t)
  let ufp = List.nth acc_lst 0
  let nsb = List.nth acc_lst 1
  let p = List.nth acc_lst 2

  (* True if zero is in the computed bound *)
  let zero_in_bound = List.nth acc_lst 3

  (* True if zero is reachable *)
  let zero_reachable = List.nth acc_lst 4

  (* Create a new var *)
  let mk_var ctx s_name = Expr.mk_const ctx (mk_string ctx s_name) val_t

  (* define max *)
  let mk_max ctx e1 e2 = Boolean.mk_ite ctx (mk_le ctx e1 e2) e2 e1

  (* define min *)
  let mk_min ctx e1 e2 = Boolean.mk_ite ctx (mk_le ctx e1 e2) e1 e2

  let rec constraint_val_to_z3 ctx count = function
    | Var v -> (mk_var ctx v, [])
    | Cst i -> (Integer.mk_numeral_i ctx i, [])
    | Ufp x -> (apply ufp [ mk_var ctx x ], [])
    | Ulp x ->
        let x = mk_var ctx x in
        ( mk_add ctx
            [
              Integer.mk_numeral_i ctx 1;
              apply ufp [ x ];
              mk_unary_minus ctx (apply p [ x ]);
            ],
          [] )
    | Nsb x -> (apply nsb [ mk_var ctx x ], [])
    | P x -> (apply p [ mk_var ctx x ], [])
    | Max (x, y) ->
        let x, c1 = constraint_val_to_z3 ctx count x in
        let y, c2 = constraint_val_to_z3 ctx count y in
        (mk_max ctx x y, List.concat [ c1; c2 ])
    | Min (x, y) ->
        let x, c1 = constraint_val_to_z3 ctx count x in
        let y, c2 = constraint_val_to_z3 ctx count y in
        (mk_min ctx x y, List.concat [ c1; c2 ])
    | BinOp (bop, x, y) ->
        let x, c1 = constraint_val_to_z3 ctx count x in
        let y, c2 = constraint_val_to_z3 ctx count y in
        let e =
          match bop with
          | Plus -> mk_add ctx [ x; y ]
          | Minus -> mk_sub ctx [ x; y ]
          | Times -> mk_mul ctx [ x; y ]
        in
        (e, List.concat [ c1; c2 ])
    | Div (x, i) ->
        let x, c = constraint_val_to_z3 ctx count x in
        let d = Arithmetic.mk_div ctx x (Arithmetic.Real.mk_numeral_i ctx i) in
        count := !count + 1;
        let fl =
          Arithmetic.Integer.mk_const_s ctx (Printf.sprintf "floor_%d" !count)
        in
        let fl_cstr =
          [
            mk_le ctx (Arithmetic.Integer.mk_int2real ctx fl) d;
            mk_le ctx d (Arithmetic.Integer.mk_int2real ctx fl);
          ]
        in
        (fl, List.concat [ fl_cstr; c ])

  let rec constraint_to_z3 ctx count = function
    | True -> mk_true ctx
    | False -> mk_false ctx
    | And cstr_list ->
        let z3_cstr_list =
          List.fold_left
            (fun acc cstr -> constraint_to_z3 ctx count cstr :: acc)
            [] cstr_list
        in
        mk_and ctx z3_cstr_list
    | Or cstr_list ->
        let z3_cstr_list =
          List.fold_left
            (fun acc cstr -> constraint_to_z3 ctx count cstr :: acc)
            [] cstr_list
        in
        mk_or ctx z3_cstr_list
    | Not cstr -> mk_not ctx (constraint_to_z3 ctx count cstr)
    | Eq (c1, c2) ->
        mk_eq ctx
          (constraint_to_z3 ctx count c1)
          (constraint_to_z3 ctx count c2)
    | LOp (lop, lhs, rhs) ->
        let (lhs, c1), (rhs, c2) =
          ( constraint_val_to_z3 ctx count lhs,
            constraint_val_to_z3 ctx count rhs )
        in
        let c =
          (match lop with
          | Geq -> mk_ge
          | Gt -> mk_gt
          | Leq -> mk_le
          | Lt -> mk_lt
          | Eq -> mk_eq)
            ctx lhs rhs
        in
        mk_and ctx (List.concat [ c1; c2; [ c ] ])
    | ZeroInBound v -> apply zero_in_bound [ mk_var ctx v ]
    | ZeroReachable v -> apply zero_reachable [ mk_var ctx v ]

  let solve_sat (cstr : constraints) : bool =
    let solver = Solver.mk_simple_solver ctx in
    let count = ref 0 in
    Solver.add solver [ constraint_to_z3 ctx count cstr ];
    match Solver.check solver [] with
    | UNSATISFIABLE -> false
    | SATISFIABLE -> true
    | UNKNOWN -> true
end
