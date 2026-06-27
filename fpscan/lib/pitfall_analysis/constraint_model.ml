module type ConstraintModel = sig
  type constraint_t

  val op_to_constraint :
    Subset_ast_expr.bop -> string -> string -> string -> constraint_t

  val call_to_constraint : string -> string -> string list -> constraint_t
end

module MakeConstraintModelBase (FloatFormat : Float_format.FloatFormat) :
  ConstraintModel with type constraint_t = Constraint.constraints = struct
  open Constraint

  type constraint_t = constraints

  let op_to_constraint (op : Subset_ast_expr.bop) z x y =
    match op with
    | Plus | Minus ->
        And
          [
            (* NSB *)
            LOp
              ( Geq,
                Nsb z,
                BinOp
                  ( Minus,
                    Ufp z,
                    Max
                      ( BinOp
                          ( Plus,
                            Max
                              ( BinOp (Minus, Ufp x, Nsb x),
                                BinOp (Minus, Ufp y, Nsb y) ),
                            Cst 2 ),
                        Ulp z ) ) );
            (* UFP *)
            LOp (Leq, Ufp z, BinOp (Plus, Max (Ufp x, Ufp y), Cst 1));
          ]
    | Times ->
        LOp
          ( Geq,
            Nsb z,
            BinOp
              ( Minus,
                Ufp z,
                (* Main max *)
                Max
                  ( Max
                      ( (* Line 1 *)
                        BinOp
                          ( Plus,
                            Ufp x,
                            BinOp
                              ( Plus,
                                Ufp y,
                                BinOp (Minus, Cst 3, Min (Nsb x, Nsb y)) ) ),
                        (* Line 2 *)
                        BinOp
                          ( Minus,
                            BinOp (Plus, Ufp x, BinOp (Plus, Ufp y, Cst 3)),
                            BinOp (Plus, Nsb x, Nsb y) ) ),
                    (* Line 3 *)
                    Ulp z ) ) )
    | Div ->
        (* not cond or (cond and cstr) *)
        Or
          [
            (* not cond *)
            LOp (Leq, Nsb y, Cst 1);
            (* cond and cstr *)
            And
              [
                (* cond *)
                LOp (Gt, Nsb y, Cst 1);
                (* cstr *)
                LOp
                  ( Geq,
                    Nsb z,
                    BinOp
                      ( Minus,
                        Ufp z,
                        (* max *)
                        Max
                          ( (* line 1 *)
                            BinOp
                              ( Minus,
                                BinOp (Plus, Ufp x, Cst 3),
                                BinOp (Plus, Ufp y, Min (Nsb x, Nsb y)) ),
                            (* line 2 *)
                            BinOp (Minus, Ulp z, Cst 1) ) ) );
              ];
          ]

  let call_to_constraint z f args =
    match f with
    | "cos" | "sin" | "atan" ->
        let x =
          match args with
          | v :: [] -> v
          | _ -> failwith "Trigo function require exactly one argument"
        in
        LOp
          ( Geq,
            Nsb z,
            Min (BinOp (Minus, Nsb x, Cst 1), Cst FloatFormat.precision) )
    | "sqrt" ->
        let x =
          match args with
          | v :: [] -> v
          | _ -> failwith "sqrt requires exactly one argument"
        in
        LOp
          ( Geq,
            Nsb z,
            BinOp
              ( Minus,
                Ufp z,
                Max
                  ( (* [(ufp(x) - nsb(x) + 1)/2] + 1 *)
                    BinOp
                      ( Plus,
                        Div (BinOp (Minus, BinOp (Plus, Ufp x, Cst 1), Nsb x), 2),
                        Cst 1 ),
                    (* ulp(z) - 1 *)
                    BinOp (Minus, Ulp z, Cst 1) ) ) )
    | _ -> failwith ("unsuported function: " ^ f)
end

module MakeConstraintModelZero (FloatFormat : Float_format.FloatFormat) :
  ConstraintModel with type constraint_t = Constraint.constraints = struct
  module BaseModel = MakeConstraintModelBase (FloatFormat)

  type constraint_t = BaseModel.constraint_t

  let op_to_constraint (op : Subset_ast_expr.bop) z x y =
    let open Constraint in
    let cstr = BaseModel.op_to_constraint op z x y in
    match op with
    | Plus | Minus ->
        And
          [
            cstr;
            (* Zero is reachable if *)
            (* Zero is in bound *)
            (* and *)
            (* Zero is reachable for both operand *)
            (* or *)
            (* The ufp of the operand are equal *)
            Eq
              ( ZeroReachable z,
                And
                  [
                    ZeroInBound z;
                    Or
                      [
                        LOp (Eq, Ufp x, Ufp y);
                        And [ ZeroReachable x; ZeroReachable y ];
                      ];
                  ] );
            LOp (Geq, Ufp z, Ulp x);
            LOp (Geq, Ufp z, Ulp y);
          ]
    | Times ->
        And
          [
            cstr;
            (* Zero is reachable if *)
            (* Zero is in bound *)
            (* and *)
            (* One of the operand is zero *)
            Eq
              ( ZeroReachable z,
                And [ ZeroInBound z; Or [ ZeroReachable x; ZeroReachable y ] ]
              );
            LOp (Geq, Ufp z, BinOp (Plus, Ufp x, Ufp y));
          ]
    | Div ->
        And
          [ cstr; Eq (ZeroReachable z, And [ ZeroInBound z; ZeroReachable x ]) ]

  let call_to_constraint = BaseModel.call_to_constraint
end
