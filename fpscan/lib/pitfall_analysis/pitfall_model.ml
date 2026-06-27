module type PitfallModel = sig
  type constraint_t

  val cancellation_to_constraint : string -> string -> string -> constraint_t
  val absoprtion_to_constraint : string -> string -> string -> constraint_t
end

module PitfallModelStandard :
  PitfallModel with type constraint_t = Constraint.constraints = struct
  open Constraint

  type constraint_t = constraints

  let cancellation_to_constraint z x y =
    And
      [
        LOp (Eq, Ufp x, Ufp y);
        Or
          [
            LOp (Lt, Ufp z, BinOp (Minus, Ufp x, Nsb x));
            LOp (Lt, Ufp y, BinOp (Minus, Ufp y, Nsb y));
          ];
      ]

  let absoprtion_to_constraint _ x y = LOp (Leq, Ufp x, Ulp y)
end

module PitfallModelZero :
  PitfallModel with type constraint_t = Constraint.constraints = struct
  open Constraint

  type constraint_t = constraints

  let cancellation_to_constraint z x y =
    let cstr = PitfallModelStandard.cancellation_to_constraint z x y in
    Or
      [
        cstr;
        (* A cancellation occur if the result is 0 while the two operand are not 0 *)
        And [ ZeroReachable z; Not (And [ ZeroReachable x; ZeroReachable y ]) ];
      ]

  let absoprtion_to_constraint = PitfallModelStandard.absoprtion_to_constraint
end
