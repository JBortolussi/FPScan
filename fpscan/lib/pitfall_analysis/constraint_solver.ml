module type ConstraintSolver = sig
  type constraint_t

  val solve_sat : constraint_t -> bool
end
