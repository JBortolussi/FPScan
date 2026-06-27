open Subset_ast_expr

type bitblast_constraint_t =
  | Asn of string * expr
  | BoolCond of string * cmp
  | NotBoolCond of string * cmp
  | Bound of string * Q.t * Q.t
  (* | Ite of bitblast_constraint_t * bitblast_constraint_t *)
  | Cancellation of string * string * string
  | Absorption of string * string * string
  | True
  | False
  | And of bitblast_constraint_t list
  | Or of bitblast_constraint_t list

module BitblastPitfallModel :
  Pitfall_model.PitfallModel with type constraint_t = bitblast_constraint_t =
struct
  type constraint_t = bitblast_constraint_t

  let cancellation_to_constraint z x y = Cancellation (z, x, y)
  let absoprtion_to_constraint z x y = Absorption (z, x, y)
end

module BitblastConstraintGenerator :
  Constraint_generator.ConstraintGenerator
    with type constraint_t = bitblast_constraint_t = struct
  type constraint_t = bitblast_constraint_t

  module PitfallModel = BitblastPitfallModel

  let mk_true () = True
  let mk_false () = False
  let mk_and l = And l
  let mk_or l = Or l

  let mk_branch (cond : bool_expr) (c_then : constraint_t)
      (c_else : constraint_t) =
    let var, cmp = match cond with BCond (var, cmp) -> (var, cmp) in
    mk_or
      [
        mk_and [ BoolCond (var, cmp); c_then ];
        mk_and [ NotBoolCond (var, cmp); c_else ];
      ]

  let range_cstr m z =
    match Machine_state.MachineState.find_opt z m with
    | None -> True
    | Some b ->
        let l, u = Bounds.get b in
        (* only manage Rational *)
        let l, u =
          match (l, u) with
          | Rat ql, Rat qu -> (ql, qu)
          | _, _ -> failwith "Unsuported value in machine state"
        in
        Bound (z, l, u)

  let asn_to_constraint ?p_cstr:(_ = true) z e m =
    mk_and [ Asn (z, e); range_cstr m z ]

  let free_var_cstr vars m =
    mk_and
      (Constraint_generator.StringSet.fold
         (fun v acc -> range_cstr m v :: acc)
         vars [])
end
