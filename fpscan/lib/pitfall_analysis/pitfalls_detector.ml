open Subset_ast_stm
open Machine_state
open Pitfall_report

module type PitfallDetector = sig
  val detect_pitfall :
    ?sanity_check:bool -> stm -> MachineStateSet.t -> pitfall_report list * bool
end

module MakePitfallDetector
    (Gen : Constraint_generator.ConstraintGenerator)
    (Solver : functor
      ()
      ->
      Constraint_solver.ConstraintSolver
        with type constraint_t = Gen.constraint_t) : PitfallDetector = struct
  open Subset_ast_stm
  open Subset_ast_expr
  open Pitfall_report
  module PitfallModel = Gen.PitfallModel

  type constraint_t = Gen.constraint_t

  module T = Domainslib.Task

  let detect_pitfall ?(sanity_check : bool = false) (stm : stm)
      (m : MachineStateSet.t) : pitfall_report list * bool =
    (* get all and bound variables *)
    let all_variables, bound_variables = extract_variables stm in
    (* deduce free variables *)
    let free_variables = StringSet.diff all_variables bound_variables in
    let mk_free_var_cstr = Gen.free_var_cstr free_variables in

    let pool =
      T.setup_pool ~num_domains:(Domain.recommended_domain_count ()) ()
    in

    let rec detect_pitfall (stm : stm) (ctx_cstr : constraint_t) :
        constraint_t * pitfall_report list T.promise * bool T.promise =
      match stm with
      | Seq (_, s1, s2) ->
          let cstr1, r1, sanity_1 = detect_pitfall s1 ctx_cstr in
          let cstr2, r2, sanity_2 =
            detect_pitfall s2 (Gen.mk_and [ cstr1; ctx_cstr ])
          in

          let report_promise =
            T.async pool (fun _ ->
                List.concat [ T.await pool r1; T.await pool r2 ])
          in
          let sanity_promise =
            T.async pool (fun _ ->
                T.await pool sanity_1 && T.await pool sanity_2)
          in
          (Gen.mk_and [ cstr1; cstr2; ctx_cstr ], report_promise, sanity_promise)
      | Asn (l, z, e) -> (
          let machine_state = MachineStateSet.get_location_state l m in
          match machine_state with
          | None ->
              (* state is not reachable *)
              ( Gen.mk_false (),
                T.async pool (fun _ -> []),
                T.async pool (fun _ -> true) )
          | Some machine_state ->
              let pitfall_cstr_list =
                match e with
                | Binop (bop, x, y) -> (
                    match bop with
                    | Plus | Minus ->
                        [
                          ( Absorption (l, x, y),
                            PitfallModel.absoprtion_to_constraint z x y );
                          ( Absorption (l, y, x),
                            PitfallModel.absoprtion_to_constraint z y x );
                          ( Cancellation l,
                            PitfallModel.cancellation_to_constraint z x y );
                        ]
                    | _ -> [])
                | _ -> []
              in
              let local_cstr = Gen.asn_to_constraint z e machine_state in
              let analyse_ctx =
                Gen.mk_and
                  [ ctx_cstr; mk_free_var_cstr machine_state; local_cstr ]
              in
              let pitfall_list =
                List.fold_left
                  (fun acc (pitfall, pitfall_cstr) ->
                    let pitfall =
                      T.async pool (fun _ ->
                          let module Solver = Solver () in
                          if
                            Solver.solve_sat
                              (Gen.mk_and [ pitfall_cstr; analyse_ctx ])
                          then pitfall
                          else None l)
                    in
                    pitfall :: acc)
                  [] pitfall_cstr_list
              in
              (* Sanity check *)
              let sanity_check_promise =
                T.async pool (fun _ ->
                    if sanity_check then
                      let module Solver = Solver () in
                      Solver.solve_sat analyse_ctx
                    else true)
              in
              let report_promise =
                T.async pool (fun _ ->
                    [
                      {
                        location = l;
                        stm;
                        pitfall_list =
                          List.map (fun p -> T.await pool p) pitfall_list;
                      };
                    ])
              in
              (local_cstr, report_promise, sanity_check_promise))
      | Ite (_, cond, s_then, s_else) ->
          let cstr_then, r_then, sanity_then = detect_pitfall s_then ctx_cstr in
          let cstr_else, r_else, sanity_else = detect_pitfall s_else ctx_cstr in
          let report_promise =
            T.async pool (fun _ ->
                List.concat [ T.await pool r_then; T.await pool r_else ])
          in
          let sanity_promise =
            T.async pool (fun _ ->
                T.await pool sanity_then && T.await pool sanity_else)
          in
          ( Gen.mk_branch cond cstr_then cstr_else,
            report_promise,
            sanity_promise )
      | Nop _ ->
          let report_promise = T.async pool (fun _ -> []) in
          let sanity_promise = T.async pool (fun _ -> true) in
          (Gen.mk_true (), report_promise, sanity_promise)
    in
    let pitfall_report_list, sanity =
      T.run pool (fun _ ->
          let _, pitfall_report_list_promise, sanity_check_promise =
            detect_pitfall stm (Gen.mk_true ())
          in
          ( T.await pool pitfall_report_list_promise,
            T.await pool sanity_check_promise ))
    in
    (pitfall_report_list, sanity)
end
