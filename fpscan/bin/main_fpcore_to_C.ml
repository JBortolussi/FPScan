open Tiny
open Load_domains
module M = Map.Make (String)

let rec rename_var (rename : string -> string) (stm : Tiny.Subset_ast_stm.stm) :
    Tiny.Subset_ast_stm.stm =
  (* let rm_star = String.map (fun c -> if Char.equal c '*' then 's' else c) in *)
  let rename_var = rename_var rename in
  let rec rename_var_expr (expr : Tiny.Subset_ast_expr.expr) :
      Tiny.Subset_ast_expr.expr =
    let open Tiny.Subset_ast_expr in
    match expr with
    | Cst (_, _) -> expr
    | Binop (bop, e1, e2) -> Binop (bop, rename e1, rename e2)
    | Unop (uop, e) -> Unop (uop, rename e)
    | Var s -> Var (rename s)
    | Rand (_, _) -> expr
    | Call (f, args) -> Call (f, List.map rename args)
  in
  let rename_var_bool_expr (bexpr : Tiny.Subset_ast_expr.bool_expr) :
      Tiny.Subset_ast_expr.bool_expr =
    match bexpr with BCond (s, cmp) -> BCond (rename s, cmp)
  in
  match stm with
  | Seq (l, s1, s2) -> Seq (l, rename_var s1, rename_var s2)
  | Asn (l, s, e) -> Asn (l, rename s, rename_var_expr e)
  | Ite (l, b, s1, s2) ->
      Ite (l, rename_var_bool_expr b, rename_var s1, rename_var s2)

let _ =
  let input = ref "" in
  let output = ref "" in

  let speclist =
    Arg.align
      [
        ("-f", Arg.Set_string input, "Input fpcore file");
        ("-o", Arg.Set_string output, "Output C file");
      ]
  in

  Arg.parse speclist (fun _ -> ()) "NO";
  let vars, tiny_ast = Parse_fpcore.parse_fpcore_prgm !input in
  let ast, tiny_ast = Parse_fpcore.prepare_program tiny_ast in
  let ast =
    rename_var (String.map (fun c -> if Char.equal c '*' then 's' else c)) ast
  in
  let tiny_ast = Tiny.Subset_ast_stm.to_tiny_stm ast in
  Format.fprintf Format.std_formatter "%a@." Ast.fprint_stm tiny_ast;
  print_endline "Program parsed";

  let domain_name = Printf.sprintf "interval_float_f%d" 32 in
  let dom = prepare_domains (List.map get_domain [ domain_name ]) in
  let results = Analyze.analyze dom (-1) (-1) (-1) true 1 0 vars tiny_ast in
  let module Results = (val results : Analyze.Results) in
  let m = Results.results in

  let module PrintResults = PrintResults.Make (Results.Res) in
  let machine_state =
    Machine_state.MachineStateSet.get_machine_state tiny_ast results
  in

  let all_vars, bound_vars = Subset_ast_stm.extract_variables ast in
  let free_vars = Subset_ast_expr.StringSet.diff all_vars bound_vars in
  let rand_vars = Subset_ast_stm.bound_to_rand_var ast in
  let input_vars = Subset_ast_expr.StringSet.union free_vars rand_vars in

  let out = Stdlib.open_out !output in
  Printf.printf "%s" !output;
  let ff = Format.formatter_of_out_channel out in
  (* let ff = Format.std_formatter in *)
  Format.fprintf ff "%a" Subset_ast_stm.stm_to_C_prgm ast;

  let _, m =
    Machine_state.MachineStateSet.fold
      (fun l m acc ->
        match m with
        | None -> acc
        | Some m ->
            let remain_v, _ = acc in
            Subset_ast_expr.StringSet.fold
              (fun v acc ->
                match Machine_state.MachineState.find_opt v m with
                | None -> acc
                | Some b ->
                    let l, u = Bounds.get b in
                    let remain_v, acc = acc in
                    let remain_v =
                      Subset_ast_expr.StringSet.remove v remain_v
                    in
                    let acc =
                      M.add v (Scalar.to_float l, Scalar.to_float u) acc
                    in
                    (remain_v, acc))
              remain_v acc)
      machine_state (input_vars, M.empty)
  in

  let input_vars =
    input_vars |> Subset_ast_expr.StringSet.to_list |> List.sort String.compare
  in

  Format.fprintf ff "@.";

  List.iter
    (fun v ->
      let l, u = M.find v m in
      Format.fprintf ff "//%s: [%s;%s]@." v (string_of_float l)
        (string_of_float u))
    input_vars;

  (* M.iter (
    fun v (l, u) -> 
      Format.fprintf ff "//%s: [%s;%s]@." v (string_of_float l) (string_of_float u)
  ) m; *)

  (* let m = Machine_state.MachineStateSet.get_location_state l machine_state in

  (match m with
  | None -> failwith "No m"
  | Some m -> (
    Machine_state.MachineState.fprint ff m;
    Subset_ast_expr.StringSet.iter (
      fun v -> (
        match Machine_state.MachineState.find_opt v m with
        | None -> failwith "No v"
        | Some b -> (
          let l, u = Bounds.get b in
          Format.fprintf ff "//%s:[%s;%s]@." 
            v 
            (l |> Scalar.to_float |> string_of_float)
            (u |> Scalar.to_float |> string_of_float)
        )
      )
    ) input_vars
  )); *)
  Stdlib.close_out out;

  ()
