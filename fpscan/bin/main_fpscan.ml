open Tiny
open Load_domains

type str_itv = string * string
module StringMap = Map.Make(String)
module LocationMap = Map.Make (struct type t = Location.t let compare = compare end)

module StringSet = Set.Make(String)

let check (module R: Analyze.Results) (stm: Ast.stm) =
  let module Res = R.Res in
  let m = R.results in

  let get_properties l =
    match Res.find_aopt None (Location.end_p l) m with
    | None -> Some (StringMap.empty)
    | Some (a) -> (
      try
        let properties = Res.Dom.to_properties a in
        let m = List.fold_left (fun map ((v, _), b) -> (
          StringMap.add (Ast.Var.get_name v) b map
        )) StringMap.empty properties
        in
        Some (m)
      with Relational.BotEnv -> None
    )
  in

  let print_loc ff l stm vars =
    let properties = get_properties l in
    (* Any none matched var is top *)
    let top_var =  StringSet.diff vars (
      match properties with
      | None -> StringSet.empty
      | Some (raw_vars) ->
        let vars_list = List.fold_left (
          fun acc (k, v) -> k :: acc
        ) [] (StringMap.bindings raw_vars)
        in
        (* StringSet.singleton (Int.to_string (StringMap.cardinal raw_vars)) *)
        StringSet.of_list vars_list
    ) in
    Format.fprintf ff "@,@[<v1>%a@," Ast.fprint_stm stm;
    if not (StringSet.is_empty top_var)
    then (
      Format.fprintf ff "top vars: @[";
      StringSet.iter (
        fun v -> Format.fprintf ff "%s,@," v
      ) top_var;
      Format.fprintf ff "@]@,"
    )
    ;
    Format.fprintf ff "vars: @,@[<v2>  ";
    match properties with
    | None -> ()
    | Some (properties) ->
      List.iter (
        fun (k, v) ->
          Format.fprintf ff "%s: %a@," k Value_properties.fprint v
      ) (StringMap.bindings properties)
    ;
    Format.fprintf ff "@]@]"
  in

  let get_vp_key (key: Value_properties.key) (vp: Value_properties.t) : bool =
    let rec list_eq (a: 'a list) (b: 'a list) =
      match a, b with
      | [], [] -> true
      | ela :: tla, elb :: tlb when (String.compare ela elb) == 0 -> list_eq tla tlb
      | _ -> false
    in
    List.fold_left (
      fun acc (k, v) -> (
        acc || (list_eq k key)
      )
    ) false (Value_properties.M.bindings vp)
  in

  let check_vp (vp: Value_properties.t) (v: string) (key: string) =
    if get_vp_key [key] vp
    then failwith (Printf.sprintf "%s is %s" v key)
  in

  let check_properties ff l stm vars =
    let properties = get_properties l in
    match properties with
    | None -> ()
    | Some (properties) ->
      (* Any none matched var is top *)
      let top_var =  StringSet.diff vars (
        let vars_list = List.fold_left (
          fun acc (k, v) -> k :: acc
        ) [] (StringMap.bindings properties)
        in
        (* StringSet.singleton (Int.to_string (StringMap.cardinal raw_vars)) *)
        StringSet.of_list vars_list
      ) in
      let fprint_set (ff: Format.formatter) (s: StringSet.t) =
        let v = StringSet.choose s in
        let s = StringSet.remove v s in
        Format.fprintf ff "%s" v;
        StringSet.iter (
          fun v -> Format.fprintf ff ",@,%s" v
        ) s
      in
      if not (StringSet.is_empty top_var) then failwith (
        Format.asprintf "Some vars are top: %a" fprint_set top_var
      );
      List.iter (
        fun (v, p) -> (
          let check_vp = check_vp p v in
          check_vp "NaN";
          check_vp "MInf";
          check_vp "PInf"
        )
      ) (StringMap.bindings properties)
  in

  let rec iter
    (ff: Format.formatter)
    (f: Format.formatter -> Location.t -> Ast.stm -> StringSet.t -> unit)
    (stm: Ast.stm)
    (vars: StringSet.t): StringSet.t =
      match stm with
      | Seq (l, s1, s2) ->
        let vars1 = iter ff f s1 vars in
        let vars2 = iter ff f s2 vars1 in
        vars2
      | Ite (l, _, s1, s2) ->
        let vars1 = iter ff f s1 vars in
        let vars2 = iter ff f s2 vars in
        StringSet.inter vars1 vars2
      | Asn (l, z, _) ->
        let vars = StringSet.add z vars in
        f ff l stm vars;
        vars
      | Nop (l) -> vars
  in

  let ff = Format.std_formatter in
  Format.fprintf ff "@[<v1>";
  (* ignore (iter ff print_loc stm StringSet.empty); *)
  ignore (iter ff check_properties stm StringSet.empty);
  Format.fprintf ff "@]@."

let choose_detector
  (flag_why3: bool)
  (flag_zero: bool)
  (flag_bitblasting: bool)
  (fp_format: int) = (
    let module FloatFormat = (val (Float_format.to_float_format fp_format): Float_format.FloatFormat) in
    (* bitblasting is a special case *)
    if flag_bitblasting
    then (
      let module ConstraintGenerator = Bitbalst_constraint.BitblastConstraintGenerator in
      let module Solver = Bitblast_constraint_solver.MakeBitblastConstraintSolver (FloatFormat) in
      (module Pitfalls_detector.MakePitfallDetector (ConstraintGenerator) (Solver) : Pitfalls_detector.PitfallDetector)
    )
    else (
      let gen = (module
          Constraint_generator.MakeConstraintGenerator
            (FloatFormat)
            (Constraint_model.MakeConstraintModelZero (FloatFormat))
            (Pitfall_model.PitfallModelZero) : Constraint_generator.ConstraintGenerator
      ) in
      let gen = (
        if flag_zero
        then (module
          Constraint_generator.MakeConstraintGenerator
            (FloatFormat)
            (Constraint_model.MakeConstraintModelZero (FloatFormat))
            (Pitfall_model.PitfallModelZero) : Constraint_generator.ConstraintGenerator with type constraint_t = Constraint.constraints
        )
        else (module
          Constraint_generator.MakeConstraintGenerator
            (FloatFormat)
            (Constraint_model.MakeConstraintModelBase (FloatFormat))
            (Pitfall_model.PitfallModelStandard) : Constraint_generator.ConstraintGenerator with type constraint_t = Constraint.constraints
        )
      ) in
      let module ConstraintGenerator = (val gen : Constraint_generator.ConstraintGenerator with type constraint_t = Constraint.constraints) in
      if flag_why3
      then (
        let module Solver = Constraint_solver_why.WhyConstraintSolver in
        (module Pitfalls_detector.MakePitfallDetector (ConstraintGenerator) (Solver) : Pitfalls_detector.PitfallDetector)
      )
      else (
        let module Solver = Constraint_solver_z3.Z3ConstraintSolver in
        (module Pitfalls_detector.MakePitfallDetector (ConstraintGenerator) (Solver) : Pitfalls_detector.PitfallDetector)
      )
    )
  )

let parse_tiny_prgm (path: string) : Ast.Var.Set.t * Ast.stm = (
  print_endline "\nParse Tiny program";
  let vars, tiny_ast = Parse.file path in
  vars, tiny_ast
)

let input_file = ref None
let set_input_file filename =
  match !input_file with
  | None -> input_file := Some filename
  | Some _ ->
     raise (Arg.Bad ("Only accepts one input file: superfluous file \""
                     ^ filename ^ "\""))

let _ =
  let usage_msg = "usage" in
  let use_why3 = ref false in
  let zero_mode = ref false in
  let bit_blast = ref false in
  let yaml_filename = ref "" in
  let input_filename = ref "" in
  let fpcore_format = ref false in
  let fp_format = ref 32 in
  let sanity_check_enable = ref false in
  let unroll_no_branch = ref false in
  let unroll_n = ref (0) in
  let speclist = Arg.align [
    ("-why3", Arg.Set use_why3, "Use why3 backend");
    ("-zero", Arg.Set zero_mode, "Consider 0 as special value");
    ("-bitblasting", Arg.Set bit_blast, "Use bitblasting check");
    ("-yaml", Arg.Set_string yaml_filename, "Output yaml report");
    ("-fpcore", Arg.Set fpcore_format, "Use fpcore input format");
    ("-f", Arg.Set_int fp_format, "Floating-Point format, 32bit float by default");
    ("-sanity_check", Arg.Set sanity_check_enable, "Enable sanity checks");
    ("-unroll_no_branch", Arg.Set unroll_no_branch, "Disable branch in urolling");
    ("-unroll_n", Arg.Set_int unroll_n, "Enable unrolling [n] times");
  ] in
  let check_argument () = (
    (* check floating-point format *)
    (
      match !fp_format with
      | 16 | 32 -> ()
      | _ -> failwith (Printf.sprintf "Unsupported format %d" !fp_format)
    );
    (* check that -bitblasting is used alone *)
    if (!bit_blast && (!use_why3 || !zero_mode))
    then (
      Printf.eprintf "-bitblasting must be used alone";
      Arg.usage speclist usage_msg;
      raise Report.Error
    )
    else (
      match !input_file with
      | None ->
        Printf.eprintf "%s: No input file provided.\n" Sys.argv.(0);
        Arg.usage speclist usage_msg;
        raise Report.Error
      | Some filename ->
          input_filename := filename;
          (* auto set -fpcore if needed *)
          (
            let re = Str.regexp_string ".fpcore" in
            try
              ignore (Str.search_forward re filename 0);
              fpcore_format := true
            with Not_found -> ()
          );
    )
  ) in
  try
    Arg.parse speclist set_input_file usage_msg;
    check_argument ();

    match !input_file with
    | None ->
      Printf.eprintf "%s: No input file provided.\n" Sys.argv.(0);
      Arg.usage speclist usage_msg
    | Some input_filename -> (

      (* Get ast *)

      let vars, tiny_ast = (
        if !fpcore_format
        then Parse_fpcore.parse_fpcore_prgm input_filename
        else parse_tiny_prgm input_filename
      ) in
      Format.fprintf (Format.std_formatter) "@,Initial program:@,";
      Format.fprintf (Format.std_formatter) "%a@." Ast.fprint_stm tiny_ast;
      let do_unroll = !unroll_n > 0 in
      let tiny_ast = if do_unroll then 
        let ast = Ast.unroll_while ~use_branch:(not !unroll_no_branch) ~rename:true !unroll_n tiny_ast in
        Format.fprintf (Format.std_formatter) "@,Program after loop unrolling:@,";
        Format.fprintf (Format.std_formatter) "%a@." Ast.fprint_stm ast;
        ast
    else tiny_ast in
      let ast, tiny_ast = Parse_fpcore.prepare_program tiny_ast in
      let vars = Typing.get_vars tiny_ast in 
      Format.fprintf (Format.std_formatter) "@,Analysed program:@,";
      Format.fprintf (Format.std_formatter) "%a@." Ast.fprint_stm tiny_ast;
      print_endline "Program parsed";

      (* Get varaible ranges from tiny *)
      let domain_name = Printf.sprintf "interval_float_f%d" !fp_format in
      let dom = prepare_domains (List.map get_domain [domain_name]) in
      let results = Analyze.analyze dom (-1) (-1) (-1) true 1 0 vars tiny_ast in
      let module Results = (val results: Analyze.Results) in

      let m = Results.results in
      check results tiny_ast;

      let machine_state = Machine_state.MachineStateSet.get_machine_state tiny_ast results in
      let module Detector = (val choose_detector !use_why3 !zero_mode !bit_blast !fp_format : Pitfalls_detector.PitfallDetector) in
      let res, sanity_result = Detector.detect_pitfall ~sanity_check:!sanity_check_enable ast machine_state in

      (* Output result *)
      print_endline (
        List.fold_left (
          fun acc p -> (Pitfall_report.string_of_pitfall_report p) ^ acc
        ) "" res
      );

      (* Save result as yaml file *)
      (
        if !yaml_filename != ""
        then Yaml_unix.to_file_exn (Fpath.v !yaml_filename) (Pitfall_report.yaml_of_pitfall_report_list res sanity_result)
        else ()
      );
      ()
    )
  with Report.Error -> exit 2