(*
 * TINY (Tiny Is Not Yasa (Yet Another Static Analyzer)):
 * a simple abstract interpreter for teaching purpose.
 * Copyright (C) 2012  P. Roux
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let simulation_with_scen = ref (-1) (* number of iteration for the mode -sc *)
let simulation_with_scen_full = ref (-1) (* number of iteration for the mode -scf *)
let simulation_with_scen_trans = ref (-1) (* number of iteration for the mode -scf *)

let input_file = ref None
let output_file = ref None
let compile_mode = ref false
let descending = ref 1
let unrolling = ref 0
let invariant_vs_simulation_mode = ref true (* true = invariant, false = simulation *)


open Tiny
open Load_domains
  

let quiet () = Report.verbosity := 0

let set_input_file filename =
  match !input_file with
  | None -> input_file := Some filename
  | Some _ ->
     raise (Arg.Bad ("Only accepts one input file: superfluous file \""
                     ^ filename ^ "\""))

let set_output_file s = output_file := Some s

let _ =
  let usage_msg = Printf.sprintf
    "Usage: %s [options] <input_filename>" Sys.argv.(0) in
  let speclist = Arg.align [
    ("--abstract-domain", Arg.String decl_domain,
     "<domain>  Use abstract domain <domain> " ^ Domains.available_domains_str);
    ("-a", Arg.String decl_domain,
     "<domain>  Use abstract domain <domain> " ^ Domains.available_domains_str);
    ("--param", Arg.String set_param,
     "<p>  Send <p> to the abstract domain, syntax <dom>:<p> can be used \
      to target the (sub)domain <dom>");
    ("-p", Arg.String set_param,
     "<p>  Send <p> to the abstract domain, syntax <dom>:<p> can be used \
      to target the (sub)domain <dom>");
    ("--help-domain", Arg.String help_domain,
     "<domain>  Print params of <domain>");
    ("-h", Arg.String help_domain, "<domain>  Print params of <domain>");
    ("--compile", Arg.Set compile_mode, " Compilation mode: compile to C");
    ("-c", Arg.Set compile_mode, " Compilation mode: compile to C");
    ("--quiet", Arg.Unit quiet, " Quiet mode");
    ("-q", Arg.Unit quiet, " Quiet mode");
    ("--verbose", Arg.Set_int Report.verbosity,
     "<n>  Verbosity level (default is 1)");
    ("-v", Arg.Set_int Report.verbosity, "<n>  Verbosity level (default is 1)");
    ("--output", Arg.String set_output_file,
     "<filename>  Output results to file <filename> (default is standard ouput)");
    ("-o", Arg.String set_output_file,
     "<filename>  Output results to file <filename> (default is standard ouput)");
    ("--descending", Arg.Set_int descending,
     "<n>  Perform <n> descending iterations after fixpoint of a loop \
      is reached (default is 1)");
    ("-d", Arg.Set_int descending,
     "<n>  Perform <n> descending iterations after fixpoint of a loop \
      is reached (default is 1)");
    ("--unrolling", Arg.Set_int unrolling,
     "<n>  Unroll loops <n> times before computing fixpoint (default is 0)");
    ("-u", Arg.Set_int unrolling,
     "<n>  Unroll loops <n> times before computing fixpoint (default is 0)");
    ("--simu", Arg.Clear invariant_vs_simulation_mode,
     "  Perform set-based simulation with the abstract domain, without computing fixpoint");
    ("-s", Arg.Clear invariant_vs_simulation_mode,
     "  Perform set-based simulation with the abstract domain, without computing fixpoint");
    ("-sc", Arg.Set_int simulation_with_scen,
    "<n>  Perform set based simulation for <n> iterations and uses the scenario files for the variables given as argument to instruction read_input");
    ("-scf", Arg.Set_int simulation_with_scen_full,
    "<n>  Similar to -sc option, all iterations of the variables are kept in the environment.");
    ("-sct", Arg.Set_int simulation_with_scen_trans,
    "<n>  Similar to -sc option, excepts variables in the environemment represent the transitions between iterations.");
  ] in
  try  
    Arg.parse speclist set_input_file usage_msg;
    (* try to set terminal width for format *)
    let cols = Sys.command "exit `stty size | cut -d\" \" -f2`" in
    if cols >= 32 then Format.set_margin cols;
    match !input_file with
    | None ->
      Printf.eprintf "%s: No input file provided.\n" Sys.argv.(0);
      Arg.usage speclist usage_msg
    | Some input_filename ->
      let vars, ast = Parse.file input_filename in
      if !compile_mode then
        Compile.compile input_filename !output_file
      else
	let dom =
	  match !domains with 
	  | [] -> (
	    match Domains.available_domains with
	    | [] -> Report.error "compiled without any abstract domain!"
	    | d :: _ -> prepare_domain d
	  )
    | _ -> prepare_domains (List.map get_domain !domains)
        in

        Report.nlogf 1 "Analyze file %s, writing results to %s."
        input_filename (Utils.output_filename_string !output_file);
        (* Process NN constructs *)
        let vars, ast = Nn.process_ast vars ast in 

        (******************)
        (* For the set based simulation mode only *)

        let dom =            

          if !simulation_with_scen > (-1) || !simulation_with_scen_full > (-1) || !simulation_with_scen_trans > (-1) then
            let module DomBase = (val dom : Relational.Domain) in

            let scenario = Scenario.Scenario.empty in

            let updated_scenario, _, read_inputs_vars = Scenario.BuildScenario.construct_scenario ast vars scenario Scenario.NameSet.empty Scenario.NameSet.empty in

            (****)
            (* HERE : Creation of a scenario of constraints for the input variables given to read_inputs instructions and k, the iterator *)

            (*add k to the environment (iterator)*)
            let vars = Ast.Var.Set.add ("k", Ast.RealT) vars in

            (*
            Create a very simple constraint scenario for i (used with the controller_simu_simple.tiny file) :
              c1 : -k <= i <= k, for 4 <= k <= 8
              c2 : -10k <= i <= 10k, for 9 <= k <= 15
            *)
            
              let c1_leq = Scenario.BuildConstraint.Leq ("i", [("k", 1.0)], 0.) in
              let c1_geq = Scenario.BuildConstraint.Geq ("i", [("k", -1.0)], 0.) in

              let c2_leq = Scenario.BuildConstraint.Leq ("i", [("k", 10.0)], 0.) in
              let c2_geq = Scenario.BuildConstraint.Geq ("i", [("k", -10.0)], 0.) in

              let list_constraints_t = [c1_leq; c1_geq; c2_leq; c2_geq] in

              (* Check that the constraints are only applied to inputs variables *)
              Scenario.BuildConstraint.check_constraints list_constraints_t read_inputs_vars;

              let ast_expr_c1_leq = Scenario.BuildConstraint.t_to_astexpr c1_leq in 
              let ast_expr_c1_geq = Scenario.BuildConstraint.t_to_astexpr c1_geq in

              let ast_expr_c2_leq = Scenario.BuildConstraint.t_to_astexpr c2_leq in
              let ast_expr_c2_geq = Scenario.BuildConstraint.t_to_astexpr c2_geq in

              let scenario1 = Scenario.BuildConstraint.create_scenario 4 5 [ast_expr_c1_leq; ast_expr_c2_geq] in
              let scenario2 = Scenario.BuildConstraint.create_scenario 9 15 [ast_expr_c2_leq; ast_expr_c2_geq] in

              (*creating a constraint on a state variable : y <= 2k at iteration 5*)
              let cs = Scenario.BuildConstraint.Leq ("y", [("k", 2.0)], 0.) in
              let ast_expr_cs = Scenario.BuildConstraint.t_to_astexpr cs in 
              let scenario_constraint_y = [Scenario.BuildConstraint.create_scenario 5 5 [ast_expr_cs]] in



            let scenario_constraints = [scenario1] in

            (****)

            let module S = Scenario.Make(
            struct
              let max_iter =
                if !simulation_with_scen = (-1) then
                  if !simulation_with_scen_full = (-1)
                    then !simulation_with_scen_trans
                  else !simulation_with_scen_full
                else !simulation_with_scen
              let input_vars = read_inputs_vars
              let scenario = updated_scenario
              (***)
              let scenario_constraints = scenario_constraints
              let scenario_constraints_states = scenario_constraint_y
              (***)
             end) in 
            let module DomSimInstance =
              (val
                if !simulation_with_scen > -1
                then (module DomSim.DomSim(DomBase)(S) : Relational.Domain)
                else if !simulation_with_scen_full > -1
                  then (module DomSim2.DomSim2(DomBase)(S) : Relational.Domain)
                else (module DomSim3.DomSim3(DomBase)(S) : Relational.Domain)
              )
            in
            (module DomSimInstance : Relational.Domain)

          else
            dom

          (******)

        in
     
        let results = Analyze.analyze dom !simulation_with_scen !simulation_with_scen_full !simulation_with_scen_trans !invariant_vs_simulation_mode !descending !unrolling vars ast in
        let module Results = (val results: Analyze.Results) in
        let module Res = Results.Res in
        let module PrintResults = PrintResults.Make (Res) in
        let pp = Utils.with_out_ch !output_file in
        let m = Results.results in
        if !Report.verbosity > 1 then pp (PrintResults.print m vars ast)
        else if !Report.verbosity > 0 then pp (PrintResults.print_invariants m vars ast)
  with Report.Error -> exit 2
