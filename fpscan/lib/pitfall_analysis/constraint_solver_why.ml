open Constraint_solver
open Constraint

module WhyConstraintSolver () :
  ConstraintSolver with type constraint_t = constraints = struct
  type constraint_t = constraints

  let get_why_result (file_name : string) : bool =
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_file file_name in
    let res_str =
      json |> member "prover-result" |> member "answer" |> to_string
    in
    match res_str with "Valid" -> true | _ -> false

  let rec constraint_val_to_why (ff : Format.formatter) = function
    | Var v -> Format.fprintf ff "%s" v
    | Cst i -> Format.fprintf ff "%d" i
    | Ulp v -> Format.fprintf ff "ulp %s" v
    | Ufp v -> Format.fprintf ff "ufp %s" v
    | Nsb v -> Format.fprintf ff "nsb %s" v
    | P v -> Format.fprintf ff "p %s" v
    | Max (x, y) ->
        Format.fprintf ff "max (%a) (%a)" constraint_val_to_why x
          constraint_val_to_why y
    | Min (x, y) ->
        Format.fprintf ff "min (%a) (%a)" constraint_val_to_why x
          constraint_val_to_why y
    | BinOp (bop, x, y) ->
        let op = match bop with Plus -> "+" | Minus -> "-" | Times -> "*" in
        Format.fprintf ff "(%a) %s (%a)" constraint_val_to_why x op
          constraint_val_to_why y

  let rec constraint_to_why (ff : Format.formatter) (cstr : constraint_t) =
    let fprint_cstr_list op ff (l : constraint_t list) =
      let first = List.hd l in
      let tl = List.tl l in
      if List.is_empty tl then Format.fprintf ff "%a" constraint_to_why first
      else (
        Format.fprintf ff "@[<v2>%a" constraint_to_why first;
        List.iter
          (fun c ->
            Format.fprintf ff "@,%s %a" op constraint_to_why c
            (* fun c -> Format.fprintf ff "%s %s@," op "c"  *)
            (* fun c -> Format.fprintf ff "l" *))
          tl;
        Format.fprintf ff "@]")
    in
    match cstr with
    | True -> Format.fprintf ff "true"
    | False -> Format.fprintf ff "false"
    | And cstr_list -> fprint_cstr_list "/\\" ff cstr_list
    | Or cstr_list ->
        Format.fprintf ff "(%a)" (fprint_cstr_list "\\/") cstr_list
    | Not cstr -> Format.fprintf ff "not (%a)" constraint_to_why cstr
    | Eq (c1, c2) ->
        Format.fprintf ff "(%a) = (%a)" constraint_to_why c1 constraint_to_why
          c2
    | LOp (lop, x, y) ->
        let lop =
          match lop with
          | Geq -> ">="
          | Gt -> ">"
          | Leq -> "<="
          | Lt -> "<"
          | Eq -> "="
        in
        Format.fprintf ff "(%a) %s (%a)" constraint_val_to_why x lop
          constraint_val_to_why y
    | ZeroInBound v -> Format.fprintf ff "zero_in_bound %s" v
    | ZeroReachable v -> Format.fprintf ff "zero_reachable %s" v

  let mk_header ff () =
    Format.fprintf ff
      "\n\
       theory Prgm\n\
      \   use int.Int\n\
      \   use int.MinMax\n\
      \   use Bool\n\
      \   type mFloat = {\n\
      \       ufp: int; \n\
      \       nsb: int; \n\
      \       p: int;\n\
      \       zero_in_bound: bool;\n\
      \       zero_reachable: bool\n\
      \   }\n\
      \   let function ulp (x: mFloat) = ufp x - p x"

  let mk_prgm (cstr : constraint_t) (vars : string list) (ff : Format.formatter)
      () : unit =
    Format.fprintf ff "@[<v2>predicate prgm@,@[<v0>";
    List.iter (fun v -> Format.fprintf ff "(%s: mFloat)@," v) vars;
    Format.fprintf ff "@]= %a" constraint_to_why cstr;
    Format.fprintf ff "@]"

  let mk_goal (ff : Format.formatter) (vars : string list) =
    Format.fprintf ff "@[<v2>goal G:";
    List.iter (Format.fprintf ff "@,forall %s: mFloat.") vars;
    Format.fprintf ff "@,  not (prgm %s) "
      (List.fold_left (fun acc v -> acc ^ " " ^ v) "" vars);
    Format.fprintf ff "@]"

  let solve_sat (cstr : constraint_t) : bool =
    (* let ff = Format.std_formatter in *)
    let oc = open_out "prgm.why" in
    let ff = Format.formatter_of_out_channel oc in
    let vars = cstr |> Constraint.get_vars |> StringSet.to_list in
    Format.fprintf ff "@[<v2>%a@,@,%a@,@,%a@,end@.]" mk_header ()
      (mk_prgm cstr vars) () mk_goal vars;
    close_out oc;

    if not (Sys.file_exists "out") then Sys.mkdir "out" 644;
    let why_out =
      Unix.openfile "why.out" [ Unix.O_RDWR; Unix.O_CREAT; O_TRUNC ] 0o644
    in
    let pid =
      Unix.create_process "why3"
        [| "why3"; "prove"; "prgm.why"; "-PZ3,4.15.2"; "--json" |]
        Unix.stdin why_out Unix.stderr
    in

    (* wait for why3 to complete *)
    let _, _ = Unix.waitpid [] pid in
    Unix.close why_out;

    not (get_why_result "why.out")
end
