open Subset_ast_expr

type stm =
  | Seq of Location.t * stm * stm
  | Asn of Location.t * string * expr
  | Ite of Location.t * bool_expr * stm * stm
  | Nop of Location.t

(* Convert tiny statement to simple statement *)
let rec to_simple_stm (stm : Ast.stm) =
  match stm with
  | Seq (l, s1, s2) -> Seq (l, to_simple_stm s1, to_simple_stm s2)
  | Asn (l, z, e) -> Asn (l, z, to_simple_expr e)
  | Ite (l, g, s1, s2) ->
      (* chek guard *)
      let g =
        match g.expr_desc with
        | Cond (expr, cmp) ->
            (* make sure it is a comparison of a variable to 0 *)
            (* a - 0 <  0 *)
            (* a - 0 <= 0 *)
            let v =
              match expr.expr_desc with
              | Binop (bop, e1, e2) ->
                  if bop == Minus then
                    (* e1 must be a variable *)
                    match e1.expr_desc with
                    | Var v -> (
                        (* e2 must be 0 *)
                        match e2.expr_desc with
                        | Cst (q, _) when Q.equal q Q.zero -> v
                        | __ -> failwith "Unsuported condition (e2)")
                    | _ -> failwith "Unsuported condition (e1)"
                  else failwith "Unsuported operator in condition"
              | Var v -> v
              | _ ->
                  failwith
                    (Format.asprintf "Unsuported condition: %a (%s)"
                       Ast.fprint_expr expr (Location.to_string l))
            in
            BCond (v, to_simple_cmp cmp)
        | _ ->
            Ast.fprint_expr Format.std_formatter g;
            failwith "unsuported guard expression"
      in
      Ite (l, g, to_simple_stm s1, to_simple_stm s2)
  | Nop l -> Nop l
  | _ -> failwith "unsuported statement"

(* Convert statement to string *)
let rec string_of_stm = function
  | Seq (_, s1, s2) ->
      Printf.sprintf "%s;\n%s" (string_of_stm s1) (string_of_stm s2)
  | Asn (_, var, e) -> Printf.sprintf "%s = %s" var (string_of_expr e)
  | Ite (_, g, s1, s2) ->
      Printf.sprintf "if (%s)\nthen\n%s\nelse\n%s\nendif"
        (string_of_bool_expr g) (string_of_stm s1) (string_of_stm s2)
  | Nop _ -> "pass"

(* Retrun list of all the variables and of bound variables *)
let extract_variables (stm : stm) : StringSet.t * StringSet.t =
  let rec _extract_variables (stm : stm) (all_vars : StringSet.t)
      (bound_vars : StringSet.t) : StringSet.t * StringSet.t =
    match stm with
    | Seq (_, s1, s2) ->
        let all_vars, bound_vars = _extract_variables s1 all_vars bound_vars in
        _extract_variables s2 all_vars bound_vars
    | Asn (_, var, e) ->
        (* Add vars to all and bounded variables *)
        let all_vars = StringSet.add var all_vars in
        let bound_vars = StringSet.add var bound_vars in

        (* extract varables from the expression *)
        let all_vars = StringSet.union all_vars (vars_of_expr e) in
        (all_vars, bound_vars)
    | Ite (_, _, s1, s2) ->
        (* extract variables from s1 *)
        let all_vars, bound_vars = _extract_variables s1 all_vars bound_vars in

        (* extract variables from s2 *)
        _extract_variables s2 all_vars bound_vars
    | Nop _ -> (all_vars, bound_vars)
  in
  _extract_variables stm StringSet.empty StringSet.empty

let bound_to_rand_var (stm : stm) : StringSet.t =
  let rec bound_to_rand_var (stm : stm) (vars : StringSet.t) : StringSet.t =
    match stm with
    | Seq (_, s1, s2) | Ite (_, _, s1, s2) ->
        let vars = bound_to_rand_var s1 vars in
        bound_to_rand_var s2 vars
    | Asn (_, z, e) ->
        if Subset_ast_expr.expr_is_rand e then StringSet.add z vars else vars
    | Nop _ -> vars
  in
  bound_to_rand_var stm StringSet.empty

let rec to_tiny_stm (stm : stm) : Ast.stm =
  match stm with
  | Seq (l, s1, s2) -> Ast.Seq (l, to_tiny_stm s1, to_tiny_stm s2)
  | Asn (l, v, e) ->
      let e = Subset_ast_expr.expr_to_tiny_expr e l Ast.RealT in
      Ast.Asn (l, v, e)
  | Ite (l, g, s1, s2) ->
      let g = Subset_ast_expr.bool_expr_to_tiny_expr g l in
      let s1 = to_tiny_stm s1 in
      let s2 = to_tiny_stm s2 in
      Ast.Ite (l, g, s1, s2)
  | Nop l -> Nop l

let rec fprint_stm_to_C (fmt : Format.formatter) (stm : stm) : unit =
  match stm with
  | Seq (_, s1, s2) ->
      Format.fprintf fmt "%a%a" fprint_stm_to_C s1 fprint_stm_to_C s2
  | Ite (_, g, s_then, s_else) ->
      Format.fprintf fmt "if (%a) {@[<v4>@,%a@]@,} else {@[<v4>@,%a@]}"
        Subset_ast_expr.fprint_boolexpr_to_C g fprint_stm_to_C s_then
        fprint_stm_to_C s_else
  | Asn (_, v, e) ->
      if not (Subset_ast_expr.expr_is_rand e) then
        Format.fprintf fmt "%s = %a;@," v Subset_ast_expr.fprint_expr_to_C e
  | Nop _ -> ()

let stm_to_C_prgm (fmt : Format.formatter) (stm : stm) : unit =
  let all_vars, bound_vars = extract_variables stm in
  let free_vars = StringSet.diff all_vars bound_vars in
  let rand_vars = bound_to_rand_var stm in
  let input_vars = StringSet.union free_vars rand_vars in
  let def_vars = StringSet.diff all_vars input_vars in
  let input_vars = StringSet.to_list input_vars in
  let input_vars = List.sort String.compare input_vars in
  let fdef_vars (fmt : Format.formatter) (vars : StringSet.t) =
    StringSet.iter (fun v -> Format.fprintf fmt "float %s;@," v) vars
  in
  Format.fprintf fmt "#include <math.h>@.";
  Format.fprintf fmt "float ex(%s) {@,@[<v4>    %a%a@]@.}@."
    (match input_vars with
    | el1 :: el2 :: tl ->
        List.fold_left
          (fun acc v -> Printf.sprintf "%s, float %s" acc v)
          ("float " ^ el1) (el2 :: tl)
    | el :: [] -> Printf.sprintf "float %s" el
    | [] -> "")
    fdef_vars def_vars fprint_stm_to_C stm;
  let fprint_list (f : int -> Format.formatter -> string -> unit)
      (sep : Format.formatter -> unit -> unit) fmt vars =
    match vars with
    | el1 :: tl ->
        f 0 fmt el1;
        List.iteri
          (fun i v -> Format.fprintf fmt "%a%a" sep () (f (i + 1)) v)
          tl
    | [] -> ()
  in
  Format.fprintf fmt
    "@,\
     #include <stdlib.h>@.int main(int argc, char *argv[]) {@,\
     @[<v4>    %a@,\
     ex(%a);@,\
     return 0;@]@.}"
    (fprint_list
       (fun i fmt v ->
         Format.fprintf fmt "float %s = atof(argv[%d]);" v (i + 1))
       (fun fmt _ -> Format.fprintf fmt "@,"))
    input_vars
    (fprint_list
       (fun _ fmt v -> Format.fprintf fmt "%s" v)
       (fun fmt _ -> Format.fprintf fmt ", "))
    input_vars
