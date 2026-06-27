module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

(* Symbol *)

type symbol = string

(* Number *)

type number = NDec of Q.t

(* Argument *)

type argument = ASymbol of symbol

(* Expressions *)

type operator = Plus | Minus | Mult | Div | Gt | Geq | And | Neq

let string_of_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Gt -> ">"
  | Geq -> "=>"
  | And -> "and"
  | Neq -> "!="

type expr =
  | ENumber of number
  | ESymbol of symbol
  | EOp of operator * expr list
  | ELet of (symbol * expr) list * expr
  | ELetStar of (symbol * expr) list * expr
  | EIte of expr * expr * expr
  | ECall of string * expr list
  | EWhile of expr * (string * expr * expr) list * expr

let rec string_of_expr = function
  | ENumber (NDec n) -> Q.to_string n
  | ESymbol s -> s
  | EOp (op, operand_list) -> (
      match operand_list with
      (* An operator must have at least 2 operands *)
      | [] -> failwith "An operator must have at least one operand"
      | e :: [] -> (
          (* case of unary - *)
          match op with
          | Minus ->
              Printf.sprintf "%s %s" (string_of_operator op) (string_of_expr e)
          | _ -> failwith "binary operator must have 2 operands")
      (* Print all the operands joined by the operator symbol *)
      | el :: tail ->
          "("
          ^ List.fold_left
              (fun acc e ->
                acc ^ " " ^ string_of_operator op ^ " " ^ string_of_expr e)
              (string_of_expr el) tail
          ^ ")")
  | ELet (bindings, e) ->
      "(let"
      (* Add the bindings *)
      ^ List.fold_left
          (fun acc (s, e) -> acc ^ " (" ^ s ^ " " ^ string_of_expr e ^ ")")
          "" bindings
      (* Add the expression *)
      ^ " "
      ^ string_of_expr e ^ ")"
  | ELetStar (bindings, e) ->
      "(let*"
      (* Add the bindings *)
      ^ List.fold_left
          (fun acc (s, e) -> acc ^ " (" ^ s ^ " " ^ string_of_expr e ^ ")")
          "" bindings
      (* Add the expression *)
      ^ " "
      ^ string_of_expr e ^ ")"
  | EIte (c, e1, e2) ->
      Printf.sprintf "if (%s) then (%s) else (%s)" (string_of_expr c)
        (string_of_expr e1) (string_of_expr e2)
  | ECall (f, args) ->
      Printf.sprintf "%s (%s)" f
        (List.fold_left
           (fun acc e -> acc ^ ", " ^ string_of_expr e)
           (args |> List.hd |> string_of_expr)
           (List.tl args))
  | EWhile (cond, bindings_list, ret) ->
      let cond_str = string_of_expr cond in
      let string_of_bind el =
        let v, init, update = el in
        Printf.sprintf "(%s %s %s)" v (string_of_expr init)
          (string_of_expr update)
      in
      let bindings_str =
        match bindings_list with
        | [] -> ""
        | el :: [] -> string_of_bind el
        | el :: tl ->
            List.fold_left
              (fun acc el -> Printf.sprintf "%s %s" acc (string_of_bind el))
              (string_of_bind el) tl
      in
      let ret_str = string_of_expr ret in
      Printf.sprintf "(while (%s) (%s) (%s))" cond_str bindings_str ret_str

(* Property *)

type data = DExpr of expr | DString of string
type property = Prop of string * data

(* FPCore *)

type fpcore = FPCore of argument list * property list * expr

let string_of_fpcore = function FPCore (_, _, e) -> string_of_expr e

(* Tiny program *)

let extract_val = function
  | ENumber (NDec q) -> q
  | _ -> failwith "invlid operation in pre"

let extract_var = function
  | ESymbol s -> s
  | _ -> failwith "invlid operation in pre"

let expr_to_asn_interval (e : expr) =
  let module StringMap = Map.Make (String) in
  let upper_bounds : Q.t StringMap.t = StringMap.empty in
  let lower_bounds : Q.t StringMap.t = StringMap.empty in

  let get_var (e : expr) : string option =
    match e with ESymbol v -> Some v | _ -> None
  in

  let get_val (e : expr) : Q.t option =
    match e with ENumber (NDec q) -> Some q | _ -> None
  in

  let rec expr_to_asn_interval (e : expr) (lower_bounds : Q.t StringMap.t)
      (upper_bounds : Q.t StringMap.t) : Q.t StringMap.t * Q.t StringMap.t =
    match e with
    | ENumber _ | ESymbol _ | ELet _ | ELetStar _
    | EIte (_, _, _)
    | ECall (_, _)
    | EWhile (_, _, _) ->
        failwith ("invalid pre: " ^ string_of_expr e)
    | EOp (bop, e_list) -> (
        match bop with
        | Gt | Geq -> (
            match e_list with
            | [] | _ :: [] -> failwith "pre: comparasion requires more argument"
            | [ lhs; rhs ] -> (
                (* 2 values *)
                (* find var *)
                match (get_var lhs, get_var rhs) with
                | None, None -> failwith "pre: one operand must be a variable"
                | Some _, Some _ -> failwith "pre: both operand are var"
                (* var > val *)
                | Some v, None -> (
                    match get_val rhs with
                    | None ->
                        failwith
                          (Printf.sprintf "pre: comparison requires a val: %s"
                             (string_of_expr rhs))
                    | Some value ->
                        (StringMap.add v value lower_bounds, upper_bounds))
                (* val > var *)
                | None, Some v -> (
                    match get_val lhs with
                    | None ->
                        failwith
                          (Printf.sprintf "pre: comparison requires a val: %s"
                             (string_of_expr lhs))
                    | Some value ->
                        (lower_bounds, StringMap.add v value upper_bounds)))
            | [ lhs; mid; rhs ] ->
                (* 3 operands *)
                (* rhs: val, mid: var, lhs: val*)
                (* lhs > mid > rhs *)
                let some_or_fail_val (o : 'a option) (m : string) : 'a =
                  match o with None -> failwith m | Some a -> a
                in
                let some_or_fail_var (o : 'b option) (m : string) : 'b =
                  match o with None -> failwith m | Some a -> a
                in
                let lhs_v =
                  some_or_fail_val (get_val lhs) "pre: lhs must be a value"
                in
                let mid_v : string =
                  some_or_fail_var (get_var mid) "pre: mid must be a variable"
                in
                let rhs_v =
                  some_or_fail_val (get_val rhs) "pre: rhs must be a value"
                in
                ( StringMap.add mid_v rhs_v lower_bounds,
                  StringMap.add mid_v lhs_v upper_bounds )
            | _ -> failwith "pre: too many argument in comparison")
        | And ->
            List.fold_left
              (fun (lower_bounds, upper_bounds) e ->
                expr_to_asn_interval e lower_bounds upper_bounds)
              (lower_bounds, upper_bounds)
              e_list
        | _ -> failwith "pre: bool operator expected")
  in

  let lower_bounds, upper_bounds =
    expr_to_asn_interval e lower_bounds upper_bounds
  in
  (* match lower and upper bounds *)
  let stm_list : Ast.stm list =
    StringMap.fold
      (fun var l_b acc ->
        let u_b = StringMap.find_opt var upper_bounds in
        match u_b with
        | None -> failwith (Printf.sprintf "unmatched lower bound for %s" var)
        | Some u_b ->
            let e =
              Ast.mk_expr (Location.dummy ()) Ast.RealT
                (Ast.Rand ((l_b, Q.to_string l_b), (u_b, Q.to_string u_b)))
            in
            Ast.Asn (Location.dummy (), var, e) :: acc)
      lower_bounds []
  in

  match stm_list with
  | [] -> Ast.Nop (Location.dummy ())
  | s :: [] -> s
  | s :: tl ->
      List.fold_left (fun acc stm -> Ast.Seq (Location.dummy (), acc, stm)) s tl

let pre_to_tiny (prgm : fpcore) : Ast.stm option =
  let (FPCore (_, property_list, _)) = prgm in
  let pre =
    List.fold_left
      (fun acc prop ->
        let (Prop (name, d)) = prop in
        if String.compare name ":pre" == 0 then Some d else acc)
      None property_list
  in

  match pre with
  | None ->
      print_endline "no pre";
      None
  | Some d -> (
      match d with
      | DString _ -> None
      | DExpr e -> Some (expr_to_asn_interval e))

let to_tiny_bop = function
  | Plus -> Ast.Plus
  | Minus -> Ast.Minus
  | Mult -> Ast.Times
  | Div -> Ast.Div
  | Gt | Geq | And | Neq -> failwith "invalid operator"

let is_bool_op = function Gt | Geq | And | Neq -> true | _ -> false

let expr_to_to_tiny_asn (e : expr) (v : string) =
  let new_var_count = ref 0 in
  let new_var () =
    new_var_count := !new_var_count + 1;
    Printf.sprintf "_x_if_%d" !new_var_count
  in
  let new_var_from_var (v : string) : string =
    new_var_count := !new_var_count + 1;
    Printf.sprintf "%s_%d" v !new_var_count
  in
  let join_stm (stm_list : Ast.stm list) : Ast.stm =
    List.fold_left
      (fun acc s -> Ast.Seq (Location.dummy (), acc, s))
      (List.hd stm_list) (List.tl stm_list)
  in
  let rec expr_to_tiny_expr (assigned_vars : StringSet.t)
      (rewrite : string StringMap.t) (e : expr) :
      Ast.stm list * Ast.expr * StringSet.t =
    match e with
    | ENumber (NDec q) ->
        let e =
          Ast.mk_expr (Location.dummy ()) Ast.RealT (Cst (q, Q.to_string q))
        in
        ([], e, assigned_vars)
    | ESymbol s ->
        let s =
          match StringMap.find_opt s rewrite with None -> s | Some v -> v
        in
        let e = Ast.mk_expr (Location.dummy ()) RealT (Ast.Var s) in
        ([], e, assigned_vars)
    | EOp (bop, e_list) ->
        let pred_list, e_list, assigned_vars =
          List.fold_left
            (fun acc e ->
              let pred_list, e_list, assigned_vars = acc in
              let pred, e, assigned_vars =
                expr_to_tiny_expr assigned_vars rewrite e
              in
              (* The argument are reversed in e_list *)
              (pred :: pred_list, e :: e_list, assigned_vars))
            ([], [], assigned_vars) e_list
        in
        (* merge pred_list into one list *)
        let pred_list = List.concat pred_list in
        (* reorder arguments *)
        let e_list = List.rev e_list in

        (* merge e_list into one expression *)
        let e =
          if is_bool_op bop then
            let op =
              match bop with
              | Gt -> Ast.Strict
              | Geq -> Ast.Loose
              | Neq -> Ast.NonZero
              | _ -> failwith "invalid operator"
            in
            (* exactly two operands *)
            let lhs, rhs =
              match e_list with
              | [ lhs; rhs ] -> (lhs, rhs)
              | _ -> failwith "Comparaison only allowed between two values"
            in
            (* Compare to 0 *)
            (* check if rhs is already 0 *)
            let e =
              match rhs.expr_desc with
              | Cst (q, _) when Q.compare q Q.zero == 0 -> lhs
              | _ ->
                  Ast.mk_expr (Location.dummy ()) Ast.RealT
                    (Ast.Binop (Ast.Minus, lhs, rhs))
            in
            Ast.mk_expr (Location.dummy ()) Ast.BoolT (Ast.Cond (e, op))
          else
            let op = to_tiny_bop bop in

            List.fold_left
              (fun acc e ->
                Ast.mk_expr (Location.dummy ()) Ast.RealT
                  (Ast.Binop (op, acc, e)))
              (List.hd e_list) (List.tl e_list)
        in
        (pred_list, e, assigned_vars)
    | ELet (binding_list, e) ->
        let pred_list, assigned_vars, new_rewrite =
          let acc : Ast.stm list list = [] in
          List.fold_left
            (fun acc (s, e) ->
              let stm_list, assigned_vars, new_rewrite = acc in
              let pred, e, assigned_vars, new_rewrite_entry =
                expr_to_tiny_asn assigned_vars rewrite e s
              in
              let new_rewrite =
                match new_rewrite_entry with
                | None -> new_rewrite
                | Some v -> StringMap.add s v new_rewrite
              in
              ([ e ] :: pred :: stm_list, assigned_vars, new_rewrite))
            (acc, assigned_vars, StringMap.empty)
            binding_list
        in
        (* merge rewrite and new_rewrite *)
        let rewrite =
          StringMap.union (fun _ _ v2 -> Some v2) rewrite new_rewrite
        in

        (* let pred_list = List.rev pred_list in *)
        let new_pred_list, e, assigned_vars =
          expr_to_tiny_expr assigned_vars rewrite e
        in
        (List.concat [ new_pred_list; List.concat pred_list ], e, assigned_vars)
    | ELetStar (binding_list, e) ->
        let pred_list, assigned_vars, rewrite =
          let acc : Ast.stm list list = [] in
          List.fold_left
            (fun acc (s, e) ->
              let stm_list, assigned_vars, rewrite = acc in
              let pred, e, assigned_vars, new_rewrite_entry =
                expr_to_tiny_asn assigned_vars rewrite e s
              in
              let rewrite =
                match new_rewrite_entry with
                | None -> rewrite
                | Some v -> StringMap.add s v rewrite
              in
              ([ e ] :: pred :: stm_list, assigned_vars, rewrite))
            (acc, assigned_vars, rewrite)
            binding_list
        in

        (* let pred_list = List.rev pred_list in *)
        let new_pred_list, e, assigned_vars =
          expr_to_tiny_expr assigned_vars rewrite e
        in
        (List.concat [ new_pred_list; List.concat pred_list ], e, assigned_vars)
    | EIte (c, e1, e2) ->
        let v = new_var () in
        let c_pred, c, assigned_vars =
          expr_to_tiny_expr assigned_vars rewrite c
        in
        let e1_pred, stm1, assigned_vars_then, _ =
          expr_to_tiny_asn assigned_vars rewrite e1 v
        in
        let e2_pred, stm2, assigned_vars_else, _ =
          expr_to_tiny_asn assigned_vars rewrite e2 v
        in
        let stm1 = join_stm (List.concat [ e1_pred; [ stm1 ] ]) in
        let stm2 = join_stm (List.concat [ e2_pred; [ stm2 ] ]) in
        let assigned_vars =
          StringSet.union assigned_vars
            (StringSet.inter assigned_vars_else assigned_vars_then)
        in
        let stm = Ast.Ite (Location.dummy (), c, stm1, stm2) in
        let pred = List.concat [ c_pred; [ stm ] ] in
        ( pred,
          Ast.mk_expr (Location.dummy ()) Ast.RealT (Ast.Var v),
          assigned_vars )
    | ECall (f, args) ->
        let pred, assigned_vars, tiny_args =
          List.fold_left
            (fun (pred_list, assigned_vars, args_list) e ->
              let pred, e, assigned_vars =
                expr_to_tiny_expr assigned_vars rewrite e
              in
              (List.concat [ pred_list; pred ], assigned_vars, e :: args_list))
            ([], assigned_vars, []) args
        in
        let tiny_args = List.rev tiny_args in
        ( pred,
          Ast.mk_expr (Location.dummy ()) Ast.RealT (Ast.Call (f, tiny_args)),
          assigned_vars )
    | EWhile (cond, binding_list, ret) ->
        (* Let not supported nested in cond *)
        (* cond *)
        let pred_cond, cond, assigned_vars =
          expr_to_tiny_expr assigned_vars rewrite cond
        in

        (* list variables binded in the loop *)
        let updated_vars =
          List.fold_left (fun acc (v, _, _) -> v :: acc) [] binding_list
        in
        let local_rewrite =
          List.fold_left
            (fun local_rewrite v ->
              StringMap.add v (Printf.sprintf "%s_old" v) local_rewrite)
            rewrite updated_vars
        in

        let stm_init_list, stm_body_list, assigned_vars =
          List.fold_left
            (fun (stm_init_list, stm_body_list, assigned_vars) (v, init, update)
               ->
              (* init *)
              let pred_init, stm_init, assigned_vars, _ =
                expr_to_tiny_asn assigned_vars local_rewrite init v
              in
              (* remove v from assigned vars to avoid renaimaing in loop *)
              let assigned_vars = StringSet.remove v assigned_vars in

              (* body *)
              let pred_body, stm_body, assigned_vars, _ =
                expr_to_tiny_asn assigned_vars local_rewrite update v
              in

              ( List.concat [ pred_init; [ stm_init ] ] :: stm_init_list,
                List.concat [ pred_body; [ stm_body ] ] :: stm_body_list,
                assigned_vars ))
            ([], [], assigned_vars) binding_list
        in

        let stm_init = List.concat stm_init_list in
        let stm_body = List.concat stm_body_list in
        let asn_old =
          List.fold_left
            (fun acc v ->
              Ast.Asn
                ( Location.dummy (),
                  Printf.sprintf "%s_old" v,
                  Ast.mk_expr (Location.dummy ()) RealT (Ast.Var v) )
              :: acc)
            [] updated_vars
        in

        let stm_init = List.concat [ stm_init; asn_old ] in
        let stm_body = List.concat [ stm_body; asn_old ] in
        let pred_ret, ret, assigned_vars =
          expr_to_tiny_expr assigned_vars rewrite ret
        in
        let fold_smt_list (stm_list : Ast.stm list) =
          match List.rev stm_list with
          | [] -> Ast.Nop (Location.dummy ())
          | el :: [] -> el
          | el :: tl ->
              List.fold_left
                (fun acc stm -> Ast.Seq (Location.dummy (), stm, acc))
                el tl
        in
        (* let init = fold_smt_list stm_init in *)
        let body = fold_smt_list stm_body in
        let while_loop = Ast.While (Location.dummy (), cond, body) in
        let pred =
          List.rev
            (List.concat [ stm_init; pred_cond; [ while_loop ]; pred_ret ])
        in
        (pred, ret, assigned_vars)
  and expr_to_tiny_asn (assigned_vars : StringSet.t)
      (rewrite : string StringMap.t) (e : expr) (v : string) :
      Ast.stm list * Ast.stm * StringSet.t * string option =
    (* convert the expression *)
    let pred, e, assigned_vars = expr_to_tiny_expr assigned_vars rewrite e in
    let v, assigned_vars, rewrite =
      (* rename v if already used *)
      match StringSet.find_opt v assigned_vars with
      | Some v ->
          let new_var = new_var_from_var v in
          (new_var, StringSet.add new_var assigned_vars, Some new_var)
      | None -> (v, StringSet.add v assigned_vars, None)
    in
    let stm = Ast.Asn (Location.dummy (), v, e) in
    (pred, stm, assigned_vars, rewrite)
  in
  expr_to_tiny_asn StringSet.empty StringMap.empty e v

let fpcore_to_tiny (fpcore : fpcore) : Ast.stm =
  let (FPCore (_, _, e)) = fpcore in
  let pre = pre_to_tiny fpcore in
  let pred_stm, stm, _, _ = expr_to_to_tiny_asn e "__result__" in
  let main_stm =
    List.fold_left
      (fun acc stm -> Ast.Seq (Location.dummy (), stm, acc))
      stm pred_stm
  in
  match pre with
  | None -> main_stm
  | Some stm -> Ast.Seq (Location.dummy (), stm, main_stm)

(* SSA *)

let remove_zero (stm : Subset_ast_stm.stm) : Subset_ast_stm.stm =
  let is_zero_var (v : string) (zero_var : StringSet.t) : bool =
    match StringSet.find_opt v zero_var with None -> false | Some _ -> true
  in
  let is_zero (e : Subset_ast_expr.expr) (zero_var : StringSet.t) : bool =
    match e with
    | Cst (q, _) -> Q.compare Q.zero q == 0
    | Var s -> is_zero_var s zero_var
    | Rand (_, _) -> false
    | Unop (_, v) -> is_zero_var v zero_var
    | Binop (op, v1, v2) -> (
        match op with
        | Plus | Minus -> is_zero_var v1 zero_var && is_zero_var v2 zero_var
        | Times -> is_zero_var v1 zero_var || is_zero_var v2 zero_var
        | Div -> is_zero_var v1 zero_var)
    | Call (_, _) -> false
  in
  let is_zero_asn (stm : Subset_ast_stm.stm) (zero_var : StringSet.t) :
      bool * string =
    match stm with Asn (_, v, e) -> (is_zero e zero_var, v) | _ -> (false, "")
  in
  let rec remove_zero (stm : Subset_ast_stm.stm) (zero_var : StringSet.t) :
      Subset_ast_stm.stm * StringSet.t =
    match stm with
    | Seq (l, s1, s2) ->
        let zero_s1, v1 = is_zero_asn s1 zero_var in
        if zero_s1 then
          let zero_var = StringSet.add v1 zero_var in
          (* Clean and propagate s2 *)
          (* check s2 *)
          let zero_s2, v2 = is_zero_asn s2 zero_var in
          if zero_s2 then (Subset_ast_stm.Nop l, StringSet.add v2 zero_var)
          else remove_zero s2 (StringSet.add v1 zero_var)
        else
          let s1, zero_var = remove_zero s1 zero_var in

          (* check s2 *)
          let zero_s2, v2 = is_zero_asn s2 zero_var in
          if zero_s2 then (s1, StringSet.add v2 zero_var)
          else
            let s2, zero_var = remove_zero s2 zero_var in
            (Seq (l, s1, s2), zero_var)
    | Asn (l, v, e) ->
        let e =
          match e with
          | Binop (bop, v1, v2) -> (
              match bop with
              | Plus ->
                  if is_zero_var v1 zero_var then Subset_ast_expr.Var v2
                  else if is_zero_var v2 zero_var then Subset_ast_expr.Var v1
                  else e
              | Minus ->
                  if is_zero_var v1 zero_var then
                    Subset_ast_expr.Unop (Minus, v2)
                  else if is_zero_var v2 zero_var then Subset_ast_expr.Var v1
                  else e
              | _ -> e)
          | _ -> e
        in
        (Subset_ast_stm.Asn (l, v, e), zero_var)
    | Ite (l, g, s1, s2) ->
        (* We cannot remove branch and to simplify we won't remove Ite *)
        let s1, zero_var_then = remove_zero s1 zero_var in
        let s2, zero_var_else = remove_zero s2 zero_var in
        let zero_var = StringSet.inter zero_var_then zero_var_else in
        (Ite (l, g, s1, s2), zero_var)
    | Nop l -> (Nop l, zero_var)
  in

  let stm, _ = remove_zero stm StringSet.empty in
  stm

let mk_tiny_ssa (stm : Ast.stm) : Subset_ast_stm.stm =
  let open Ast in
  let fresh_var_count = ref 0 in
  let fresh_var () : string =
    fresh_var_count := !fresh_var_count + 1;
    Printf.sprintf "_x_%d" !fresh_var_count
  in
  let get_var (e : Subset_ast_expr.expr) : string =
    match e with
    | Var v -> v
    | _ ->
        let err = "not a varaible: " ^ Subset_ast_expr.string_of_expr e in
        failwith err
  in
  let join_stm (stm_list : Subset_ast_stm.stm list) : Subset_ast_stm.stm =
    List.fold_left
      (fun acc s -> Subset_ast_stm.Seq (Location.dummy (), acc, s))
      (List.hd stm_list) (List.tl stm_list)
  in
  let rec mk_tiny_expr_ssa (e : expr) (force_var : bool) :
      Subset_ast_stm.stm list * Subset_ast_expr.expr =
    match e.expr_desc with
    | Cst (q, s) ->
        let e = Subset_ast_expr.Cst (q, s) in
        if force_var then
          let v = fresh_var () in
          ( [ Subset_ast_stm.Asn (Location.dummy (), v, e) ],
            Subset_ast_expr.Var v )
        else ([], e)
    | Var n -> ([], Subset_ast_expr.Var n)
    | Binop (bop, e1, e2) ->
        let pred1, e1 = mk_tiny_expr_ssa e1 true in
        let pred2, e2 = mk_tiny_expr_ssa e2 true in
        let bop = Subset_ast_expr.to_simple_bop bop in
        let e = Subset_ast_expr.Binop (bop, get_var e1, get_var e2) in
        if force_var then
          let pred, e =
            match e with
            | Var _ -> ([], e)
            | _ ->
                let v = fresh_var () in
                let asn = Subset_ast_stm.Asn (Location.dummy (), v, e) in
                ([ asn ], Var v)
          in
          (List.concat [ pred1; pred2; pred ], e)
        else (List.concat [ pred1; pred2 ], e)
    | Rand (q1, q2) -> ([], Subset_ast_expr.Rand (q1, q2))
    | Cond (_, _) ->
        ( (* don't touch for now *)
          [],
          Subset_ast_expr.to_simple_expr e )
    | Call (f, args) ->
        let pred, args =
          List.fold_left
            (fun (pred_list, v_list) e ->
              let pred, e = mk_tiny_expr_ssa e true in
              let v = get_var e in
              (List.concat [ pred_list; pred ], v :: v_list))
            ([], []) args
        in
        let args = List.rev args in
        let e = Subset_ast_expr.Call (f, args) in
        let v = fresh_var () in
        let asn = Subset_ast_stm.Asn (Location.dummy (), v, e) in
        let pred = List.concat [ pred; [ asn ] ] in
        (pred, Var v)
    | _ -> failwith "unknown expression"
  in
  let mk_cond_ssa (e : Ast.expr) :
      Subset_ast_stm.stm list * Subset_ast_expr.bool_expr =
    (* must be a binop or a cond *)
    match e.expr_desc with
    | Binop (bop, _, _) ->
        (* No operator supported for now *)
        failwith
          (Printf.sprintf "%s operator is not supported yet for conditions"
             (string_of_bop bop))
    | Cond (e, cmp) ->
        let cmp = Subset_ast_expr.to_simple_cmp cmp in
        let pred, e = mk_tiny_expr_ssa e true in
        let v = get_var e in
        (pred, Subset_ast_expr.BCond (v, cmp))
    | _ -> failwith "Condition must be boolean"
  in
  let rec mk_tiny_ssa (stm : stm) : Subset_ast_stm.stm list =
    match stm with
    | Seq (_, s1, s2) ->
        let pred1 = mk_tiny_ssa s1 in
        let pred2 = mk_tiny_ssa s2 in
        List.concat [ pred1; pred2 ]
    | Asn (l, v, e) ->
        let pred, e = mk_tiny_expr_ssa e false in
        let asn = Subset_ast_stm.Asn (l, v, e) in
        List.concat [ pred; [ asn ] ]
    | Ite (l, g, s1, s2) ->
        (* don't touch for now *)
        let pred, g = mk_cond_ssa g in
        let s1 = s1 |> mk_tiny_ssa |> join_stm in
        let s2 = s2 |> mk_tiny_ssa |> join_stm in
        List.concat [ pred; [ Subset_ast_stm.Ite (l, g, s1, s2) ] ]
    | Nop _ -> []
    | _ -> failwith "unsupported statement"
  in

  stm |> mk_tiny_ssa |> join_stm

(* Typing *)

let input_vars (prgm : fpcore) : StringSet.t =
  let (FPCore (arg_list, _, _)) = prgm in
  List.fold_left
    (fun acc arg ->
      let (ASymbol v) = arg in
      StringSet.add v acc)
    StringSet.empty arg_list

let rec vars_of_ast_stm (stm : Ast.stm) : StringSet.t =
  match stm with
  | Asn (_, v, _) -> StringSet.singleton v
  | Seq (_, s1, s2) | Ite (_, _, s1, s2) ->
      StringSet.union (vars_of_ast_stm s1) (vars_of_ast_stm s2)
  | _ -> StringSet.empty

let env_of_fpcore (prgm : fpcore) : Typing.typing_env =
  let stm = fpcore_to_tiny prgm in
  let vars = StringSet.union (input_vars prgm) (vars_of_ast_stm stm) in
  StringSet.fold
    (fun v env -> Typing.decl_var_type Ast.RealT env (v, Location.dummy ()))
    vars Typing.empty_env
