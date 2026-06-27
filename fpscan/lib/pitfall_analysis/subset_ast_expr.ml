(* Define a simplified version of the Tiny ast *)

module StringSet = Stdlib.Set.Make (String)

(* Expressions *)

(* Binary operators *)
type bop = Plus | Minus | Times | Div
type bbop = Eq

(* Unary operators *)
type unop = Minus

(* Comparaison operators *)
type cmp = Strict | Loose | Zero | NonZero

(* expression *)
type expr =
  | Cst of Q.t * string  (** n *)
  | Binop of bop * string * string  (** expr + expr,... *)
  | Unop of unop * string
  | Var of string
  | Rand of (Q.t * string) * (Q.t * string)
  | Call of string * string list

type bool_expr = BCond of string * cmp

(* Conversion functions *)

(* convert an binary operator *)
let to_simple_bop (bop : Ast.bop) =
  match bop with
  | Plus -> Plus
  | Minus -> Minus
  | Times -> Times
  | Div -> Div
  | _ -> failwith "unsoprted operator"

let to_simple_unop (bop : Ast.bop) : unop =
  match bop with Minus -> Minus | _ -> failwith "unsoported unary operator"

let to_simple_cmp (cmp : Ast.cmp) =
  match cmp with
  | Strict -> Strict
  | Loose -> Loose
  | Zero -> Zero
  | NonZero -> NonZero

(* convert an expression *)
let to_simple_expr (expr : Ast.expr) =
  match expr.expr_desc with
  | Cst (v, vs) -> Cst (v, vs)
  | Var n -> Var n
  | Binop (bop, e1, e2) -> (
      match (e1.expr_desc, e2.expr_desc) with
      | Var n1, Var n2 -> Binop (to_simple_bop bop, n1, n2)
      (* Unary Minus *)
      | Cst (_, s), Var n2 when s = "0" -> Unop (to_simple_unop bop, n2)
      | _ -> failwith "One of the operand is not a variable")
  | Rand (l, r) -> Rand (l, r)
  | Ast.Call (f, e_list) ->
      Call
        ( f,
          List.fold_left
            (fun acc (e : Ast.expr) ->
              match e.expr_desc with
              | Var v -> v :: acc
              | _ -> failwith "function argument must be variable")
            [] e_list )
  | _ ->
      Printf.eprintf "Unsoported expression ";
      Ast.fprint_expr Format.err_formatter expr;
      failwith "Unsoported expression"

let mk_var (l : Location.t) (t : Ast.base_type) (v : string) : Ast.expr =
  Ast.mk_expr l t (Ast.Var v)

let bool_expr_to_tiny_expr (expr : bool_expr) (l : Location.t) : Ast.expr =
  match expr with
  | BCond (v, cmp) ->
      let cmp =
        match cmp with
        | Strict -> Ast.Strict
        | Loose -> Ast.Loose
        | Zero -> Ast.Zero
        | NonZero -> Ast.NonZero
      in
      let v = mk_var l Ast.RealT v in
      let e = Ast.Cond (v, cmp) in
      Ast.mk_expr l Ast.BoolT e

let expr_to_tiny_expr (expr : expr) (l : Location.t) (t : Ast.base_type) :
    Ast.expr =
  let e =
    match expr with
    | Cst (q, s) -> Ast.Cst (q, s)
    | Var v -> Ast.Var v
    | Unop (_, v) ->
        let z = Ast.mk_expr l t (Cst (Q.zero, "0")) in
        let v = mk_var l t v in
        Ast.Binop (Ast.Minus, z, v)
    | Rand (q1, q2) -> Ast.Rand (q1, q2)
    | Binop (bop, v1, v2) ->
        let bop =
          match bop with
          | Plus -> Ast.Plus
          | Minus -> Ast.Minus
          | Times -> Ast.Times
          | Div -> Ast.Div
        in
        let v1 = mk_var l t v1 in
        let v2 = mk_var l t v2 in
        Ast.Binop (bop, v1, v2)
    | Call (f, args) ->
        let args =
          List.fold_left
            (fun acc v -> Ast.mk_expr l t (Ast.Var v) :: acc)
            [] args
        in
        let args = List.rev args in
        Ast.Call (f, args)
  in
  Ast.mk_expr l t e

(* string_of_* functions *)
let string_of_bop (bop : bop) : string =
  match bop with Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"

let string_of_unop = function Minus -> "-"

let string_of_cmp (cmp : cmp) =
  match cmp with
  | Strict -> " > "
  | Loose -> " >= "
  | Zero -> " = "
  | NonZero -> " != "

let string_of_expr (e : expr) : string =
  match e with
  | Cst (_, s) -> s
  | Var s -> s
  | Rand ((_, s1), (_, s2)) -> Printf.sprintf "Rand (%s, %s)" s1 s2
  | Binop (bop, v1, v2) -> Printf.sprintf "%s %s %s" v1 (string_of_bop bop) v2
  | Unop (unop, v) -> Printf.sprintf "%s %s" (string_of_unop unop) v
  | Call (f, args) ->
      Printf.sprintf "%s (%s)" f
        (List.fold_left
           (fun acc v -> acc ^ ", " ^ v)
           (List.hd args) (List.tl args))

let string_of_bool_expr (expr : bool_expr) =
  match expr with
  | BCond (v, cmp) -> Printf.sprintf "%s %s 0" v (string_of_cmp cmp)

(* Helpers *)

let vars_of_expr (e : expr) : StringSet.t =
  match e with
  | Cst (_, _) -> StringSet.empty
  | Var s | Unop (_, s) -> StringSet.singleton s
  | Rand (_, _) -> StringSet.empty
  | Binop (_, v1, v2) -> StringSet.of_list [ v1; v2 ]
  | Call (_, args) -> StringSet.of_list args

let expr_is_rand (expr : expr) =
  match expr with Rand (_, _) -> true | _ -> false

let fprint_expr_to_C (fmt : Format.formatter) (expr : expr) : unit =
  match expr with
  | Cst (q, _) -> Format.fprintf fmt "%s" (q |> Q.to_float |> string_of_float)
  | Var s -> Format.fprintf fmt "%s" s
  | Unop (op, s) -> Format.fprintf fmt "%s %s" (string_of_unop op) s
  | Rand (_, _) -> failwith "range cannot be compiled"
  | Binop (bop, v1, v2) ->
      Format.fprintf fmt "%s %s %s" v1 (string_of_bop bop) v2
  | Call (f, args) ->
      let rec fprint_list f l =
        match l with
        | [ el1; el2 ] -> Format.fprintf f "%s, %s" el1 el2
        | el :: tl -> Format.fprintf f "%s %a" el fprint_list tl
        | [] -> ()
      in
      Format.fprintf fmt "(%s (%a))" f fprint_list args

let fprint_boolexpr_to_C (fmt : Format.formatter) (expr : bool_expr) : unit =
  match expr with
  | BCond (v, cmp) -> Format.fprintf fmt "%s %s 0" v (string_of_cmp cmp)
