(*
 * TINY (Tiny Is Not Yasa (Yet Another Static Analyzer)):
 * a simple abstract interpreter for teaching purpose.
 * Copyright (C) 2012, 2014  P. Roux
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


let rand_mode = ref true
    
type bop = Plus | Minus | Times | Div | And | Or | Eq
type uop = Not
         
type cmp = Strict | Loose | Zero | NonZero
		          
type base_type = IntT
               | RealT
               | BoolT
               | MIntT of bool * int (* signed * size *)
               | FixedT of Fxp.t  (* signed * total size * fractional part *)
                             
module StringSet = Set.Make(String)
               

let pp_base_type fmt = function
  | IntT -> Format.pp_print_string fmt "int"
  | RealT -> Format.pp_print_string fmt "real"
  | BoolT -> Format.pp_print_string fmt "bool"
  | MIntT (s,t) -> Format.fprintf fmt  "%sint%i_t" (if s then "" else "u") t
  | FixedT fxp -> Format.fprintf fmt "fxp(%b,%i,%i)" fxp.sign fxp.total fxp.frac

let pp_base_type_short fmt = function
  | FixedT _ -> Format.fprintf fmt "fxp"
  | t -> pp_base_type fmt t

let is_int_type t = match t with
  | IntT | FixedT _ | MIntT _ -> true
  | _ -> false
    
module Var = struct
  type var = Name.t * base_type
  type t = var
  let pp_name fmt (n,_ (*t*)) = Format.pp_print_string fmt n
  let pp fmt (n,t) = Format.fprintf fmt "%a %s;" pp_base_type t n
  let get_name ((n, _) : t) = n
  module OrderedVar = struct type t = var let compare = compare end
                    
  module Set = struct
    include Set.Make (OrderedVar)
    let to_names s = fold (fun (v,_) acc -> Name.Set.add v acc) s Name.Set.empty
    let partition_by_type t = partition (fun (_,t') -> t = t')
    let pp fmt vs = Utils.fprintf_list ~sep:"@ " pp fmt (elements vs) 
  end
  module Map = struct
    include Map.Make (OrderedVar)
  end
end


(* Untyped AST *)

type uexpr =
  | UCst of Location.t * (Q.t * string * base_type option) (** n *)
  | UVar of Location.t * Name.t  (** v *)
  | UBinop of Location.t * bop * uexpr * uexpr    (** expr + expr,... *)
  | UUnop of Location.t * uop * uexpr  
  | URand of Location.t * base_type * (Q.t *string) * (Q.t *string)    (** rand(n, n) *)
  | UCall of Location.t * Name.t * uexpr list
  | UCond of Location.t * uexpr * cmp
  | UFxpConv of Location.t * Fxp.t * Fxp.t * uexpr 
  | UShiftLeft of Location.t * uexpr * uexpr
  | UShiftRight of Location.t * uexpr * uexpr



type ustm = 
  | UAsn of Location.t * Name.t * uexpr
  | UAsrt of Location.t * uexpr
  | USeq of Location.t * ustm * ustm
  | UIte of Location.t * uexpr * ustm * ustm
  | UWhile of Location.t * uexpr * ustm
  | UReadInput of Location.t * Name.t list
  | UReadState of Location.t * Name.t list
  | UNop of Location.t
  | UNN of Location.t * Name.t list * (string * uexpr) list * Name.t list
(* Typed Ast *) 
				
type expr_desc =
  | Cst of Q.t * string (** n *)
  | Var of Name.t  (** v *)
  | Binop of bop * expr * expr    (** expr + expr,... *)
  | Unop of uop * expr 
  | Rand of (Q.t * string) * (Q.t *string)    (** rand(n, n) *)
  | Call of Name.t * expr list
  | Cond of expr * cmp
  | FxpConv of Fxp.t * Fxp.t * expr 
  | ShiftLeft of expr * expr
  | ShiftRight of expr * expr

 and expr =
   { expr_desc: expr_desc;
     expr_loc: Location.t;
     expr_type: base_type
   }


type stm = 
  | Asn of Location.t * Name.t * expr
  | Asrt of Location.t * expr
  | Seq of Location.t * stm * stm
  | Ite of Location.t * expr * stm * stm
  | While of Location.t * expr * stm
  | ReadInput of Location.t * Name.t list
  | ReadState of Location.t * Name.t list
  | Nop of Location.t
  | NN of Location.t * Name.t list * (string * Nn_types.act_t) list * Name.t list
  | Nde of Location.t * Name.t list * Name.t * expr list (* Could be created only from lustre codes *)


         		    
let string_of_bop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | And -> "&&"
  | Or -> "||"
  | Eq -> "==" (* should on;y be used with non numerical arguments. Otherwise, should be comparaison instead of binop *)

let string_of_uop = function
  | Not -> "!"
         
let string_of_cmp =
  function Loose -> ">="
         | Strict -> ">"
         | Zero -> "=="
         | NonZero -> "!="

let fprint_expr ff e =
  let prior_bop = function
    | Times | Div -> 2
    | Plus | Minus -> 1
    | Eq -> 0
    | And | Or -> -1
  in
  let rec fprint_expr_prior _ (* UNUSED: prior *) ff e = match e.expr_desc with
    | Cst (c,s) -> (
      match e.expr_type with
      | MIntT _ | IntT | RealT | BoolT ->
        Format.fprintf ff "%s" s
      | FixedT fxp ->
        Format.fprintf ff "%i /* %s */"
          (Fxp.from_rat fxp c)
          s
    )
    | Var n -> Format.fprintf ff "%s" n
    | Binop (bop, e1, e2) -> 
       (*(if prior_bop bop < prior then *)
          Format.fprintf ff "(@[%a@ %s %a@])"
       (*else*)
            (* Format.fprintf ff "%a@ %s %a")*)
        (fprint_expr_prior (prior_bop bop)) e1
        (string_of_bop bop)
        (fprint_expr_prior (prior_bop bop + 1)) e2
    | Unop (uop, e) ->
       Format.fprintf ff "(%s@ %a)"
         (string_of_uop uop)
         (fprint_expr_prior 0) e
    | Rand (c1, c2) ->
      if e.expr_type = BoolT then
        if !rand_mode then
         Format.fprintf ff "?" 
        else
          Format.fprintf ff "rand_bool()" 
      else (
        let pp_cst fmt (q,s) =
          match e.expr_type with
          | IntT | MIntT _ | RealT | BoolT -> Format.pp_print_string fmt s
          | FixedT fxp ->
            Format.fprintf fmt "%i /* %s */"
              (Fxp.from_rat fxp q)
              s
        in

          
        if !rand_mode then
          Format.fprintf ff "[@[%a,@ %a@]]"
            pp_cst c1
            pp_cst c2 
        else
          Format.fprintf ff "rand_%a(@[%a,@ %a@])"
            pp_base_type_short e.expr_type
            pp_cst c1
            pp_cst c2
      )
    | Call(n, el) ->
      Format.fprintf ff "@[<h>%s(%a)@]"
	n
	(Utils.fprintf_list ~sep:", " (fprint_expr_prior 0)) el
    | Cond(e, cmp) ->
      Format.fprintf ff "@[(%a)@ %s %s@]"
	(fprint_expr_prior 0) e (string_of_cmp cmp) (if e.expr_type = RealT then "0." else "0")
    | FxpConv(new_fxp, old_fxp , e) ->
      Format.fprintf ff "([conv %a -> %a]%a)"
        Fxp.pp old_fxp
        Fxp.pp new_fxp
        (fprint_expr_prior 0) e
    | ShiftLeft (e, dec) ->
      (* assert (dec > 0); *)
      Format.fprintf ff "@[(%a << %a)@]"
        (fprint_expr_prior 0) e
        (fprint_expr_prior 0) dec
    | ShiftRight (e, dec) ->
      (* assert (dec > 0); *)
      Format.fprintf ff "@[(%a >> %a)@]"
        (fprint_expr_prior 0) e
        (fprint_expr_prior 0) dec
    
  in
  fprint_expr_prior 0 ff e
  
let rec fprint_stm ff = function
  | Asn (_, n, e) -> Format.fprintf ff "%s = @[%a@];" n fprint_expr e
  | Asrt (_, g) -> Format.fprintf ff "assert(%a);" fprint_expr g
  | Seq (_, s1, s2) ->
     Format.fprintf ff "@[<v>%a@ %a@]" fprint_stm s1 fprint_stm s2
  | Ite (_, g, s1, s2) ->
     Format.fprintf ff "@[<v>@[<v 2>if (%a) {@ %a@]@ @[<v 2>} else {@ %a@]@ }@]"
                    fprint_expr g fprint_stm s1 fprint_stm s2
  | While (_, g, s) ->
     Format.fprintf ff "@[<v>@[<v 2>while (%a) {@ %a@]@ }@]"
       fprint_expr g fprint_stm s
  | ReadInput (_, lv) ->
    Format.fprintf ff "read_input(%a);"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         Format.pp_print_string) lv
  | ReadState (_, lv) ->
    Format.fprintf ff "read_input(%a);"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         Format.pp_print_string) lv
  | NN ( _, ins, layers, outs ) ->
     Format.fprintf ff "@[<v>@[nn((%a), (%a), (%a))@]@]"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
              Format.pp_print_string) ins
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
              (fun fmt (lfile,act) -> Format.fprintf fmt "\"%s:%a\"" lfile Nn_types.pp_act act)) layers
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
              Format.pp_print_string) outs
  | Nop _ -> ()
  | Nde(_, ln, name, le) -> 
    Format.fprintf ff "( %a ) = %s (%a);"
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        Format.pp_print_string) ln name
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        fprint_expr) le

let rec cmp_expr e1 e2 = match e1.expr_desc, e2.expr_desc with
  | Cst (n1,_), Cst (n2,_) -> Q.compare n1 n2
  | Cst _, _ -> -1
  | Var _, Cst _ -> 1
  | Var n1, Var n2 -> compare n1 n2
  | Var _, _ -> -1
  | Unop _, (Cst _ | Var _) -> 1 
  | Unop (op1, e1), Unop (op2, e2) ->
     let r = compare op1 op2 in
     if r <> 0 then r else cmp_expr e1 e2
  | Unop _, _ -> -1
  | Binop _, (Cst _ | Var _ | Unop _) -> 1
  | Binop (op1, e11, e12), Binop (op2, e21, e22) ->
     let r = compare op1 op2 in
     if r <> 0 then r
     else
       let r = cmp_expr e11 e21 in
       if r <> 0 then r else cmp_expr e12 e22
  | Binop _, _ -> -1
  | Rand _, (Cst _ | Var _ | Unop _ | Binop _) -> 1
  | Rand ((n11, _), (n12, _)), Rand ((n21, _), (n22, _)) ->
     let r = Q.compare n11 n21 in
     if r <> 0 then r
     else Q.compare n12 n22
  | Rand _, _ -> -1
  | Cond _, (Rand _ | Cst _ | Var _ | Unop _ | Binop _) -> 1
  | Cond (e1, _ (*cmp1*)) , Cond (e2, _(* cmp2 *)) -> cmp_expr e1 e2
  | Cond _, _ -> -1
  | Call _, (Rand _ | Cst _ | Var _ | Unop _ | Binop _ | Cond _) -> 1
  | Call (_(* f1 *), args1), Call (_(* f2 *), args2) -> compare args1 args2
  | Call _, _ -> -1
  | FxpConv _, (Rand _ | Cst _ | Var _ | Unop _ | Binop _ | Cond _ | Call _) -> 1
  | FxpConv (fxp_o, fxp_n, e), FxpConv (fxp_o', fxp_n', e') ->
    let r = compare fxp_o fxp_o' in
    if r <> 0 then r else
      let r = compare fxp_n fxp_n' in
      if r <> 0 then r else
        cmp_expr e e'
  | FxpConv _, _ -> -1
  | ShiftLeft _, (Rand _ | Cst _ | Var _ | Unop _ | Binop _ | Cond _ | Call _ | FxpConv _ ) -> 1
  | ShiftLeft (e1, d1), ShiftLeft (e2, d2) ->
    let r = cmp_expr e1 e2 in
    if r <> 0 then r
    else
      compare d1 d2
  | ShiftLeft _, _ -> -1
  | ShiftRight _, (Rand _ | Cst _ | Var _ | Unop _ | Binop _ | Cond _ | Call _ | FxpConv _ | ShiftLeft _ ) -> 1
  | ShiftRight (e1, d1), ShiftRight (e2, d2) ->
    let r = cmp_expr e1 e2 in
    if r <> 0 then r
    else
      compare d1 d2
  (* | ShiftRight _, _ -> -1 *)
  

module OrderedExpr = struct
  type t = expr
  let compare = cmp_expr
end

module ExprMap = Map.Make (OrderedExpr)

let loc_of_expr e = e.expr_loc

(* let loc_of_guard (e, _) = e.expr_loc *)

let loc_of_stm = function
  | Asn (l, _, _) | Asrt (l, _) | Seq (l, _, _)
  | Ite (l, _, _, _) | While (l, _, _) | ReadInput (l, _) | ReadState (l, _) | Nop l 
  | Nde (l,_,_,_) -> l
  | NN (l, _, _ ,_ ) -> l
  

let rec vars_of_expr s e = match e.expr_desc with
  | Cst _ -> s
  | Var n -> Name.Set.add n s
  | Unop (_, e1) -> vars_of_expr s e1
  | Binop ( _, e1, e2) -> vars_of_expr (vars_of_expr s e1) e2
  | Rand _ -> s
  | Call (_, el) -> List.fold_left vars_of_expr s el
  | Cond (e, _) 
  | FxpConv(_,_,e) 
  | ShiftLeft(e, _)
  | ShiftRight(e, _) -> vars_of_expr s e
let mk_expr l t d = { expr_type = t; expr_loc = l; expr_desc = d }
let mk_cond l d sl = { expr_type = BoolT; expr_loc = l; expr_desc = Cond (d, sl) }
let mk_cst_expr l t (v,v_s) = mk_expr l t (Cst (v,v_s))
let is_zero e = match e.expr_desc with Cst(q,_) -> Q.equal q Q.zero | _ -> false
                                                                         
let neg_cmp_op sl =
  match sl with
  | Loose -> Strict
  | Strict -> Loose
  | Zero -> NonZero
  | NonZero -> Zero

(* Identify expression 0 - e and replace them by e, otherwise
   build 0 - e *)        
let opposite e =
  match e.expr_desc with
  | Binop (Minus, arg1, e') when is_zero arg1 -> e'
  | _ -> 
     mk_expr 
       e.expr_loc
       e.expr_type 
       (Binop (Minus, mk_cst_expr e.expr_loc e.expr_type (Q.zero, if e.expr_type = RealT then "0." else "0"), e))

let sub e1 e2 =
  if e1.expr_type != e2.expr_type then assert false;                                     
  mk_expr e1.expr_loc e1.expr_type (Binop (Minus, e1, e2))
  
let rec neg_guard e = 
  match e.expr_desc with
  | Cond (e, sl) -> 
     let minus_e = opposite e in
     mk_expr e.expr_loc BoolT (Cond (minus_e, neg_cmp_op sl))
  | Cst (_(* c *),"true") -> mk_expr e.expr_loc BoolT (Cst (Q.of_int 0, "false"))
  | Cst (_(* c *),"false") -> mk_expr e.expr_loc BoolT (Cst (Q.of_int 1, "true"))
  | Var _(* n *) -> (* n is necessarily a boolean variable. n is true when n = 1 (ie non Zero). 
                Then the negation of n is n = 0 *)
     (*mk_expr e.expr_loc BoolT (Cond (e, Zero)) *)
     mk_expr e.expr_loc BoolT (Unop (Not, e)) (* TODO: verifier *)
    
  | Unop (Not, e') -> e'
  | Binop (And, e1, e2) -> mk_expr e.expr_loc BoolT (Binop (Or, neg_guard e1, neg_guard e2))
  | Binop (Or, e1, e2) -> mk_expr e.expr_loc BoolT (Binop (And, neg_guard e1, neg_guard e2))
  | Binop (Eq, _, _) -> mk_expr e.expr_loc BoolT (Unop (Not, e))
  | _ -> Format.eprintf "Unable to negate the expression %a used in guard.@.@?" fprint_expr e; assert false


(*
  else if e.expr_tyfunction
  | Binop (l, Minus, CstInt (l', n), e2) when n > min_int ->  (* avoid underflows *)
    Binop (l, Minus, e2, Int (l', n - 1))
  | Binop (l, Minus, e1, Int (l', n)) when n < max_int ->  (* avoid overflows *)
    Binop (l, Minus, Int (l', n + 1), e1)
  | Binop (l, Minus, e1, e2) ->
    Binop (l, Minus, Binop (l, Plus, e2, Int (l, 1)), e1)
  | e -> let l = loc_of_expr e in Binop (l, Minus, Int (l, 1), e)
*)

let vars_of_stm stm =
  (* let vars_of_guards s (e, _) = vars_of_expr s e in  *)
  let rec vars_of_stm s = function
    | Asn (_, n, e) -> vars_of_expr (Name.Set.add n s) e
    | Asrt (_, g) -> vars_of_expr s g
    | Seq (_, s1, s2) -> vars_of_stm (vars_of_stm s s1) s2
    | Ite (_, g, s1, s2) ->
      vars_of_stm (vars_of_stm (vars_of_expr s g) s1) s2
    | While (_, g, st) ->
       vars_of_stm (vars_of_expr s g) st
    | ReadInput (_, lv) ->
      List.fold_left (fun s v -> Name.Set.add v s) s lv
    | ReadState (_, lv) ->
      List.fold_left (fun s v -> Name.Set.add v s) s lv
    | Nop _ -> Name.Set.empty
    | Nde (_,ln,_ (* UNUSED: name *),le) -> List.fold_left vars_of_expr (List.fold_left (fun s n -> Name.Set.add n s) s ln) le
    | NN _ -> assert false (* should have been processed *)
  in
  vars_of_stm Name.Set.empty stm

let vars_of_expr = vars_of_expr Name.Set.empty

let get_main_while_loc stm =
  let rec aux stm =
    match stm with
    | Asn _ | Asrt _ | Ite _  | Nop _ | Nde _ | NN _ | ReadInput _ | ReadState _ -> None
    | While (_, g, _) -> Some (Location.beg_p (loc_of_expr g))
    | Seq (_, s1, s2) -> (
      match aux s1 with
      | Some l -> Some l
      | None -> aux s2
    )
  in
  match aux stm with
  | Some l -> l
  | None -> raise Not_found

let get_fixed_formats vars =
  Var.Set.fold (fun (_,t) accu -> match t with FixedT fxp -> Fxp.Set.add fxp accu | _ -> accu) vars Fxp.Set.empty
let get_assigned_vars (stm : stm) : Var.Set.t =
  let rec get_assigned_vars (stm : stm) (acc : Var.Set.t) : Var.Set.t =
    match stm with
    | Seq (_, stm1, stm2) | Ite (_, _, stm1, stm2) ->
        let vars1 = get_assigned_vars stm1 acc in
        get_assigned_vars stm2 vars1
    | Asn (_, z, e) ->
        Var.Set.add (z, e.expr_type) acc
        (* match Var.Set.find_first_opt (fun (v, t) -> v = z) acc with
        | None -> Var.Set.add  
        | Some _ -> acc) *)
    | While (_, _, stm) -> get_assigned_vars stm acc
    | Nop _ -> acc
    | _ -> failwith "get_vars not implemented"
  in
  get_assigned_vars stm Var.Set.empty

let copy_stm (stm : stm) : stm =
  match stm with
  | Asn (l, v, e) -> Asn (Location.incr_loc l, v, e)
  | Asrt (l, e) -> Asrt (Location.incr_loc l, e)
  | Seq (l, s1, s2) -> Seq (Location.incr_loc l, s1, s2)
  | Ite (l, cond, s1, s2) -> Ite (Location.incr_loc l, cond, s1, s2)
  | While (l, cond, stm) -> While (Location.incr_loc l, cond, stm)
  | ReadInput (l, name_list) -> ReadInput (Location.incr_loc l, name_list)
  | ReadState (l, name_list) -> ReadState (Location.incr_loc l, name_list)
  | Nop l -> Nop (Location.incr_loc l)
  | _ -> failwith "copy_stm not implemented"

let unroll_while ?(use_branch : bool = false) ?(rename : bool = false) (n : int)
    (stm : stm) =
  let module StringMap = Map.Make (String) in
  let loop_count = ref 0 in
  let ite_count = ref 0 in
  let rec rename_var_expr (rename : string -> int -> string) (cond : expr)
      (i : int) =
    let mk_expr = mk_expr cond.expr_loc cond.expr_type in
    let rename_var_expr = rename_var_expr rename in
    match cond.expr_desc with
    | Cst (_, _) -> cond
    | Var v -> mk_expr (Var (rename v i))
    | Unop (op, e) -> mk_expr (Unop (op, rename_var_expr e i))
    | Binop (bop, e1, e2) ->
        mk_expr (Binop (bop, rename_var_expr e1 i, rename_var_expr e2 i))
    | Rand (_, _) -> cond
    | Call (f, args) ->
        mk_expr (Call (f, List.map (fun e -> rename_var_expr e i) args))
    | Cond (e, cmp) -> mk_expr (Cond (rename_var_expr e i, cmp))
    | FxpConv (a, b, e) -> mk_expr (FxpConv (a, b, rename_var_expr e i))
    | ShiftLeft (e1, e2) ->
        let e1 = rename_var_expr e1 i in
        let e2 = rename_var_expr e2 i in
        mk_expr (ShiftLeft (e1, e2))
    | ShiftRight (e1, e2) ->
        let e1 = rename_var_expr e1 i in
        let e2 = rename_var_expr e2 i in
        mk_expr (ShiftLeft (e1, e2))
  in
  let rec rename_var_stm (rename : StringSet.t -> string -> int -> string)
      (i : int) (assigned_vars : StringSet.t) (stm : stm) : stm * StringSet.t =
    let rename_var_stm_h = rename_var_stm rename i in
    match stm with
    | Asn (l, var, e) ->
        let new_var = rename assigned_vars var (i + 1) in
        let e = rename_var_expr (rename assigned_vars) e i in
        let assigned_vars = StringSet.add var assigned_vars in
        (Asn (l, new_var, e), assigned_vars)
    | Asrt (l, e) ->
        let e = rename_var_expr (rename assigned_vars) e i in
        (Asrt (l, e), assigned_vars)
    | Seq (l, s1, s2) ->
        let s1, assigned_vars = rename_var_stm_h assigned_vars s1 in
        let s2, assigned_vars = rename_var_stm_h assigned_vars s2 in
        (Seq (l, s1, s2), assigned_vars)
    | Ite (l, cond, s1, s2) ->
        let cond = rename_var_expr (rename assigned_vars) cond i in
        let s1, assigned_vars_then = rename_var_stm_h assigned_vars s1 in
        let s2, assigned_vars_else = rename_var_stm_h assigned_vars s2 in
        let assigned_vars =
          StringSet.union assigned_vars_then assigned_vars_else
        in
        (Ite (l, cond, s1, s2), assigned_vars)
    | While (l, cond, stm) ->
        let cond = rename_var_expr (rename assigned_vars) cond i in
        let stm, assigned_vars = rename_var_stm_h assigned_vars stm in
        (While (l, cond, stm), assigned_vars)
    | ReadInput (l, name_list) ->
        let name_list =
          List.map (fun v -> rename assigned_vars v i) name_list
        in
        (ReadInput (l, name_list), assigned_vars)
    | ReadState (l, name_list) ->
        let name_list =
          List.map (fun v -> rename assigned_vars v i) name_list
        in
        (ReadState (l, name_list), assigned_vars)
    | Nop _ -> (stm, assigned_vars)
    | _ -> failwith "rename var stm not implemented"
  in
  let rename_var_loop_no_guard (var : string) (i : int) : string =
    if rename then Printf.sprintf "%s_%d_%d" var !loop_count i else var
  in
  let rename_var_loop (vars : StringSet.t) (var : string) (i : int) : string =
    if rename then
      match StringSet.find_opt var vars with
      | None -> var
      | Some _ -> Printf.sprintf "%s_%d_%d" var !loop_count i
    else var
  in
  let rename_var_stm_in_loop (stm : stm) (i : int) (vars : StringSet.t) : stm =
    let rename (assigned_vars : StringSet.t) (var : string) (i : int) : string =
      match StringSet.find_opt var assigned_vars with
      | None -> rename_var_loop vars var (i - 1)
      | Some _ -> rename_var_loop vars var i
    in
    let stm, _ = rename_var_stm rename i StringSet.empty stm in
    stm
  in
  let rename_stm_varmap (stm : stm) (varmap : string StringMap.t) : stm =
    let rename (_ : StringSet.t) (var : string) (_ : int) : string =
      match StringMap.find_opt var varmap with
      | None -> var
      | Some new_var -> new_var
    in
    let stm, _ = rename_var_stm rename 0 StringSet.empty stm in
    stm
  in
  let rename_expr_varmap (expr : expr) (varmap : string StringMap.t) : expr =
    let rename (v : string) (_ : int) =
      match StringMap.find_opt v varmap with None -> v | Some new_v -> new_v
    in
    rename_var_expr rename expr 0
  in
  let merge_varmap (prev : string StringMap.t) (next : string StringMap.t) :
      string StringMap.t =
    StringMap.union (fun _ _ n -> Some n) prev next
  in
  let asn_var (l : Location.t) (vars : Var.Set.t) (i : int) : stm =
    let asn =
      Var.Set.fold
        (fun (v, t) acc ->
          Asn
            ( Location.incr_loc l,
              v,
              mk_expr (Location.incr_loc l) t
                (Var (rename_var_loop_no_guard v i)) )
          :: acc)
        vars []
    in
    match asn with
    | [] -> Nop (Location.incr_loc l)
    | el :: tl ->
        List.fold_left
          (fun acc stm -> Seq (Location.incr_loc l, acc, stm))
          el tl
  in
  let rec unroll (l : Location.t) (cond : expr) (stm : stm) (vars : StringSet.t)
      (vars_type : Var.Set.t) (i : int) (acc : stm) : stm =
    if i = 0 then acc
    else
      let stm_i = rename_var_stm_in_loop stm i vars in
      let acc =
        if use_branch then
          let cond_i = rename_var_expr (rename_var_loop vars) cond (i - 1) in
          let stm_i =
            if i = n then Seq (l, stm_i, asn_var l vars_type i) else stm_i
          in
          let stm_else = asn_var l vars_type (i - 1) in
          Ite (l, cond_i, Seq (l, stm_i, acc), stm_else)
        else
          Seq
            ( l,
              (if i = n then
                 Seq (Location.incr_loc l, stm_i, asn_var l vars_type n)
               else stm_i),
              acc )
      in
      unroll l cond stm vars vars_type (i - 1) acc
  in
  let rec unroll_while (stm : stm) : stm * string StringMap.t =
    match stm with
    | Seq (l, stm1, stm2) ->
        let stm1, rewrite1 = unroll_while stm1 in
        let stm2, rewrite2 = unroll_while stm2 in
        let stm1 = rename_stm_varmap stm1 rewrite2 in
        (Seq (l, stm1, stm2), merge_varmap rewrite2 rewrite1)
    | Ite (l, cond, stm1, stm2) ->
        let stm1, rewrite1 = unroll_while stm1 in
        let stm2, rewrite2 = unroll_while stm2 in
        (* get variables affected by both then and else *)
        let common_var =
          StringMap.merge
            (fun k v1 v2 ->
              match (v1, v2) with
              | Some v1, Some v2 when v1 != v2 -> Some k
              | _ -> None)
            rewrite1 rewrite2
        in
        let rewrite =
          StringMap.union
            (fun _ t e -> if t = e then Some t else None)
            rewrite1 rewrite2
        in
        if StringMap.is_empty common_var then
          (Ite (l, cond, stm1, stm2), rewrite)
        else (
          ite_count := !ite_count + 1;
          let rewrite_then, rewrite_else, rewrite =
            StringMap.fold
              (fun v _ (r_then, r_else, r) ->
                let new_v = Printf.sprintf "%s_ite_%d" v !ite_count in
                let x1 = StringMap.find v rewrite1 in
                let x2 = StringMap.find v rewrite2 in
                ( StringMap.add x1 new_v r_then,
                  StringMap.add x2 new_v r_else,
                  StringMap.add v new_v r ))
              common_var
              (StringMap.empty, StringMap.empty, StringMap.empty)
          in

          let stm1 = rename_stm_varmap stm1 rewrite_then in
          let stm2 = rename_stm_varmap stm2 rewrite_else in
          let cond = rename_expr_varmap cond rewrite in
          (Ite (l, cond, stm1, stm2), rewrite))
    | While (l, cond, stm) ->
        print_endline "while found";
        let vars_type = get_assigned_vars stm in
        let vars =
          Var.Set.fold
            (fun (v, _) acc -> StringSet.add v acc)
            vars_type StringSet.empty
        in
        let ret =
          ( unroll l cond stm vars vars_type n (Nop (Location.incr_loc l)),
            StringSet.fold
              (fun v acc -> StringMap.add v (rename_var_loop_no_guard v 0) acc)
              vars StringMap.empty )
        in
        loop_count := !loop_count + 1;
        ret
    | _ -> (stm, StringMap.empty)
  in
  let stm, _ = unroll_while stm in
  stm
