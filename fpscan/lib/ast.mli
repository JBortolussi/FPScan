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

(** Type of abstract syntax trees, printing and various functions on them. *)

(** {2 Type of Abstract Syntax Trees} *)

type bop = Plus | Minus | Times | Div | And | Or | Eq
type uop = Not

(* type cmp = Le | Lt *)

type cmp = Strict | Loose | Zero | NonZero
		       
type base_type = IntT | RealT | BoolT
               | MIntT of bool * int (* signed * size *)
               | FixedT of Fxp.t  (* signed * total size * fractional part *)

val pp_base_type: Format.formatter -> base_type -> unit				


module Var : sig
  type t = Name.t * base_type
  val pp_name: Format.formatter -> t -> unit
  val pp: Format.formatter -> t -> unit
  val get_name : t -> string
  module Set: sig
    include Set.S
    val to_names: t -> Name.Set.t
    val partition_by_type: base_type -> t -> t * t
    val pp: Format.formatter -> t -> unit
  end
  module Map: sig
    include Map.S
  end
  module OrderedVar : sig
    type t = Name.t * base_type
    val compare : t -> t -> int
  end
end with type t = Name.t * base_type and
         type Set.elt = Name.t * base_type and
         type Map.key = Name.t * base_type 


(* Untyped AST *)

type uexpr =
  | UCst of Location.t * (Q.t * string * base_type option) (** n *)
  | UVar of Location.t  * Name.t  (** v *)
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
  | ShiftLeft of  expr * expr
  | ShiftRight of expr * expr


 and expr =
   { expr_desc: expr_desc;
     expr_loc: Location.t;
     expr_type: base_type
   }

(*type guard = expr * cmp  (** expr >= 0 (or >) *)*)

type stm = 
  | Asn of Location.t * Name.t * expr  (** v = expr; *)
  | Asrt of Location.t * expr (* guard *)  (** assert(guard); *)
  | Seq of Location.t * stm * stm  (** stm stm *)
  | Ite of Location.t * expr(* guard *) * stm * stm   (** if (guard) \{ stm \} else \{ stm \} *)
  | While of Location.t * expr (* guard *) * stm  (** while (guard) \{ stm \} *)
  | ReadInput of Location.t * Name.t list
  | ReadState of Location.t * Name.t list
  | Nop of Location.t
  | NN of Location.t * Name.t list * (string * Nn_types.act_t) list * Name.t list
  | Nde of Location.t * Name.t list * Name.t * expr list (** (v1, v2, v3) = node (a1, a2)) *)
                 
(** {3 Maps of expressions} *)

(** Total ordering function over expressions (ignoring locations). *)
val cmp_expr : expr -> expr -> int

module ExprMap : Map.S with type key = expr  (** Maps from exprs. *)

(** {2 Various Utility Functions} *)

(** [loc_of_expr e] returns location contained in expression [e]. *)
val loc_of_expr : expr -> Location.t

(* (\** [loc_of_guard g] returns location contained in guard [g]. *\) *)
(* val loc_of_guard : guard -> Location.t *)

(** [loc_of_stm s] returns location contained in statement [s]. *)
val loc_of_stm : stm -> Location.t

(** [vars_of_expr e] returns the set of variables appearing
    in expression [e]. *)
val vars_of_expr : expr -> Name.Set.t

val unroll_while : ?use_branch:bool -> ?rename:bool -> int -> stm -> stm

val neg_cmp_op: cmp -> cmp
  
(** [opposte e] returns the expression [-e] *)
val opposite : expr -> expr

(** [sub e1 e2] = e1 - e2 *)
val sub: expr -> expr -> expr
  
(** [neg_guard e sl] returns an expression [e'] such that guard e' >= 0 is
    equivalent to e > 0 is sl = Strict (resp >= if Loose). It depends on e
    type. *)
val neg_guard : expr -> expr

(** [vars_of_stm s] returns the set of variables appearing
    in statement [s]. *)
val vars_of_stm : stm -> Name.Set.t

val mk_expr : Location.t -> base_type -> expr_desc -> expr
val mk_cond : Location.t -> expr -> cmp  -> expr
val mk_cst_expr : Location.t -> base_type -> Q.t * string -> expr
(** {2 Printing Functions} *)

val string_of_bop : bop -> string
val string_of_uop : uop -> string

val string_of_cmp : cmp -> string

val fprint_expr : Format.formatter -> expr -> unit

(* val fprint_guard : Format.formatter -> guard -> unit *)

val fprint_stm : Format.formatter -> stm -> unit

val get_main_while_loc: stm -> Location.t

val get_fixed_formats: Var.Set.t -> Fxp.Set.t

val is_int_type : base_type -> bool

val get_assigned_vars : stm -> Var.Set.t