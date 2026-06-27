%{
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

let loc start_pos end_pos = Location.location_of_positions start_pos end_pos

let build l bop e1 e2 = (* match bop, Q.compare e2 (Q.of_int 0) with *)
  (* | Ast.Minus, 0 -> e1 *)
  (* | _ ->  *)Ast.UBinop (l, bop, e1, e2)
	     
let build_op_eq l v bop e = Ast.UAsn (l, v, build l bop (Ast.UVar (l, v)) e)

let build_call l name el = Ast.UCall (l, name, el)     

let build_comp l bop e1 e2 sl = Ast.UCond (l, (build l bop e1 e2), sl)

let build_itv a b loc tgt_type =
  let x1, x1s, t1 = a
  and x2, x2s, t2 = b in
  if t1 = t2 && t1 = tgt_type then
    Ast.URand (loc, t1, (x1, x1s), (x2, x2s))
  else
    failwith "invalid type: range with different types or invalid type"


let build_fxp_itv a b loc tgt_type  =
  let x1, x1s, t1 = a
  and x2, x2s, t2 = b in
  if t1 = t2 then (* just checking both bounds are of same type.
		     Convertion will be addressed later. *)
    Ast.URand (loc, tgt_type, (x1, x1s), (x2, x2s))
  else
    failwith "invalid type: range with different types or invalid type"

let build_nn l inputs layers_files outputs =
  Ast.UNN (l, List.map fst inputs, layers_files, List.map fst outputs)

let build_fxp signed total_bits  (frac_bits, _, frac_type) =
  match frac_type with
  | Ast.IntT ->
     let _t = total_bits in
     let _f = Q.to_int frac_bits in
     if _t >= _f && _f >= 0 then
       Fxp.mk signed _t _f
     else
       failwith "invalid type: fixpoint definition" 
  | _ ->
     failwith "invalid type: fixpoint definition should use valid integers" 

%}

%token <Q.t * string * Ast.base_type> NUM
%token <string> VAR
%token <string> STRING
%token TRUE FALSE 
%token LPAR RPAR SEMICOL DBLDOT COMMA RAND_ITV_REAL RAND_ITV_INT RAND_BOOL EQUAL IF ELSE WHILE READIN READS LBRA RBRA
%token PLUS2 MINUS2
%token GT LT GE LE
%token SHIFTLEFT SHIFTRIGHT
%token PLUS MINUS
%token TIMES DIV
%token UMINUS
%token AND OR
%token EQEQ
%token NOT
%token INTTYPE REALTYPE BOOLTYPE 
%token <bool * int * int> FIXTYPE
%token <bool * int> MACINTTYPE
%token <bool * int * bool * int> FXPCONV
%token <bool * int> FXP_CST
%token <bool * int> RAND_ITV_FXP
%token NN 
%token EOF

%nonassoc WHILE 
%nonassoc IFX
%nonassoc ELSE
%left OR
%left AND 
%nonassoc NOT
%left GT LT GE LE
%left PLUS MINUS
%left TIMES DIV 
%nonassoc UMINUS

%type < Typing.typing_env * Ast.ustm> file
%start file

%%

file:
| decls stmts EOF { $1 Typing.empty_env, $2 }

decls:
| vtype varlist SEMICOL decls {  
  fun env -> 
    let env = List.fold_left (Typing.decl_var_type $1) env $2 in
    $4 env
}

| vtype varlist SEMICOL { fun env -> List.fold_left (Typing.decl_var_type $1) env $2 }



vtype:
  | INTTYPE { Ast.IntT }
  | REALTYPE { Ast.RealT }
  | BOOLTYPE { Ast.BoolT }
  | MACINTTYPE {
	let s, t = $1 in
	Ast.MIntT (s,t) }
  | FIXTYPE {
	let s, t, f = $1 in 
	let fxp = Fxp.mk s t f in
	Ast.FixedT fxp
	    }

	   
varlist:
| VAR COMMA varlist { ($1,loc $symbolstartpos $endpos)::$3 }
| VAR { [$1, loc $symbolstartpos $endpos] }
    

bloc:
| LBRA stmts RBRA { $2 }
| stm { $1 }

stmts:
| stm { $1 }
| stm stmts { Ast.USeq (loc $symbolstartpos $endpos, $1, $2) }


stm:
| VAR EQUAL expr SEMICOL { Ast.UAsn (loc $symbolstartpos $endpos, $1, $3) }
| IF LPAR expr RPAR bloc ELSE bloc
        { Ast.UIte (loc $symbolstartpos $endpos, $3, $5, $7) }
| IF LPAR expr RPAR bloc %prec IFX
        { Ast.UIte (loc $symbolstartpos $endpos, $3, $5, Ast.UNop(loc $symbolstartpos $endpos)) }
| WHILE LPAR expr RPAR bloc
    { Ast.UWhile (loc $symbolstartpos $endpos, $3, $5) }
| READIN LPAR varlist RPAR SEMICOL
    { Ast.UReadInput (loc $symbolstartpos $endpos, List.map fst $3) }
| READS LPAR varlist RPAR SEMICOL
    { Ast.UReadState (loc $symbolstartpos $endpos, List.map fst $3) }
/* syntactic sugar : v *= e ~~> v = v * e */
| VAR PLUS EQUAL expr SEMICOL { build_op_eq (loc $symbolstartpos $endpos) $1 Ast.Plus $4 }
| VAR MINUS EQUAL expr SEMICOL { build_op_eq (loc $symbolstartpos $endpos) $1 Ast.Minus $4 }
| VAR TIMES EQUAL expr SEMICOL { build_op_eq (loc $symbolstartpos $endpos) $1 Ast.Times $4 }
| VAR DIV EQUAL expr SEMICOL { build_op_eq (loc $symbolstartpos $endpos) $1 Ast.Div $4 }
/* syntactic sugar : ++x ~~> x = x + 1 */
| PLUS2 VAR SEMICOL { build_op_eq (loc $symbolstartpos $endpos) $2 Ast.Plus (Ast.UCst (loc $symbolstartpos $endpos, (Q.of_int 1, "1", None))) }
| VAR PLUS2 SEMICOL { build_op_eq (loc $symbolstartpos $endpos) $1 Ast.Plus (Ast.UCst (loc $symbolstartpos $endpos, (Q.of_int 1, "1", None))) }
| MINUS2 VAR SEMICOL { build_op_eq (loc $symbolstartpos $endpos) $2 Ast.Minus (Ast.UCst (loc $symbolstartpos $endpos, (Q.of_int 1, "1", None))) }
| VAR MINUS2 SEMICOL { build_op_eq (loc $symbolstartpos $endpos) $1 Ast.Minus (Ast.UCst (loc $symbolstartpos $endpos, (Q.of_int 1, "1", None))) }
| NN LPAR LPAR varlist RPAR COMMA LPAR layerlist RPAR COMMA LPAR varlist RPAR RPAR SEMICOL { build_nn (loc $symbolstartpos $endpos) $4 $8 $12 }


expr:
| NUM { let x, xs, t = $1 in Ast.UCst (loc $symbolstartpos $endpos, (x, xs, Some t)) }
  | FXP_CST LPAR NUM COMMA NUM RPAR {
	      let s, t = $1 in
	      let fxp = build_fxp s t $3 in
	      let x, xs, _ (* t *) = $5 in (* we neglect t *)
	      Ast.UCst (loc $symbolstartpos $endpos, (x, xs, Some (Ast.FixedT fxp)))
	  }
  | FXPCONV LPAR NUM COMMA NUM COMMA expr RPAR {
	      let s1, t1, s2, t2 = $1 in
	      let f1 = $3 in
	      let f2 = $5 in
	      let fxp_old = build_fxp s1 t1 f1 in
	      let fxp_new = build_fxp s2 t2 f2 in
	    let e = $7 in
	    Ast.UFxpConv (loc $symbolstartpos $endpos, fxp_old, fxp_new, e)
	  }
| TRUE { Ast.UCst ( loc $symbolstartpos $endpos, (Q.one, "true", Some BoolT) ) }
| FALSE { Ast.UCst ( loc $symbolstartpos $endpos, (Q.zero, "false", Some BoolT) ) }
| VAR { Ast.UVar (loc $symbolstartpos $endpos, $1) }
| RAND_BOOL LPAR RPAR {
		  Ast.URand (loc $symbolstartpos $endpos, Ast.BoolT, (Q.of_int 0, "false"), (Q.of_int 1, "true"))
}
| RAND_ITV_INT LPAR signed_num COMMA signed_num RPAR {
		 build_itv $3 $5 (loc $symbolstartpos $endpos) Ast.IntT
}
| RAND_ITV_REAL LPAR signed_num COMMA signed_num RPAR {
		 build_itv $3 $5 (loc $symbolstartpos $endpos) Ast.RealT
}
  | RAND_ITV_FXP LPAR NUM COMMA signed_num COMMA signed_num RPAR {
		   let s, t = $1 in
		   let fxp = build_fxp s t $3 in
	build_fxp_itv $5 $7 (loc $symbolstartpos $endpos) (Ast.FixedT fxp)
}
| LPAR expr RPAR { $2 }
| expr PLUS expr { build (loc $symbolstartpos $endpos) Ast.Plus $1 $3 }
| expr MINUS expr { build (loc $symbolstartpos $endpos) Ast.Minus $1 $3 }
| expr TIMES expr { build (loc $symbolstartpos $endpos) Ast.Times $1 $3 }
| expr DIV expr { build (loc $symbolstartpos $endpos) Ast.Div $1 $3 }
/* syntactic sugar : -e ~~> 0 - e */
| MINUS expr %prec UMINUS { build (loc $symbolstartpos $endpos) Ast.Minus (Ast.UCst (loc $symbolstartpos $endpos, (Q.of_int 0, "0", None))) $2 }
| VAR LPAR exprlist RPAR { build_call (loc $symbolstartpos $endpos) $1 $3 }
/* everything rephrased as expr >= 0 or expr > 0 */
| expr GT expr { build_comp (loc $symbolstartpos $endpos) Ast.Minus $1 $3 Ast.Strict }
| expr LT expr { build_comp (loc $symbolstartpos $endpos) Ast.Minus $3 $1 Ast.Strict }
| expr GE expr { build_comp (loc $symbolstartpos $endpos) Ast.Minus $1 $3 Ast.Loose }
| expr LE expr { build_comp (loc $symbolstartpos $endpos) Ast.Minus $3 $1 Ast.Loose }
/* Bool expr */
| NOT expr { Ast.UUnop (loc $symbolstartpos $endpos, Ast.Not, $2)}
| expr AND expr { build (loc $symbolstartpos $endpos) Ast.And $1 $3 }
| expr OR expr { build (loc $symbolstartpos $endpos) Ast.Or $1 $3 }
  | expr EQEQ expr { build (loc $symbolstartpos $endpos) Ast.Eq $1 $3 }
/* shift ops */
| expr SHIFTLEFT expr { Ast.UShiftLeft (loc $symbolstartpos $endpos, $1, $3) }
| expr SHIFTRIGHT expr { Ast.UShiftRight (loc $symbolstartpos $endpos, $1, $3) }


exprlist:
| expr COMMA exprlist {$1 :: $3}
| expr                 {[$1]}

layerlist:
| STRING DBLDOT expr  COMMA layerlist {($1, $3) :: $5}
| STRING DBLDOT expr                {[$1, $3]}



signed_num:
| NUM { $1 }
| MINUS NUM  { let x, xs, t = $2 in Q.neg x, "-" ^ xs, t }

bool:
  | TRUE { true }
  | FALSE { false }
