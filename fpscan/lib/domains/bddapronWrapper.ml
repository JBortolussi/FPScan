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

(* Bddapron.Expr0.print env cond0 Format.std_formatter lexpr0; *)

open Ast
let rand_fresh_name = "rand_fresh_name"
(* We only rely on BDDApron for real variables *)
module type TypeAndManager = sig
  val name: string
  type domain_type
  val apronManager : domain_type Apron.Manager.t
end

module Wrapper (T : TypeAndManager) : Relational.Domain = struct
  include T
      
  type symbol = string (*expr_desc*)
  type bbinop =
    | Or
    | And
    | EQZ
    | NEQZ
    | GT
    | GEQ
  type binaryop =
    | Bool of bbinop
    | Opapron of Apron.Texpr1.binop
  type env = symbol Bddapron.Env.t

  type valt = domain_type Bddapron.Bdddomain1.t0
        
  type t = (symbol,valt) Bddapron.Domain1.t
  type ('b,'c,'d) bddman = (symbol,'b,'c,'d) Bddapron.Domain1.man
        
  let parse_param _ = ()
  let fprint_help fmt = ()
                      
  let nonrel_base = None
  let is_partitioned () = true
                                     
  let print_symbol (fmt:Format.formatter) l = Format.pp_print_string fmt l
      
      
        
  let make_bddapron apron =
    Bddapron.Domain1.make_bdd apron (* on utilise bdd *)

  let (env0,cond0) : (symbol Bddapron.Env.t * symbol Bddapron.Cond.t) =
    
    let cudd = Cudd.Man.make_v ~numVars:30 () in
        Cudd.Man.set_gc 500000000 Gc.major Gc.full_major;
  
      let symbol =
        Bddapron.Env.make_symbol
          print_symbol
      in
      let bddindex0 = ref 400 in
      let bddsize = ref 400 in
      let env0 = Bddapron.Env.make
          ~symbol:symbol
          ~bddindex0:(!bddindex0) ~bddsize:(!bddsize * 2)
          ~relational:true cudd
      in
      let cond0 = Bddapron.Cond.make
          ~symbol:env0.Bdd.Env.symbol
          ~bddindex0:0 ~bddsize:(!bddindex0) cudd
      in
      Cudd.Man.group cudd 0 (!bddindex0 + (2 * !bddsize))  Cudd.Man.MTR_FIXED;
      Cudd.Man.group cudd 0 !bddindex0 Cudd.Man.MTR_DEFAULT;
      Cudd.Man.group cudd !bddindex0 (2 * !bddsize) Cudd.Man.MTR_DEFAULT;
      for i = 0 to pred (!bddsize) do
        Cudd.Man.group cudd (!bddindex0 + 2*i) 2 Cudd.Man.MTR_DEFAULT
      done;
  
      (env0,cond0)

  let manager = make_bddapron apronManager 

 
  let fprint =
    Bddapron.Domain1.print manager
      
  let json t = `String (Format.asprintf "%a" fprint t)
      
  let order =
    Bddapron.Domain1.is_leq manager   
      
  let conv_typ tiny_typ =
    match tiny_typ with
    | IntT -> `Int
    | RealT -> `Real
    | BoolT -> `Bool


  let env_of_var_set s =
    let a = Var.Set.fold
        (fun (vname, vtype) res ->
          (vname, conv_typ vtype)::res
        ) s []
    in
    (*let a = [("randint", `Int)]@a in*)
    Bddapron.Env.add_vars env0 a
      
      
  let get_vars s = 
    let env = Bddapron.Domain1.get_env s in
    let set_var = PSette.elements (Bddapron.Env.vars env) in
    let symbol_to_astvar v =
      List.map
        (
         fun el -> (el,RealT)
        ) v
    in
    Var.Set.of_list (symbol_to_astvar set_var)

  let add_var_int env var =
    let a = (var, `Int) in
    Bddapron.Env.add_vars env [a]
      
  let top s = 
    Bddapron.Domain1.top manager (env_of_var_set s)
  let bottom s = 
    Bddapron.Domain1.bottom manager (env_of_var_set s)
  let is_bottom s = 
    Bddapron.Domain1.is_bottom manager s
      
  let join = 
    Bddapron.Domain1.join manager
  let meet = 
    Bddapron.Domain1.meet manager
      
  let widening x y =
    let y = join x y in  (* Apron's widening requires that its first argument
                          * is included in the second one. TODO: verify it is still the case with BddApron *)
     Bddapron.Domain1.widening manager x y
      
  let q_to_mpqf q =
    (*
      Z_mlgmpidl.mpq_of_q q
     *)
    Mpqf.of_frac (Z.to_int (Q.num q)) (Z.to_int (Q.den q))
      
      
      (* comment différencier les constante booléennes à celle numérique? *)      
  let translate_cst env cond cst =
    let (q, b) = cst in
    let typ_of_cst = if b="true" || b="false" then "bool" else "num" in
    if typ_of_cst="bool"
    then
      Bddapron.Expr0.Bool.to_expr
        ((if b="true" then Bddapron.Expr0.Bool.dtrue else Bddapron.Expr0.Bool.dfalse)
           env cond)
    else
      Bddapron.Expr0.Apron.to_expr
        (Bddapron.Expr0.Apron.cst env cond (Apron.Coeff.s_of_mpqf (q_to_mpqf q)))
        
  let apply_bbinop env cond op e1 e2 =
    match op with
    | Or | And ->
        let e1 = Bddapron.Expr0.Bool.of_expr e1 in
        let e2 = Bddapron.Expr0.Bool.of_expr e2 in
        begin match op with
        | Or -> Bddapron.Expr0.Bool.dor env cond e1 e2
        | And -> Bddapron.Expr0.Bool.dand env cond e1 e2
        | _-> assert false
        end
    | EQZ | NEQZ ->
        let typexpr1 = Bddapron.Expr0.typ_of_expr env e1 in
        let typexpr2 = Bddapron.Expr0.typ_of_expr env e2 in
        if typexpr1<>typexpr2 then begin assert false end;
        let res = Bddapron.Expr0.eq env cond e1 e2 in
        if op = EQZ
        then res
        else Bddapron.Expr0.Bool.dnot env cond res
    | GT | GEQ ->
        let typexpr = Bddapron.Expr0.typ_of_expr env e1 in
        let e1 = Bddapron.Expr0.Apron.of_expr e1 in
        begin match op with
        | GT ->
            Bddapron.Expr0.Apron.sup env cond e1
        | GEQ ->
            Bddapron.Expr0.Apron.supeq env cond e1
        | _-> assert false
        end
          
          
          
  let apply_binop env cond binop e1 e2 : symbol Bddapron.Expr0.expr =
    match binop with
    | Bool op ->
        let e = apply_bbinop env cond op e1 e2 in
        Bddapron.Expr0.Bool.to_expr e
    | Opapron op ->
        let typexpr = Bddapron.Expr0.typ_of_expr env e1 in
        begin match typexpr with
        (* For the moment we do not partition over numerical values *)
     (*   |`Bint (b,size) ->
            let e1 = Bddapron.Expr0.Bint.of_expr e1 in
            let e2 = Bddapron.Expr0.Bint.of_expr e2 in
            let fop = match op with
            | Apron.Texpr1.Add -> Bddapron.Expr0.Bint.add
            | Apron.Texpr1.Sub -> Bddapron.Expr0.Bint.sub
            | Apron.Texpr1.Mul -> Bddapron.Expr0.Bint.mul
            | _ -> assert false
            in
            let e = fop env cond e1 e2 in
            Bddapron.Expr0.Bint.to_expr e*)
        | `Real | `Int ->
            let e1 = Bddapron.Expr0.Apron.of_expr e1 in
            let e2 = Bddapron.Expr0.Apron.of_expr e2 in
            let bop =
              begin
                match op with
                | Apron.Texpr1.Add -> Bddapron.Expr0.Apron.add
                | Apron.Texpr1.Sub -> Bddapron.Expr0.Apron.sub
                | Apron.Texpr1.Mul -> Bddapron.Expr0.Apron.mul
                | Apron.Texpr1.Div -> Bddapron.Expr0.Apron.div
                | _ -> assert false
              end
            in
            Bddapron.Expr0.Apron.to_expr (bop env cond e1 e2)
        | `Bool -> assert false
        | `Benum _-> assert false
        | `Bint _ -> assert false
        end

  (* create an expression n >= q1 AND n <= q2 *)
  let translate_rand_to_cons env cond n q1 q2 loc typ =
    let a = {expr_desc = Cst (q1, "q1"); expr_loc = loc; expr_type = typ} in
    let b = {expr_desc = Cst (q2, "q2"); expr_loc = loc; expr_type = typ} in
    let minus1 = {expr_desc = Binop (Minus, n, a); expr_loc = loc; expr_type = typ} in
    let minus2 = {expr_desc = Binop (Minus, n, b); expr_loc = loc; expr_type = typ} in
    let cons1 = {expr_desc = Cond (minus1, Loose); expr_loc = loc; expr_type = typ} in
    let cons2_1 = {expr_desc = Cond (minus2, Strict); expr_loc = loc; expr_type = typ} in
    let cons2 = {expr_desc = Unop (Not, cons2_1); expr_loc = loc; expr_type = typ} in
    {expr_desc = Binop (And, cons1, cons2); expr_loc = loc; expr_type = typ}
              
  let rec apron_expr_of_expr env cond e =
    let env' = env in
    match e.expr_desc with 
    | Cst (q,b) -> translate_cst env' cond (q,b)
    | Var n -> Bddapron.Expr0.var env cond n
    | Unop (unop, e) ->
       let e = apron_expr_of_expr env' cond e in
       begin match unop with
       | Not ->
         Bddapron.Expr0.Bool.to_expr
            (Bddapron.Expr0.Bool.dnot env cond
               (Bddapron.Expr0.Bool.of_expr e))
       end
    | Binop (bop, e1, e2) ->
        let e1 = apron_expr_of_expr env' cond e1 in
        let e2 = apron_expr_of_expr env' cond e2 in
        let bop = match bop with
        | Plus -> Opapron Apron.Texpr1.Add
        | Minus -> Opapron Apron.Texpr1.Sub
        | Times -> Opapron Apron.Texpr1.Mul
        | Div -> Opapron Apron.Texpr1.Div
        | Eq -> Bool EQZ
        | Or -> Bool Or
        | And -> Bool And
        in
        let expr = apply_binop env cond bop e1 e2 in
        expr
    | Rand ((q1,_), (q2,_)) -> assert false (* Error when a user do an operation on rand functions or use it in a guard*)
       (*(* We bind a fresh variable to apply constraints *)
       let env2 =  Bddapron.Env.add_vars env [(rand_fresh_name, conv_typ e.expr_type)] in
       let t = Bddapron.Domain1.change_environment manager t env2 in
       
       let n = {expr_desc = Var rand_fresh_name;
                expr_loc = e.expr_loc;
                expr_type = e.expr_type
               }
       in
       (* We create an expression on that variable to encode both bounds of the interval *)
       let ast_expr = translate_rand_to_cons env2 cond n q1 q2 e.expr_loc e.expr_type in
       let (expr0,tbis) = apron_expr_of_expr t env2 cond ast_expr in
       let bexpr0 = Bddapron.Expr0.Bool.of_expr expr0 in
       let bexpr2 =
         Bddapron.Expr2.Bool.of_expr0 ~normalize:true ~reduce:true ~careset:true env2 cond bexpr0
       in
       let t2 = Bddapron.Domain1.meet_condition2 manager t bexpr2 in
       apron_expr_of_expr t2 (Bddapron.Domain1.get_env t2) cond n*)
    | Call _ -> assert false
    | Cond (expr, sl) ->
       let expr = apron_expr_of_expr env' cond expr in
       let bop = match sl with
         | Loose -> Bool GEQ
         | Strict -> Bool GT
         | Zero -> Bool EQZ
         | NonZero -> Bool NEQZ
       in
       apply_binop env cond bop expr expr(* le deuxieme "expr" n'est pas utiliser dans apply_binop, le membre de droite etant le zero *)

  let bexpr2_of_expr t cond0 expr =
    let env = Bddapron.Domain1.get_env t in
    let cond = Bdd.Cond.copy cond0 in
    let expr0 = apron_expr_of_expr env cond expr in
    let bexpr0 = Bddapron.Expr0.Bool.of_expr expr0 in
    Bddapron.Expr2.Bool.of_expr0 ~normalize:true ~reduce:true ~careset:true env cond bexpr0
      
  (* Assignment: special treatment of x = rand_xxx (a,b) expressions.)
   *)
  let assignment n e t =
    let env = Bddapron.Domain1.get_env t in
    match e.expr_desc with
    | Rand ((q1,_), (q2,_)) ->
        if (e.expr_type = BoolT) then
          t
        else
          let n = {expr_desc = Var n;
                   expr_loc = e.expr_loc;
                   expr_type = e.expr_type
                 }
          in
          (* We create an expression on that variable to encode both bounds of the interval *)
          let ast_expr = translate_rand_to_cons env cond0 n q1 q2 e.expr_loc e.expr_type in
          let expr0 = apron_expr_of_expr env cond0 ast_expr in
          let bexpr0 = Bddapron.Expr0.Bool.of_expr expr0 in
          let bexpr2 =
            Bddapron.Expr2.Bool.of_expr0 ~normalize:true ~reduce:true ~careset:true env cond0 bexpr0
          in
          Bddapron.Domain1.meet_condition2 manager t bexpr2
    | _ ->
        let lexpr0 = apron_expr_of_expr env cond0 e in
        let lexpr2 = Bddapron.Expr2.List.of_lexpr0 env cond0 [lexpr0] in
        Bddapron.Domain1.assign_listexpr2 manager
            t [n] lexpr2 None
    (* In case of a x = rand_xxx(a,b), eliminate the temporary
       variable used to express the condition *)
    (*if (Bddapron.Env.mem_var env2 rand_fresh_name) then
      let env2 = Bddapron.Domain1.get_env t2 in
      let env2 = Bddapron.Env.remove_vars env2 [rand_fresh_name] in
      Bddapron.Domain1.change_environment manager t2 env2
    else
      t2*)

  let guard e t =
    let env = Bddapron.Domain1.get_env t in
    let condition2 = bexpr2_of_expr t cond0 e in
    Bddapron.Domain1.meet_condition2 manager t condition2

  let to_bounds t =
    let vars = get_vars t in
    let env = Bddapron.Env.get_env t in
    let apron_env = Bddapron.Env.apron env in
    let apront = Apron.Abstract1.top apronManager apron_env in
    Var.Set.fold (fun ((n,_) as v) res ->
        let box = Apron.Abstract1.bound_variable apronManager apront (Apron.Var.of_string n) in
        let n_res =
          if Apron.Interval.is_bottom box then
            assert false
          else
            let build_bound x = assert false (* TODO *)
            in
            build_bound box.inf, build_bound box.sup
        in
        ((v, None), n_res)::res
      ) vars []
end

module PolkaBDD = Wrapper (struct
                 let name = "polkabdd"
                 type domain_type = Polka.loose Polka.t 
                 let apronManager = Polka.manager_alloc_loose ()
end)

module OctBDD = Wrapper (struct
                 let name = "octbdd"
                 type domain_type = Oct.t 
                 let apronManager = Oct.manager_alloc ()
end)

module BoxBDD = Wrapper (struct
                 let name = "boxbdd"
                 type domain_type = Box.t
                 let apronManager = Box.manager_alloc ()
end)

    
