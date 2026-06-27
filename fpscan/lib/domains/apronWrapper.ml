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

(* We only rely on Apron for real variables *)
module type TypeAndManager = sig
  val name: string
  type domain_type
  val manager : domain_type Apron.Manager.t
end

module Wrapper (T : TypeAndManager) : Relational.Domain = struct
  include T

  type t = domain_type Apron.Abstract1.t
  let partitioned = ref false
  let print_bounds = ref false
                  
  let parse_param s =
    if s = T.name ^ ":nopart" then
      partitioned := true;
    if s = T.name ^ ":bounds" then
      print_bounds := true
      
  let fprint_help fmt (*fmt*) = Format.fprintf fmt "bounds"
  let nonrel_base = None
  let is_partitioned () = !partitioned
  (*let fprint = Apron.Abstract1.print*)

  let order x y =
    Apron.Abstract1.is_leq manager x y 

  let env_of_var_set s =
    let (sInt,_) = Ast.Var.Set.partition_by_type IntT s in
    let (sReal,_) = Ast.Var.Set.partition_by_type RealT s in
    let sInt = Ast.Var.Set.to_names sInt in
    let aInt = Array.of_list (Name.Set.elements sInt) in
    let aInt = Array.map (fun v -> Apron.Var.of_string v) aInt in
    let sReal = Ast.Var.Set.to_names sReal in
    let aReal = Array.of_list (Name.Set.elements sReal) in
    let aReal = Array.map (fun v -> Apron.Var.of_string v) aReal in
    Apron.Environment.make aInt aReal

  let get_vars s =
    let env = Apron.Abstract1.env s in
    let vi, vr = Apron.Environment.vars env in
    let arr_to_set v typ =
      Ast.Var.Set.of_list
        (
          List.map
            (fun v -> Apron.Var.to_string v, typ)
            (Array.to_list v)
        )
    in
    Ast.Var.Set.union (arr_to_set vi Ast.IntT) (arr_to_set vr Ast.RealT)
    
  let top s = Apron.Abstract1.top manager (env_of_var_set s)
  let bottom s = Apron.Abstract1.bottom manager (env_of_var_set s)
  let is_bottom s = Apron.Abstract1.is_bottom manager s
                  
  let join x y =
    (* Format.eprintf "join begin %a (%a, %b) vs %a (%a, %b)@.@?" fprint x Ast.Var.Set.pp (get_vars x) (is_bottom x) fprint y Ast.Var.Set.pp (get_vars y) (is_bottom y); *)
    let res =
      match is_bottom x, is_bottom y with
      | true, false -> y
      | false, true -> x
      | true, true -> bottom (get_vars x)
      | _ -> Apron.Abstract1.join manager x y
    in
    res
    
  let meet x y =
    (* Format.eprintf "meet begin %a (%a, %b) vs %a (%a, %b)@.@?" fprint x Ast.Var.Set.pp (get_vars x) (is_bottom x) fprint y Ast.Var.Set.pp (get_vars y) (is_bottom y); *)
    let res =
      if is_bottom x || is_bottom y then
        bottom (get_vars x)
      else
        Apron.Abstract1.meet manager x y
    in
    res

  let widening x y =
    (* Format.eprintf "widening begin %a (%a, %b) vs %a (%a, %b)@.@?" fprint x Ast.Var.Set.pp (get_vars x) (is_bottom x) fprint y Ast.Var.Set.pp (get_vars y) (is_bottom y); *)
    let res =
      let y = join x y in  (* Apron's widening requires that its first argument
                            * is included in the second one. *)
      Apron.Abstract1.widening manager x y
    in
    res
    
  let conv_typ typ =
    match typ with
    | Ast.IntT -> Apron.Texpr1.Int
    | RealT -> Apron.Texpr1.Real
    | Ast.FixedT _ -> Apron.Texpr1.Int (* fixed point are addressed as integers *)
    | Ast.MIntT _ -> Apron.Texpr1.Int (* fixed point are addressed as integers *)
    | BoolT -> assert false (* Not handled by Apron domain *) 
             
  let q_to_mpqf q =
    (*
    Z_mlgmpidl.mpq_of_q q
     *)
    Mpqf.of_frac (Z.to_int (Q.num q)) (Z.to_int (Q.den q))
      
  let apron_texpr_of_expr env e =
    let rec apron_expr_of_expr e =
      match e.Ast.expr_desc with 
      | Ast.Cst (q, _) -> Apron.Texpr1.Cst (Apron.Coeff.s_of_mpqf (q_to_mpqf q))
      | Ast.Var n -> Apron.Texpr1.Var (Apron.Var.of_string n)
      | Ast.Unop _ (*(unop, e)*) -> assert false (* The only unop is boolean negation *)
      | Ast.Binop (bop, e1, e2) ->
        let e1 = apron_expr_of_expr e1 in
        let e2 = apron_expr_of_expr e2 in
        let bop = match bop with
          | Ast.Plus -> Apron.Texpr1.Add
          | Ast.Minus -> Apron.Texpr1.Sub
          | Ast.Times -> Apron.Texpr1.Mul
          | Ast.Div -> Apron.Texpr1.Div
          | _ -> assert false
        in
        Apron.Texpr1.Binop (bop, e1, e2, conv_typ e.expr_type, Apron.Texpr1.Near)
      | Ast.Rand ((q1, _), (q2, _)) -> Apron.Texpr1.Cst (Apron.Coeff.i_of_mpqf (q_to_mpqf q1) (q_to_mpqf q2))
      | Ast.Call _ -> assert false
      | Ast.Cond _ -> assert false
      | Ast.ShiftLeft (e, dec) when Ast.is_int_type e.expr_type ->
        let e = apron_expr_of_expr e in
        let dec = apron_expr_of_expr dec in
        let cst_2 = Apron.Texpr1.Cst(Apron.Coeff.s_of_int 2) in
        let shift = Apron.Texpr1.Binop(Apron.Texpr1.Pow, cst_2, dec, Apron.Texpr1.Int, Apron.Texpr1.Near) in 
        (* let shift = Apron.Texpr1.Cst(Apron.Coeff.s_of_int (Int.shift_left 1 dec)) in *)
        Apron.Texpr1.Binop (Apron.Texpr1.Mul, e, shift, Apron.Texpr1.Int, Apron.Texpr1.Near)
      | Ast.ShiftLeft _ -> assert false 
      | Ast.ShiftRight (e, dec) when Ast.is_int_type e.expr_type ->
        let e = apron_expr_of_expr e in
        (* let shift = Apron.Texpr1.Cst (Apron.Coeff.s_of_int (Int.shift_left 1 dec)) in *)
        let dec = apron_expr_of_expr dec in
        let cst_2 = Apron.Texpr1.Cst(Apron.Coeff.s_of_int 2) in
        let shift = Apron.Texpr1.Binop(Apron.Texpr1.Pow, cst_2, dec, Apron.Texpr1.Int, Apron.Texpr1.Near) in 
        Apron.Texpr1.Binop (Apron.Texpr1.Div, e, shift, Apron.Texpr1.Int, Apron.Texpr1.Near)
      | Ast.ShiftRight _ -> assert false 
      | Ast.FxpConv(fxp_old, fxp_new, e) ->
        (* TODO: manage the overflow part of the datatype.
           For the moment, we focus on precision and shifts *)
        let e = apron_expr_of_expr e in
        let dec = fxp_new.frac - fxp_old.frac in
        if dec = 0 then
          e
        else
          let op, shift = 
            if dec > 0 then
              Apron.Texpr1.Mul, Apron.Coeff.s_of_int (Int.shift_left 1 dec)
            else
              Apron.Texpr1.Div, Apron.Coeff.s_of_int (Int.shift_left 1 (-dec))
          in
          let shift =  Apron.Texpr1.Cst shift in 
          Apron.Texpr1.Binop (op, e, shift, Apron.Texpr1.Int, Apron.Texpr1.Near)

    in
    Apron.Texpr1.of_expr env (apron_expr_of_expr e)
      
  let assignment n e t =
     if e.Ast.expr_type != BoolT then
      let e = apron_texpr_of_expr (Apron.Abstract1.env t) e in
      Apron.Abstract1.assign_texpr_array manager
        t [|Apron.Var.of_string n|] [|e|] None
    else
      t

let backward_assignment is_first_iter (input_vars : Scenario.NameSet.t) var expr t =

    if expr.Ast.expr_type != BoolT then (
      let open Apron in
      let env = Apron.Abstract1.env t in
      let apron_expr = apron_texpr_of_expr env expr in

      Format.printf "[backward_assignment] initial state :";
      Apron.Abstract1.print Format.std_formatter t;
      Format.printf "\n%!";

      (*******)

      (* Extract all the contraints that contain the input var and potentially the variable k (no state variables) *)
      let get_relevant_constraints input_var t = 
      
        let (int_vars, real_vars) = Environment.vars env in
        let all_vars = Array.append int_vars real_vars in

        (* Check if the constraint contains an input variable and maybe k but no state variables *)
        let contains_input_and_not_states linexpr all_vars =
          let var_apron = Apron.Var.of_string input_var in
          let k_apron = Apron.Var.of_string "k" in

          let array_filter f arr =
            let res = ref [] in
            Array.iter (fun x -> if f x then res := x :: !res) arr;
            Array.of_list (List.rev !res)
          in

          (* Truly used vars (non zero coefficient)*)
          let used_vars =
            array_filter (fun v ->
              not (Apron.Coeff.is_zero (Apron.Linexpr1.get_coeff linexpr v))
            ) all_vars
          in

          (*Format.printf "used_vars: %s\n" (String.concat ", " (Array.to_list used_vars |> List.map Var.to_string));*)

          let allowed_vars = [| var_apron; k_apron|] in

          let all_allowed = Array.for_all (fun v ->
            Array.exists (fun av -> Var.compare av v = 0) allowed_vars
          ) used_vars in

          (* the input variable must be in the constraint *)
          let contains_var = Array.exists (fun v ->  Var.compare v var_apron = 0) used_vars in

          contains_var && all_allowed
        in

        (* Extraction of the relevant constraints *)
        let lincons_array = Abstract1.to_lincons_array manager t in
        let lincons1_arr = Array.map (fun lincons0 -> { Lincons1.lincons0 = lincons0; env = lincons_array.array_env }) lincons_array.lincons0_array in
        let relevant_constraints =
          Array.of_list (
            List.rev (
              Array.fold_left (fun acc lincons ->
                let linexpr = Lincons1.get_linexpr1 lincons in
                if contains_input_and_not_states linexpr all_vars then lincons :: acc else acc
              ) [] lincons1_arr
            )
          )
        in

        relevant_constraints
      in

    (******)

      let t = 
        if is_first_iter then

          let all_relevant_constraints =
            List.flatten (
                List.map (fun input_var -> Array.to_list (get_relevant_constraints input_var t)) (Scenario.NameSet.elements input_vars)
            )
          in
          (* Remove the inputs variable from the environement : allows to prevent the elimination of the inputs variables when doing the substitution *)
          let t = List.fold_left (fun t input_var -> Abstract1.forget_array manager t [| Apron.Var.of_string input_var |] false) t (Scenario.NameSet.elements input_vars)
          in

          (* Injection of the relevant constraints *)
          let lincons0_arr =
            Array.of_list (
              List.map (fun lincons1 -> lincons1.Lincons1.lincons0) all_relevant_constraints
            )
          in

          let env =
            match all_relevant_constraints with
            | lincons1 :: _ -> lincons1.env
            | [] -> failwith "No constraints in all_relevant_constraints!"
          in

          let earray : Lincons1.earray = {
            lincons0_array = lincons0_arr;
            array_env = env;
          } in

          let t = Abstract1.meet_lincons_array manager t earray
          in
          Format.printf "[backward_assignment] State after forgetting inputs vars (i) and reinjecting constraints on inputs vars :\n";
          Apron.Abstract1.print Format.std_formatter t;
          Format.printf "\n%!";
          t
        
        else
          t
      in

        Format.printf "[backward_assignment] Substituting var = %s with expr = " var;
        Ast.fprint_expr Format.std_formatter expr;
        Format.printf "\n%!";

        (* Substitution *)
        let t' = Apron.Abstract1.substitute_texpr manager t (Apron.Var.of_string var) apron_expr None in

        Format.printf "[backward_assignment] State after substitution:\n";
        Apron.Abstract1.print Format.std_formatter t';
        Format.printf "\n%!";

        t'

  ) else
      t

  let read_input (t : t) (vars : Ast.Var.t list) : t = 
    let expr = Scenario.Scenario.create_rand_expr (-1.) 1. Ast.RealT in
    List.fold_left (fun acc (name, _) -> assignment name expr acc) t vars

  let read_state t _ = t

  let guard e t =
    let res =
      match e.Ast.expr_desc with
      | Ast.Cond (expr, sl) -> (
        let cons =
          let cond = match sl with
            | Ast.Loose -> Apron.Tcons1.SUPEQ
            | Ast.Strict -> Apron.Tcons1.SUP
            | Ast.Zero -> Apron.Tcons1.EQ
            | Ast.NonZero -> Apron.Tcons1.DISEQ
          in
          let texpr = apron_texpr_of_expr (Apron.Abstract1.env t) expr in
          Apron.Tcons1.make texpr cond
        in
        (*Apron.Tcons1.print Format.std_formatter cons;*)
        let earray =
          let a = Apron.Tcons1.array_make (Apron.Abstract1.env t) 1 in
          Apron.Tcons1.array_set a 0 cons;
          a in
        Apron.Abstract1.meet_tcons_array manager t earray
      )
      | Ast.Cst (_, "true") -> t
      | Ast.Cst (_, "false") -> bottom (get_vars t)
      | Ast.Var _ -> t      
      | _ -> t (* Other conditions cannot yet be address *)  
    in
    res

    (*let to_bounds t =
    let vars = get_vars t in
    Ast.Var.Set.fold (fun ((n,_) as v) res ->
        let box = Apron.Abstract1.bound_variable manager t (Apron.Var.of_string n) in
        let n_res =
          if Apron.Interval.is_bottom box then
            assert false
          else
            let build_bound _ (*x*) = assert false (* TODO *)
            in
            Bounds.mk (build_bound box.inf) (build_bound box.sup)
        in
        ((v, None), n_res)::res
      ) vars []*)


  let to_bounds t =
    let scalar_of_apron (s : Apron.Scalar.t) : Scalar.t =
      let f = match s with
        | Apron.Scalar.Float f -> f
        | Apron.Scalar.Mpqf q -> Mpqf.to_float q
        | Apron.Scalar.Mpfrf f -> Mpfrf.to_float f
      in
      Scalar.of_float f
    in
    let vars = get_vars t in
    Ast.Var.Set.fold (fun ((n, _) as v) res ->
      let box = Apron.Abstract1.bound_variable manager t (Apron.Var.of_string n) in
      if Apron.Interval.is_bottom box ||
    Apron.Scalar.is_infty box.inf <> 0 || Apron.Scalar.is_infty box.sup <> 0
      then
        res
      else
        let n_res = Bounds.mk (scalar_of_apron box.inf) (scalar_of_apron box.sup) in
        ((v, None), n_res) :: res
    ) vars []

  let to_properties _t =
    [] (* TODO *)

  let fprint fmt t =
    (*if Flags.get_apply_to_bounds () then*)
    if !print_bounds then 
      let bounds = to_bounds t in
      let pp_bound fmt ((v, _), b) =
        Format.fprintf fmt "%s : %a" (fst v) Bounds.pp b
      in
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_bound) bounds
    else
      Apron.Abstract1.print fmt t

  let json t = `String (Format.asprintf "%a" fprint t)

  let project_values t1 t2 l_relation =
    if is_bottom t1 || is_bottom t2 then
      bottom (get_vars t2)

    else
      (* Functions to prefix variables *)
      let prefix = (string_of_int (Apron.Abstract1.hash manager t1)) ^ (string_of_int (Apron.Abstract1.hash manager t2)) in
      let rename_var_t prefix t =
        let env1 = Apron.Abstract1.env t in
        let vi, vr = Apron.Environment.vars env1 in
        let nvi, nvr, l_name_i, l_name_r = 
          let map = Array.map (fun v -> Apron.Var.of_string (prefix ^ (Apron.Var.to_string v))) in
          let nvi = map vi in
          let nvr = map vr in
          let map_name = Array.map2 (fun v vn -> (Apron.Var.to_string vn, Apron.Var.to_string v)) in
          nvi, nvr, Array.to_list (map_name vi nvi), Array.to_list (map_name vr nvr) in
        let t = Apron.Abstract1.rename_array manager t vi nvi in
        let t = Apron.Abstract1.rename_array manager t vr nvr in
        t, l_name_i, l_name_r in
      let unrename_var_t t l_name_i l_name_r = 
        let env1 = Apron.Abstract1.env t in
        let vi, vr = Apron.Environment.vars env1 in
        let nvi, nvr = 
          let map v l = Array.map (fun v -> Apron.Var.of_string (List.assoc (Apron.Var.to_string v) l) ) v in
          map vi l_name_i, map vr l_name_r in
        let t = Apron.Abstract1.rename_array manager t vi nvi in
        let t = Apron.Abstract1.rename_array manager t vr nvr in
        t in
      let rename_var_l prefix l = List.map (fun (e,(n,typ)) -> (e,(prefix ^ n, typ))) l in

      (* Prefix variables of the arrival env to avoid name conflict *)
      let t2_renamed, l_real_name_i, l_real_name_r = rename_var_t prefix t2 in
      let l_relation' = rename_var_l prefix l_relation in
      
      (* Functions to generate arrays *)
      let set_t t n typ = if typ = Ast.BoolT then t else Array.append t [|n|] in
      let set_n t n typ = set_t t (Apron.Var.of_string n) typ in
      let set_e t e typ = set_t t (apron_texpr_of_expr (Apron.Abstract1.env t1) e) typ in

      (* Generate arrays from name relations *)
      let a_val_in, a_var_out = 
        List.fold_left (fun (a1,a2) (e,(n,typ)) -> set_e a1 e typ, set_n a2 n typ) ([||], [||]) l_relation' in

      (* Extension of both environment to the one containing every variables allowing meet action *)
      let lce = Apron.Environment.lce (Apron.Abstract1.env t1) (Apron.Abstract1.env t2_renamed) in
      let t1' = Apron.Abstract1.change_environment manager t1 lce false in
      (* Forget relation with variables which will be reassigned *)
      let t2' = Apron.Abstract1.forget_array manager t2_renamed a_var_out false in
      let t2' = Apron.Abstract1.change_environment manager t2' lce false in
      let t2' = meet t2' t1' in

      (* Calculate the value of entries in the new environment *)
      let change_env = Array.map (fun e -> Apron.Texpr1.extend_environment e lce) in
      let a_val_in' = change_env a_val_in in

      (* Affect new values *)
      let t2' = Apron.Abstract1.assign_texpr_array manager t2' a_var_out a_val_in' None in

      (* Rebuild original env *)
      let t2' = Apron.Abstract1.change_environment manager t2' (Apron.Abstract1.env t2_renamed) false in
      let t2_final = unrename_var_t t2' l_real_name_i l_real_name_r in
  
      t2_final
end

module Polka = Wrapper (struct
                   let name = "polka"
                   type domain_type = Polka.loose Polka.t 
                   let manager = Polka.manager_alloc_loose ()
end)

module Oct = Wrapper (struct
                 let name = "oct"
                 type domain_type = Oct.t 
                 let manager = Oct.manager_alloc ()
end)

module Box = Wrapper (struct
  let name = "box"
  type domain_type = Box.t
  let manager = Box.manager_alloc ()
end)

(*module PolkaGrid = Wrapper (struct
  let name = "polka_grid"
  type domain_type = Polka.loose PolkaGrid.t
  let manager = PolkaGrid.manager_alloc ()
end)

module Ppl = Wrapper (struct
  let name = "ppl"
  type domain_type = Ppl.loose Ppl.t
  let manager = Ppl.manager_alloc_loose ()
end) *)
    

    
