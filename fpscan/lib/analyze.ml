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

let str_entering = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Entering in node analyzis : ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

let str_leaving = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Leaving node analyzis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

let str_node = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Node analyzis : "

let str_node_end = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ End of node analyzis "

module type Results =
  sig
    module Res: Results.Register
    val results : Res.t
  end


let analyze ?(vars_node=[]) ?(ast_nodes=[]) ?(map=Name.Map.empty) domain sc_max_iter scf_max_iter sct_max_iter invariant_vs_simulation_mode descending unrolling vars ast =

  let module Dom = (val domain : Relational.Domain) in
  let module Res = (Results.ResultRegister(Dom) : Results.Register) in
  let module Dom = Res.Dom in

 
  (* Analyze statement stm from abstract value t. Returns both the resulting
   * abstract value and an updated (m : Dom.t Location.map.t) registering all
   * the intermediate results. Performs [descending] descending iterations
   * after fixpoint of loops are reached.
   * m registers results as described in PrintResults.mli. *)
  let rec post_stm bottom ?(toplevel=false) ?(unroll=(-1))  (m, t) s =
    
    (* Just a shortcut. *)
    let post_stm = post_stm bottom in

    (* let register ?(unroll=(-1)) l t (m,mi) =
      if unroll < 0 then
	Location.Map.add l t m, mi
      else (
        (* Cleaning already computed values to make sure we keep only the freshest *)
	(* let m = Location.Map.remove l m in	 *)
	m, Location.MapInd.add (l,unroll) t mi
      )
    in *)

    (* Some functions in Dom with printing added. *)
    let assignment l n e t =
      let t' = Dom.assignment n e t in
      Report.nlogf 4 "%a⟦%s = %a⟧@,(%a)@ = %a." Location.fprint l
	n Ast.fprint_expr e Dom.fprint t Dom.fprint t';
      t' in
    let guard l e t =
      let t' = Dom.guard e t in
      Report.nlogf 4 "%a⟦%a⟧@,(%a)@ = %a." Location.fprint l
	Ast.fprint_expr e 
	Dom.fprint t Dom.fprint t';
      t' in
    let join l x y =
      let t = Dom.join x y in
      Report.nlogf 4 "%a%a@ ⊔ %a@ = %a." Location.fprint l
	Dom.fprint x Dom.fprint y Dom.fprint t;
      t in
    let order x y =
      let b = Dom.order x y in
      Report.nlogf 4 "%a@ ⊑ %a@ = %b."
	Dom.fprint x Dom.fprint y b;
      b in
    let project_values t t' r = 
      Report.nlogf 4 "Projection of the environment :@ (%a)@, to the new environment :@ (%a)@," Dom.fprint t Dom.fprint t';
      Report.nlogf 4 "@ Map : %a@," (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ;@ ") 
        (fun fmt (e,(n1,_)) -> Format.fprintf fmt "%a --> %s" Ast.fprint_expr e n1)) r;
      let t'' = Dom.project_values t t' r in
      Report.nlogf 4 "Result :@  %a@," Dom.fprint t'';
      t'' in

    match s with

    | Ast.Asn (l, n, e) ->
       let t = assignment l n e t in
           (* let m = register ~unroll (Location.end_p l) t m in *)
       let m = Res.register ~unroll (Location.end_p l) t m in
       m, t


    | Ast.Nde (l, ln, name, le) -> 

      (* Project standard env into sub-node env following the relation between args and inputs *)
      let project_to_called_node ln name args l =
        let a_id, e_id = Name.Map.find name map in
        let ast = List.assoc a_id ast_nodes in
        Report.nlogf 4 "%a⟦( %a ) =@ %s (%a)⟧@ %s@,Node %s :@ %a@." 
          Location.fprint l 
          (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
            Format.pp_print_string) ln
          a_id
          (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
            Ast.fprint_expr) args 
          str_entering 
          a_id 
          Ast.fprint_stm ast;
        let env_node, rel = Utils_node.get_env_rel_in e_id vars_node name args l in
        let t' = project_values t (Dom.top env_node) rel in
        Report.nlogf 4 "%s@," str_node;
        t', ast in

      (* Project sub-node env into standard env following the relation between outputs and returned values *)
      let project_to_caller_node t t' ln args name m l a =
        let a_id, e_id = Name.Map.find name map in
        let rel = Utils_node.get_rel_out e_id vars_node args name ln l in
        Report.nlogf 4 "%s@. Node %s :@  %a@," str_node_end a_id Ast.fprint_stm a;
        let t = project_values t' t rel in
        Report.nlogf 4 "@ %s@," str_leaving;
        let m = Res.register ~unroll (Location.end_p l) t m in
        m, t in

      
      let t', a = project_to_called_node ln name le l in
      let init_m = Res.empty in
      let m_node, t'' = post_stm ~unroll (init_m, t') a  in
      let m = Res.register_subnode m m_node name in
      let m, t''' = project_to_caller_node t t'' ln le name m l a in
      m, t'''



    | Ast.Asrt (l, e) ->
       let t = guard e.Ast.expr_loc e t in
       (* let m = register ~unroll (Location.end_p l) t m in *)
       let m = Res.register ~unroll (Location.end_p l) t m in
       m, t



    | Ast.Seq (_, s1, s2) -> post_stm ~toplevel ~unroll (post_stm ~unroll ~toplevel  (m, t) s1) s2

    | Ast.Ite (l, e, s1, s2) ->
       let t1 = guard e.Ast.expr_loc e t in
       (* let m = register ~unroll (Location.beg_p (Ast.loc_of_stm s1)) t1 m in *)
       let m = Res.register ~unroll (Location.beg_p (Ast.loc_of_stm s1)) t1 m in
       let m, t1 = post_stm ~toplevel ~unroll  (m, t1) s1 in
       let t2 = guard (Location.beg_p (Ast.loc_of_stm s2)) (Ast.neg_guard e) t in
       (* let m = register ~unroll (Location.beg_p (Ast.loc_of_stm s2)) t2 m in *)
       let m = Res.register ~unroll (Location.beg_p (Ast.loc_of_stm s2)) t2 m in
       let m, t2 = post_stm ~toplevel ~unroll  (m, t2) s2 in
       let t = join (Location.end_p l) t1 t2 in
       (* let m = register ~unroll (Location.end_p l) t m in *)
       let m = Res.register ~unroll (Location.end_p l) t m in
       m, t


    | Ast.ReadInput (_, lv) ->
        let lv_var : Ast.Var.t list = List.map (fun name -> (name, Ast.RealT)) lv in
        let t = Dom.read_input t lv_var in
        m, t

    (* Injection de contraintes sur les variables d'états ---> il faudrait ajouter une vérif que ce soit pas des varibales utilisées dans un read_input *)
    | Ast.ReadState (_, lv) ->
        let lv_var : Ast.Var.t list = List.map (fun name -> (name, Ast.RealT)) lv in
        let t = Dom.read_state t lv_var in
        m, t

    | Ast.While (l, e, s) ->
       let rec lfp n m t t' =

         let t'' = Dom.widening t t' in
         (* let m = register ~unroll (Location.beg_p (Ast.loc_of_expr e)) t'' m in *)
         let m = Res.register ~unroll (Location.beg_p (Ast.loc_of_expr e)) t'' m in
         Report.nlogf 3 "%aIteration %d: @[%a@ ∇ %a@ = %a.@]"
           Location.fprint l n Dom.fprint t Dom.fprint t' Dom.fprint t'';
         if order t'' t then begin
             Report.nlogf 3 "%aIteration %d: @[%a@ ⊑ %a,@ fixpoint reached.@]"
               Location.fprint l n Dom.fprint t'' Dom.fprint t;
             t'', (m, t'')
           end else begin
             let t' = guard (Ast.loc_of_expr e) e t'' in
             (* let m = register ~unroll (Location.beg_p (Ast.loc_of_stm s)) t' m in *)
             let m = Res.register ~unroll (Location.beg_p (Ast.loc_of_stm s)) t' m in
             let m, t' = post_stm ~unroll  (m, t') s in
             t'', snd (lfp (n + 1) m t'' t')
           end in

       let rec desc_iter n m t t' =
         if n > descending then m, t' else begin
             let t'' = guard (Ast.loc_of_expr e) e t' in
             (* let m = register ~unroll (Location.beg_p (Ast.loc_of_stm s)) t'' m in *)
             let m = Res.register ~unroll (Location.beg_p (Ast.loc_of_stm s)) t'' m in
             let m, t'' = post_stm ~unroll  (m, t'') s in
             let t''' = Dom.meet t' (Dom.join t t'') in
             (* let m = register ~unroll (Location.beg_p (Ast.loc_of_stm s)) t''' m in *)
             let m = Res.register ~unroll (Location.beg_p (Ast.loc_of_stm s)) t''' m in
             Report.nlogf 3 "%aDescending iteration %d of %d: \
                             @[%a@ ⊓ (@[%a@ ⊔ %a@])@ = %a.@]"
               Location.fprint l n descending
               Dom.fprint t' Dom.fprint t Dom.fprint t'' Dom.fprint t''';
             desc_iter (n + 1) m t t'''
           end in

       (* Loop unrolling: 
	 - dans le cas des boucles for, il faut voir comment gerer les branches negatives. 
	 Doit on les calculer ? 
        *)

       (* let m, neg, pos = do_unroll nb m t
         Unroll the current loop. Current unroll counter is nb, m is
         the abstract map to record local abstract values, t is the
         abstract element when evaluating the current loop body.  m is
         the updated map after the unroll. neg is the union of the
         abstract elements obtained when the guard is not matched. pos
         is the obtained element after the required number of
         unrolling.
        *)
       let rec do_unroll nb m t =
	 if nb >= unrolling then
	   (* No values for the negative case, keeping input t for the positive one *)
	   m, bottom, t 
	 else begin
	     Report.nlogf 3 "Unrolling loop: iterate #%i@ " nb;
	     (* let m = register ~unroll:nb (Location.beg_p (Ast.loc_of_expr e)) t m in *)
       let m = Res.register ~unroll:nb (Location.beg_p (Ast.loc_of_expr e)) t m in
             let t_neg = guard (Location.end_p l) (Ast.neg_guard e) t in
             let t_pos = guard (Location.end_p l) e t in
	     (* let m = register ~unroll:nb ( Location.beg_p (Ast.loc_of_stm s) ) t_pos m in *)
       let m = Res.register ~unroll:nb ( Location.beg_p (Ast.loc_of_stm s) ) t_pos m in
             if Dom.is_bottom t_pos then 
	       m, t_neg, t_pos
             else 
	       (* Recursive call and merging the negative branches *)
	       let m, t_pos = post_stm ~unroll:nb (m, t_pos) s in
	       let m, t_neg_joined, t_pos = do_unroll (nb+1) m t_pos in
	       m, Dom.join t_neg t_neg_joined, t_pos
	   end
       in      
       let m, t_neg_joined, t = do_unroll 0 m t in
       let m, t =
         if Dom.is_bottom t || (toplevel && not invariant_vs_simulation_mode) (* stop here is toplevel and set-based simulation option *)then
           m, t
         else
           (* Compute loop invariant. *)
           let m, t =
             (* First compute fixpoint. *)
             let t, (m, t') = lfp 0 m bottom t in
             (* Then perform potential descending iterations. *)
             desc_iter 1 m t t' in
           (* Compute reachable states after the loop. *)
           let t = guard (Location.end_p l) (Ast.neg_guard e) t in
           m, t
       in
       (* Adding potential unrolled cases *)
       let t = join (Location.end_p l) t t_neg_joined in 
       (* let m = register ~unroll (Location.end_p l) t m in *)
       let m = Res.register ~unroll (Location.end_p l) t m in
       m, t 

    | Ast.Nop _ -> m, t

    | Ast.NN _ -> assert false

  in

  (* Parse program, call [post_stm top] and output result. *)
  (*let vars = Ast.VarSet.to_names vars in*)
  let init_m = Res.empty in
  let m, _ =

    let bottom, top =

      let vars =

        if sc_max_iter > (-1) || scf_max_iter > (-1) || sct_max_iter > (-1)  then 

          (* Adding variable k corresponding to the iterator to the environment (for all DomSim) *)
          let vars_with_k = Ast.Var.Set.add ("k", Ast.RealT) vars in

          if sc_max_iter > (-1) then
            vars_with_k
          
          else
            let add_all_indexed_vars vars_with_k max_iter vars =
              let add_indexed_vars =
                if scf_max_iter > (-1) then
                  (* Environment for DomSim2 *)
                  fun (initial_var_name, _) max_iter vars ->
                    let rec aux i acc =
                      if i >= max_iter then acc
                      else
                        let var_name = initial_var_name ^ "_" ^ string_of_int i in
                        let acc = Ast.Var.Set.add (var_name, Ast.RealT) acc in
                        aux (i + 1) acc
                    in
                    aux 0 vars
                else
                  (* Environment for DomSim3 *)
                  fun (initial_var_name, _) max_iter vars ->
                    let rec aux i acc =
                      if i >= max_iter then acc
                      else
                        let var_name =
                          if i = 0 then initial_var_name ^ "_" ^ string_of_int i
                          else initial_var_name ^ "_" ^ string_of_int (i-1) ^ "_" ^ string_of_int i
                        in
                        let acc = Ast.Var.Set.add (var_name, Ast.RealT) acc in
                        aux (i + 1) acc
                    in
                    aux 0 vars
              in
              let initial_vars_list = Ast.Var.Set.elements vars_with_k in
              List.fold_left (fun acc initial_var -> add_indexed_vars initial_var max_iter acc) vars initial_vars_list
            in
            if scf_max_iter > (-1) then
              (* DomSim2 *)
              add_all_indexed_vars vars_with_k (scf_max_iter+1) Ast.Var.Set.empty
            else
              (* DomSim3 *)
              add_all_indexed_vars vars_with_k (sct_max_iter+1) Ast.Var.Set.empty
        else
          vars

      in

      Dom.bottom vars, Dom.top vars in

    post_stm bottom ~toplevel:true (init_m, top) ast 
  in
  Report.nlogf 1 "Analysis done.";
  let module MyResults =
    struct
      module Res = Res
      let results = m 
    end
  in
  (module MyResults: Results)

