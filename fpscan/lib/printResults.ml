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

module Make (Res : Results.Register) = struct
  module Dom = Res.Dom

  let get_unrolled_info ?(node_name=None) m l =
    Res.get_unrolled_info node_name m l

    
    (* Printing is more complex when unrolling:
       unrolled #1 val
       unrolled #i val
       remaining loop val
       joined  val *)
  let fprint_annot ?(print_top=false) ?(node_name=None) m top f ff l =
    (* Main info, ie. fixpoint ones *)
    (* Format.fprintf ff "%a == " Location.fprint l; *)
    let aopt = Res.find_aopt node_name l m in

    (* Extracting associated unrolled info *)
    let ind = get_unrolled_info ~node_name:node_name m l in
    
    if List.length ind <= 0 then
      match aopt with
      | None -> Format.fprintf ff "⊥" 
      | Some a -> 
        if Report.silent (fun () -> Dom.order top a) && not print_top then ()
        else Format.fprintf ff f (fun fmt -> Dom.fprint fmt a)
    else (* Existing unrolled info *)
      begin
        Format.fprintf ff f
          (fun fmt ->
            (* Each iterate in ascending order *)
            let found_bot, join = 
              List.fold_left (fun (found_bot, accu) (i, e) ->
                if found_bot then (true, accu) else 
                  (* Do not print useless info *)
                  let found_bot = if Dom.is_bottom e then (
                      Format.fprintf fmt "#%i+: %a@ " i Dom.fprint e;
                      true
                      )
                    else (
                      Format.fprintf fmt "#%i: %a@ " i Dom.fprint e;
                      false
                      )

                  in
                  found_bot,
                  (
                    match accu with
                    | None -> Some e
                    | Some accu -> Some (Dom.join accu e)
                  )
              ) (false, None) ind
                    
              in
              let join = Utils.desome join in
              if found_bot then
                Format.fprintf ff "#U: %a@ " Dom.fprint join 
              else
                match aopt with
                | None -> (
                  Format.fprintf ff "#+: ⊥@ ";
                  Format.fprintf ff "#U: %a@ " Dom.fprint join 
                )	
                | Some a -> (
                  Format.fprintf ff "#+: %a@ " Dom.fprint a;
                  Format.fprintf ff "#U: %a@ " Dom.fprint (Dom.join join a)
                )
          ) 
      end
	  
  let print ?(ast_nodes=[]) ?(env_nodes=[]) ?(map=Name.Map.empty) m vars t fmt =
      let rec fprint_stm ff (vars,s,node_name) =
      let top = Dom.top vars  in
      (*let fprint_annot = fprint_annot ~node_name:node_name m top in*)
      let fprint_annot = fprint_annot ~node_name:node_name m top in
      let fprint_invariant = fprint_annot "/* loop invariant: @[<v 0>%t@] */@ " in
      let fprint_annot = fprint_annot "@ /* @[<v 0>%t@] */" in
      match s with
      | Ast.Asn (l, n, e) -> Format.fprintf ff "%s = @[%a@];%a" n 
        Ast.fprint_expr e fprint_annot (Location.end_p l)
      | Ast.Asrt (l, g) -> Format.fprintf ff "assert(%a);%a"
        Ast.fprint_expr g fprint_annot (Location.end_p l)
      | Ast.Seq (_, s1, s2) ->
        Format.fprintf ff "@[<v>%a@ %a@]" fprint_stm (vars,s1,node_name) fprint_stm (vars,s2,node_name)
      | Ast.Ite (l, g, s1, s2) ->
        Format.fprintf ff "@[<v>@[<v 2>if (%a) {%a@ %a@]@ @[<v 2>} else {%a@ %a@]@ }%a@]"
          Ast.fprint_expr g
          fprint_annot (Location.beg_p (Ast.loc_of_stm s1)) fprint_stm (vars,s1,node_name)
          fprint_annot (Location.beg_p (Ast.loc_of_stm s2)) fprint_stm (vars,s2,node_name)
          fprint_annot (Location.end_p l)
      | Ast.While (l, g, s) ->
         Format.fprintf ff "@[<v>%a@[<v 2>while (%a) {%a@ %a@]@ }%a@]"
	   fprint_invariant (Location.beg_p (Ast.loc_of_expr g))
          Ast.fprint_expr g
          fprint_annot (Location.beg_p (Ast.loc_of_stm s)) fprint_stm (vars,s,node_name)
          fprint_annot (Location.end_p l) 
      | Ast.ReadInput (_, _) -> ()  
      | Ast.ReadState (_, _) -> () 
      | Ast.Nop _ (*l*) -> ()
      | Ast.NN ( l, ins, layers, outs ) ->
         Format.fprintf ff "@[<v>%a@ @[nn((%a), (%a), (%a))@]@]"
           fprint_annot (Location.beg_p l)
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
              Format.pp_print_string) ins
                      (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
              (fun fmt (lfile,act) -> Format.fprintf fmt "\"%s:%a\"" lfile Nn_types.pp_act act)) layers
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
              Format.pp_print_string) outs
      | Ast.Nde (l, ln, name, le) -> (
         let a_id, e_id = Name.Map.find name map in
         let v1, _, _ = Utils_node.get_env_node_in e_id env_nodes name le l in
        (*  let r_in = Utils_node.get_rel_in le inputs map_shared name e_id in
         let r_out = Utils_node.get_rel_out inputs le outputs map_shared name ln l in *)
         let s1 = List.assoc a_id ast_nodes in
         Format.fprintf ff "@[<v>@[<v 5>@[%a@] = %s (@[%a@]) {@ %a@]@ }%a@]"
         (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           Format.pp_print_string) ln 
         a_id
         (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           Ast.fprint_expr) le 
         (* (Format.pp_print_list 
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ;@ ")
           (fun fmt (e,(n1,_)) -> Format.fprintf fmt "%a --> %s" 
           Ast.fprint_expr e n1)) r_in *)
         fprint_stm (v1,s1,Some(name))
         (* (Format.pp_print_list 
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ;@ ")
           (fun fmt (e,(n1,_)) -> Format.fprintf fmt "%a --> %s" 
           Ast.fprint_expr e n1)) r_out *)
         fprint_annot (Location.end_p l)
      )
        
        
        
    in
    fprint_stm fmt (vars,t,None);
    Format.fprintf fmt "\n%!"
    
  let print_invariants m vars t fmt =
    let top = Dom.top vars (*(Ast.vars_of_stm t)*) in
    let fprint_annot = fprint_annot ~print_top:true m top in
    let rec fprint_stm ff s =
      match s with
      | Ast.Asn _ | Ast.Asrt _ | Ast.Nop _ | Nde _ -> ()
      | Ast.Seq (_, s1, s2)
      | Ast.Ite (_, _, s1, s2) -> fprint_stm ff s1; fprint_stm ff s2
      | Ast.While (_, g, s) -> 
         fprint_annot "%t@ " ff (Location.beg_p (Ast.loc_of_expr g));
	 fprint_stm ff s
      | Ast.ReadInput (_, _) -> ()
      | Ast.ReadState (_, _) -> ()
      | Ast.NN _ -> ()
    in
    Format.fprintf fmt "@[<v>";
    fprint_stm fmt t;
    Format.fprintf fmt "@]%!"

end
