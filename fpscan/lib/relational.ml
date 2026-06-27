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

exception BotEnv
        
(** A module type for relational domains. *)

(** Module type for relational domains. *)
module type Domain = sig
  (** Name of the domain. Used to select it via command line
      arguments. Should be of the form \[a-z\]\[a-zA-Z0-9_\]*. *)
  val name : string

  (** Underlying non relational domain when available *)
  val nonrel_base : (module NonRelational.Domain) option

  (** Specify whether the relational domain can address bool variables
     and associated partitioning *)
  val is_partitioned : unit -> bool
    
  (** [parse_param s] parses parameters in [s]. If [s] is of the form
      "dom:s'" where dom doesn't contain ':' and is not [name], the
      parameter should be ignored. If dom is [name], only s' should be
      considered ([Utils.select_param name] can be used for that). A
      warning should be issued for unrecognised parameters (use
      [Utils.warn_unknown_param]). *)
  val parse_param : string -> unit

  (** Outputs some help about the options recognized by
      [parse_param]. *)
  val fprint_help : Format.formatter -> unit

  (** Type of abstract values. *)
  type t

  (** Prints an abstract value. *)
  val fprint : Format.formatter -> t -> unit

  (** Json representation of an abstract value. *)
  val json : t -> Yojson.t

  (** {2 Lattice Structure} *)

  (** Order on type [t]. [t] with this order must be a lattice. *)
  val order : t -> t -> bool

  val top : Ast.Var.Set.t -> t
  val bottom : Ast.Var.Set.t -> t
  val is_bottom : t -> bool

  val get_vars: t -> Ast.Var.Set.t
  (** Infimums of the lattice (when the relational domain focuses on given set
      of variables). *)

  val join : t -> t -> t
  val meet : t -> t -> t
  (** Least upper bound and greatest lower bound of the lattice. *)

  (** Update the scenarios for the input variables given to read_input() instruction *)
  val read_input : t -> Ast.Var.t list -> t

  (** Injection of constraints on state variables *)
  val read_state : t -> Ast.Var.t list -> t

  (** Widening to ensure termination of the analyses. *)
  val widening : t -> t -> t

  (** {2 Abstract Operators} *)

  (** Abstract semantics of assignments and guards. *)

  (** [assignment n e t] returns a [t'] such that:
      {[[|n = e;|](\gamma(t)) \subseteq \gamma(t').]} *)
  val assignment : Name.t -> Ast.expr -> t -> t

  (** Used for backward propagation of constraints on a state variable (read_state instruction) in set based simulation mode (option -sc). 
  Works only when using the polka domains. Returns a [t'] such that : {[\gamma(t) \subseteq [|n = e;|](\gamma(t')).]} 
  Boolean value indicates if the it is the first iteration of the backpropagation, and the NameSet indicates the input variables in the program*)
  val backward_assignment : bool -> Scenario.NameSet.t -> Name.t -> Ast.expr -> t -> t

  (** [guard e t] returns a [t'] such that:
      {[[|e >= 0|](\gamma(t)) \subseteq \gamma(t').]}*)
  val guard : Ast.expr -> t -> t

  (* Export functions *)
  val to_bounds: t -> ((Ast.Var.t * (Ast.Var.t * bool) option) * Bounds.t) list
  val to_properties: t -> ((Ast.Var.t * (Ast.Var.t * bool) option) * Value_properties.t) list

  (** Projects abstract values on others abstract values following a given map *)
  val project_values : t -> t -> (Ast.expr * Ast.Var.t) list -> t
end

(** Product functor for relational domains. *)
module Prod (D1: Domain) (D2: Domain) : Domain =
struct

  let name = "_"
  let parse_param _ = ()
  let fprint_help _ (*fmt*) = ()
                      
  let is_partitioned () = false
  type t = D1.t * D2.t
  let nonrel_base = None
  let is_bottom (x,y) = D1.is_bottom x || D2.is_bottom y
  let fprint fmt (x,y) = if is_bottom (x,y) then
                           Format.fprintf fmt "⊥"
                         else
                           Format.fprintf fmt "%a, %a" D1.fprint x D2.fprint y
  let json (x,y) = 
    `Assoc ["domain_name", `String name; "left_domain", D1.json x; "right_domain", D2.json y]

  let order (x1, y1) (x2, y2) = D1.order x1 x2 && D2.order y1 y2
  let top s = D1.top s, D2.top s
  let bottom s = D1.bottom s, D2.bottom s
  let get_vars (x,y) = Ast.Var.Set.union (D1.get_vars x ) (D2.get_vars y)
  let join (x1, y1) (x2, y2) = D1.join x1 x2, D2.join y1 y2
  let meet ((x1, y1) as e1) ((x2, y2) as e2) = 
    if is_bottom e1 || is_bottom e2 then 
      bottom  (Ast.Var.Set.union (get_vars e1) (get_vars e2))
    else 
      D1.meet x1 x2, D2.meet y1 y2
  let widening (x1, y1) (x2, y2) = D1.widening x1 x2, D2.widening y1 y2
  let read_input (x,y) vars = D1.read_input x vars, D2.read_input y vars
  let read_state (x,y) vars = D1.read_state x vars, D2.read_state y vars
  let assignment v e (x,y) = D1.assignment v e x, D2.assignment v e y
  let backward_assignment b s v e (x,y) = D1.backward_assignment b s v e x, D2.backward_assignment b s v e y
  let guard e (x,y) = D1.guard e x, D2.guard e y
  let to_bounds (x,y) =
    let bx = D1.to_bounds x in
    let by = D2.to_bounds y in
    List.fold_left (fun res ((v, bx) as binding) ->
                             if List.mem_assoc v res then
                               let by = List.assoc v res in
                               (v, Bounds.join bx by)::
                                 (List.remove_assoc v res)
                             else
                               binding::res
      ) by bx
      
  let to_properties (x,y) =
    let px = D1.to_properties x in
    let py = D2.to_properties y in
    List.fold_left (fun res ((v, px) as binding) ->
        if List.mem_assoc v res then
          let py = List.assoc v res in
          (v, Value_properties.merge px py)::
          (List.remove_assoc v res)
        else
          binding::res
      ) py px
      
  let project_values (x1, y1) (x2, y2) l_relation =
    D1.project_values x1 x2 l_relation, D2.project_values y1 y2 l_relation
end


module MakeRelational (D : NonRelational.Domain) : Domain = struct
  let name = D.name
  let log = ref false
  let bnb_log = ref false
  let nlogf n = if
      D.log || !log
    then
      Report.nlogf n
    else
      Format.ifprintf
        Format.err_formatter
      
    
  let nonrel_base : (module NonRelational.Domain) option = Some (module D)

  (* Loading options *)

  (* To prevent additional partitioning: declare the domain as nonpart *)
  let partitioned = ref false
  let is_partitioned () = !partitioned

  (* To enable branch_and_bound when evaluating expression (forward) *)
  let bnb = ref false
          
  let parse_param s =
    (* Prevent partitioning is nopart option is used *)
    if s = name ^ ":nopart" then (
      Report.nlogf 1 "Partitioning of domain %s disabled@." name;
      partitioned := true;
    );
    (* Activate branch and bound in eval_expr *)
    if s = name ^ ":bnb" then (
      Report.nlogf 1 "Branch and bound when evaluating forward expressions in domain %s@." name;
      bnb := true;
    );
    (* Force log *)
    if s = name ^ ":log" then
      log := true;
    (* Force bnb log *)
    if s = name ^ ":bnb:log" then
      bnb_log := true;
    (* Continue with domain specific options *)
    D.parse_param s


  let fprint_help = D.fprint_help

  let base_type = D.base_type

  type t = EnvBot | Env of D.t Name.Map.t

  let fprint ff t = match t with
    | EnvBot -> Format.fprintf ff "⊥"
    | Env m ->
      if Name.Map.is_empty m then
        Format.fprintf ff "⊤"
      else
        let first = ref true in
        Name.Map.iter
          (fun n v ->
            if !first then begin
              Format.fprintf ff "@[<2>{%s@ %s : %a" name n D.fprint v;
              first := false
            end else
              Format.fprintf ff ",@ %s : %a" n D.fprint v)
          m;
        Format.fprintf ff " }@]"

  let json t = 
    let json_env = (match t with
     | EnvBot -> `String "⊥"
     | Env m ->
      if Name.Map.is_empty m then
        `String "⊤"
      else
        (* list_of_abstract_values is a list of pair consisting of 
           the name of the variable and its the domain value. *)
        let list_of_abstract_values =  m |> Name.Map.to_seq |> List.of_seq in 
        `Assoc (List.map 
          (fun (name, value) ->
              (name, D.json value))
          list_of_abstract_values)
    )
    in
    `Assoc ["domain_name", `String name; "env", json_env];;

  let to_bounds t =
    match t with
    | EnvBot -> raise BotEnv
    | Env map ->
       let bindings = Name.Map.bindings map in
       List.fold_right (fun (n, d) accu ->
           try
             (((n, base_type), None), D.to_bounds d)::accu
           with Utils.IsBottom -> accu
         )
         bindings []

   let to_properties t =
    match t with
    | EnvBot -> raise BotEnv
    | Env map ->
       let bindings = Name.Map.bindings map in
       List.fold_right (fun (n, d) accu ->
           try
             (((n, base_type), None), D.to_properties d)::accu
           with Utils.IsBottom -> accu
         )
         bindings []

  let find_or_top n m = try Name.Map.find n m with Not_found -> D.top

  let order_print n x y =
    let b = D.order x y in
    nlogf n "%a@ ⊑ %a@ = %b."
      D.fprint x D.fprint y b;
    b

  let order t1 t2 = match t1, t2 with
    | EnvBot, _ -> true
    | _, EnvBot -> false
    | Env m1, Env m2 -> Name.Map.fold
      (fun n v2 b -> b && order_print 6 (find_or_top n m1) v2)
      m2
      true

  let top _ = Env Name.Map.empty
  let bottom _ = EnvBot

  let is_bottom t = match t with EnvBot -> true | Env m -> Name.Map.exists (fun _ v -> D.is_bottom v) m

  let get_vars t = 
    match t with 
    | EnvBot -> Ast.Var.Set.empty 
    | Env m -> Name.Map.fold (fun k _ s -> Ast.Var.Set.add (k, base_type) s) m Ast.Var.Set.empty

  let join_f s f t1 t2 = match t1, t2 with
    | EnvBot, _ -> t2
    | _, EnvBot -> t1
    | Env m1, Env m2 ->
      let m = Name.Map.fold
        (fun n v1 m ->
          let v2 = find_or_top n m2 in
          let v = f v1 v2 in
          nlogf 5 "%a@ %s %a@ = %a."
            D.fprint v1 s D.fprint v2 D.fprint v;
          if order_print 6 D.top v then m
          else Name.Map.add n v m)
        m1
        Name.Map.empty in
      Env m

  let join = join_f "⊔" D.join

  let meet_print x y =
    let r = D.meet x y in
    nlogf 5 "%a@ ⊓ %a@ = %a."
      D.fprint x D.fprint y D.fprint r;
    r

  let meet t1 t2 = match t1, t2 with
    | EnvBot, _ | _, EnvBot -> EnvBot
    | Env m1, Env m2 ->
      try
        let m = Name.Map.fold
          (fun n v1 m ->
            let v2 = find_or_top n m2 in
            let v = meet_print v1 v2 in
            if order_print 6 v D.bottom then raise Exit
            else Name.Map.add n v m)
          m1
          m2 in
        Env m
      with Exit -> EnvBot

  (* We assume that the analyzed program has a finite number of variables.
   * Then, widening of environments is just a pointwise application of
   * D.widening.*)
  let widening = join_f "∇" D.widening

  let rec eval_expr env e = match e.Ast.expr_desc with
    | Ast.Cst (n,ns) -> eval_expr env (Ast.mk_expr e.Ast.expr_loc e.Ast.expr_type (Ast.Rand ((n,ns), (n,ns))))
    | Ast.Var n -> find_or_top n env
    | Ast.Unop (_ (*uop*), _ (*e1*)) ->
      assert false (* The only unop is "not", a bool one, should be
                      addressed elsewhere *)
    | Ast.Binop (bop, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let s, sem = match bop with
        | Ast.Plus -> "sem_plus", D.sem_plus
        | Ast.Minus -> "sem_minus", D.sem_minus
        | Ast.Times -> "sem_times", D.sem_times
        | Ast.Div -> "sem_div", D.sem_div
        | _ -> assert false (* Other binop are bool ones, should be
                               addressed elsewhere *)
      in
      let v = sem v1 v2 in
      nlogf 5 "%s@ %a@ %a@ = %a."
        s D.fprint v1 D.fprint v2 D.fprint v;
      v
    | Ast.Rand ((n1, n1s), (n2, n2s)) ->
      let v = D.sem_itv (n1,n1s) (n2,n2s) in
      nlogf 5 "sem_itv@ %a@ %a@ = %a." Q.pp_print n1 Q.pp_print n2 D.fprint v;
      v
    | Ast.Call (f, args) ->
      let vl = List.map (eval_expr env) args in
      let v = D.sem_call f vl in
      nlogf 5 "sem_call %s@ (@[%a@])@ = %a."
	f (Utils.fprintf_list ~sep:",@ " D.fprint) vl D.fprint v;
      v
    | Ast.Cond _ -> (* TODO guard e env *) D.top

    | Ast.FxpConv _ -> assert false (* TODO *)
    | Ast.ShiftLeft (e1, e2) 
    | Ast.ShiftRight (e1, e2) -> 
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      let _2 = Q.of_int 2, "2" in
      let _2_v2 = D.sem_call "pow" [D.sem_itv _2 _2; v2] in
      (
        match e.expr_desc with
        | ShiftLeft _ -> 
          D.sem_times v1 _2_v2
        | ShiftRight _ ->   (* TODO: we could compute the intersection
                               of v1 / (2 ^ v2) and (v1 * 2 ^-v2) *)

          D.sem_div v1 _2_v2
        | _ -> assert false
      )
    
  (* Extension of eval_expr to perform bnb *)

  let pp_info fmt info = Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt ":") Format.pp_print_string fmt info
  (* bounds_map associate to each free variable its bounds. Split the env into a list of (split_info, sub_env) *)
  let bnb_split bounds_map env =
    (* First heuristic: split the largest variable *)
    let largest_var, bounds =
      Name.Map.fold'
        (fun varname varbound ->
          varname, varbound
        )
        (fun ((_,b1) as e1) ((_,b2) as e2) ->
          if Bounds.size b1 >= Bounds.size b2 then
            e1 else e2
        )
        bounds_map 
    in
    let abs_val = Name.Map.find largest_var env in
    if !bnb_log then Format.eprintf "Splitting var %s (%a): "
      largest_var Bounds.pp bounds;
    match D.split abs_val with
    | None -> if !bnb_log then Format.eprintf "no split@ "; ["no_split", env]
    | Some (abs_val1, abs_val2) ->
       if !bnb_log then Format.eprintf "%a - %a@ " D.fprint abs_val1 D.fprint abs_val2; 
       let env1 = Name.Map.add largest_var abs_val1 env in
       let env2 = Name.Map.add largest_var abs_val2 env in
       let prefix = largest_var ^ "_" in
       (prefix  ^  "1", env1)::(prefix ^ "2", env2)::[]
    
  let eval_expr env e =
    if not !bnb then 
      eval_expr env e
    else
      begin
        let free_vars = Ast.vars_of_expr e in
        
        (* For debugging purpose: priunt only the env on free vars *)
        let pp_env fmt env = fprint fmt (Env (Name.Map.filter (fun k _ -> Name.Set.mem k free_vars) env)) in
        
        if Name.Set.cardinal free_vars <= 0 then
          eval_expr env e
        else
          begin
            Format.eprintf "@[<v 2>BNB eval of %a@ " Ast.fprint_expr e;
            let get_bounds env =
              let bounds = to_bounds (Env env) in
              (* Simplify representation, discarding partitioning
           information (the "context" in bounds). Focus only on
           variables of the current domain base type.  *)
              let bounds_free_vars =
                List.fold_left (
                    fun accu (((v,_base_type), _ (* ctx *)), bound) ->
                    if Typing.are_types_eq _base_type base_type && Name.Set.mem v free_vars then
                      Name.Map.update v
                        (function
                         | None -> Some bound
                         | Some old_bound -> Some (Bounds.join old_bound bound)
                        ) accu
                    else
                      accu
                  ) Name.Map.empty bounds in
              bounds_free_vars
            in
            let rec bnb_eval ?(info:string list=[]) ~depth e_val old_val_bounds env elem_list =
              if !bnb_log then Format.eprintf "BnB-ing rec. call info=@[<h 1>%a@] on env %a@ "
                pp_info info
                (* (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt "@ ") pp_info) info *)
                pp_env env;
              (* Format.eprintf "Bnb-ing env %a:@.Current elems: %a@."
               *   fprint
               *   (Env env)
               *   (Format.pp_print_list
               *      (fun fmt (info,v) -> Format.fprintf fmt "%s -> %a" info D.fprint v))
               *   elem_list
               * ; *)
              if depth > 15 then (
                if !bnb_log then Format.eprintf "Max depth: stop (nb items: %i)@ " (List.length elem_list);
                (info, e_val) :: elem_list (* We stop *)
              )
              else
                begin
                  (* Returns a list of sub env *)
                  let split_env_list = bnb_split (get_bounds env) env in
                  (* In one pass, we
                     - evaluate on the sub-env
                     - compute the associated bound,
                     - join the element, and join the bounds         
           
           Returns: joined bounds, the list of abstract values, 
           in case we are happy and want to stop.
                   *)
                  let new_val_bounds, split_env_with_bounds =
                    Utils.fold_left'
                      (fun ((split_info:string), split_env) ->
                        let e_val = eval_expr split_env e in
                        let e_val_bounds = D.to_bounds e_val in (* bounding the expression *)
                        e_val_bounds, [split_info, e_val, e_val_bounds, split_env]
                      )
                      (fun (b1, split_l1) (b2, split_l2) ->
                        Bounds.join b1 b2,  (split_l1 @ split_l2)
                      )
                      split_env_list
                  in
                  let elem_list' =
                    (List.map (fun ((split_info:string), e_val, _, _) -> split_info::info, e_val)
                       split_env_with_bounds)@elem_list
                  in
                  (* New bounds are supposed to be more precise *)
                  let distance = Bounds.compare old_val_bounds new_val_bounds in
                  if distance <= 0. then (* Either they are equal or we did not improve. Let's stop *)
                    (* Return the join of all elements *)
                    begin
                    (*Utils.fold_left' (fun x -> x) D.join*)
                      if !bnb_log then Format.eprintf "Distance is positive. No improvement. Was %a, is now %a.@ Stopping.@ "
                        Bounds.pp old_val_bounds Bounds.pp new_val_bounds;
                    elem_list'
                    end
                  else
                    begin
                      (* Recursive call on each element of split_eval *)
                      if !bnb_log then Format.eprintf "Distance negative. Could be improved.@ ";
                      List.fold_left
                      (fun accu (split_info, e_val, split_bounds, split_env)  ->
                        bnb_eval ~info:(split_info::info) ~depth:(depth+1) e_val split_bounds split_env accu
                      )
                      elem_list
                      split_env_with_bounds
                    end
                end
            in
            let e_val = eval_expr env e in
            let e_val_bounds = D.to_bounds e_val in (* bounding the expression *)
            let abstract_values_list = bnb_eval ~depth:0 e_val e_val_bounds env [] in
            (* We compute the join to return the final value. *)
            Format.eprintf "Init range: %a@ " Bounds.pp e_val_bounds;
            if !bnb_log then Format.eprintf "@[Computed %i cells:@ @[%a@]@]@ " (List.length abstract_values_list)
              (Format.pp_print_list (fun fmt (info,v) -> Format.fprintf fmt "%a -> %a" pp_info info D.fprint v)) abstract_values_list
            ;
              Format.eprintf "Final range: %a@ @]@ "
                Bounds.pp
                (Utils.fold_left' (fun (_,x) -> D.to_bounds x) Bounds.join abstract_values_list);
            
            Utils.fold_left' (fun (_,x) -> x) D.join abstract_values_list
          end
      end
    
  let assignment n e t = 
    if Typing.are_types_eq e.Ast.expr_type D.base_type then (
      match t with
      | EnvBot -> EnvBot
      | Env m ->
        let t = eval_expr m e in
        if order_print 6 t D.bottom then EnvBot
        else if order_print 6 D.top t then Env (Name.Map.remove n m)
        else Env (Name.Map.add n t m)
    )
    else
      (* let _ =
       *   Format.eprintf "Domain %s (%a) does not handle assigns of expression of type %a@."
       *     name
       *     Ast.pp_base_type D.base_type
       *     Ast.pp_base_type e.Ast.expr_type
       * in *)
      t

  (* TO DO *)
  let backward_assignment _ _ _ _ _ = assert false

  let read_input (t : t) (vars : Ast.Var.t list) : t = 
    let expr = Scenario.Scenario.create_rand_expr (-1.) 1. Ast.RealT in
    List.fold_left (fun acc (name, _) -> assignment name expr acc) t vars

  let read_state t _ = t
	    
let rec backeval_expr env e t =

  match e.Ast.expr_desc with
  | Ast.Cst (b,bs) ->
      backeval_expr env (Ast.mk_expr e.Ast.expr_loc e.Ast.expr_type (Ast.Rand ((b,bs), (b,bs)))) t
        
    | Ast.Var n ->
      let t' = find_or_top n env in
      let t = meet_print t t' in
      if order_print 6 t D.bottom then raise Exit
      else if order_print 6 D.top t then env
      else Name.Map.add n t env
    | Ast.Unop _ -> assert false (* see fwd_sem *)
    | Ast.Binop (bop, e1, e2) ->
      (* not very efficient,
       * but guards should not be very large anyway *)
      let t1 = eval_expr env e1 in
      let t2 = eval_expr env e2 in
      let s, backsem = match bop with
        | Ast.Plus -> "backsem_plus", D.backsem_plus
        | Ast.Minus -> "backsem_minus", D.backsem_minus
        | Ast.Times -> "backsem_times", D.backsem_times
        | Ast.Div -> "backsem_div", D.backsem_div
        | _ -> assert false (* see fwd_sem *)
      in
      let t1', t2' = backsem t1 t2 t in
      nlogf 5 "%s@ %a@ %a@ %a@ = @[%a,@ %a@]."
        s D.fprint t1 D.fprint t2 D.fprint t D.fprint t1' D.fprint t2';
      backeval_expr (backeval_expr env e1 t1') e2 t2'
    | Ast.Rand _ ->
        let t' = eval_expr env e in
        let t = meet_print t t' in
        if order_print 6 t D.bottom then raise Exit
        else env
    | Ast.Call _ (*(f, args) *) -> (* TODO *) env
    | Ast.Cond _ -> (* TODO guard e env *) env

    | Ast.ShiftLeft _
    | Ast.ShiftRight _ -> env (* TODO *)

    | Ast.FxpConv _ -> assert false (* TODO *)


  let rec guard expr t =
    nlogf 5 "Computing guard %a in %a@." Ast.fprint_expr expr fprint t;
    
    match expr.Ast.expr_desc with
    | Ast.Cond (e,sl) -> (
      if Typing.are_types_eq e.Ast.expr_type D.base_type then (
        match t, sl with
	      | EnvBot, _ -> EnvBot
        | Env _, Ast.NonZero -> (
          nlogf 5 "Don't expect anything precise with e <> 0"; (* Don't expect anything precise with e <> 0 *)
          let less = guard {expr with expr_desc = Ast.Cond (e, Ast.Strict)} t in
          let more = guard {expr with expr_desc = Ast.Cond (Ast.opposite e, Ast.Strict)} t in
          let res = join less more in
          nlogf 5 "<0: %a@ cup@ >0: %a@ = %a." fprint less fprint more fprint res; 
          res
        )
      	| Env m , _ -> (
          let t' = 
            match sl with 
            | (Ast.Strict | Ast.Loose) -> 
	       let pre_expr, post_val = 
	         (match base_type, sl with
	          | (Ast.IntT | Ast.FixedT _ | Ast.MIntT _), Ast.Strict -> (* e > 0, ie. e >= 1, ie. e-1 >= 0 *) 
	             let minus_one = Ast.mk_cst_expr e.Ast.expr_loc e.Ast.expr_type (Q.minus_one, "-1") in
	             let f op e = Ast.mk_expr e.Ast.expr_loc Ast.IntT (Ast.Binop (op, e, minus_one)) in
	             (f Ast.Plus), (fun x -> D.sem_plus x (D.sem_itv (Q.one, "1") (Q.one, "1")))
	          | (Ast.IntT | Ast.FixedT _ | Ast.MIntT _), Ast.Loose (* e >= 0 *) | Ast.RealT, _ ->
                   (fun id -> id), (fun id -> id)
	          | _ -> Format.eprintf "Issues computing guard for expression %a@.@?" Ast.fprint_expr expr; assert false)
	       in
	       let t = eval_expr m (pre_expr e) in
	       let t' = post_val (D.sem_geq0 t) in
               t'
            | Ast.Zero ->
               let t = eval_expr m e in
               let zero = let z = Q.zero, "0" in D.sem_itv z z in
	       let t' = D.meet t zero in
               t'
            | Ast.NonZero -> assert false (* should be done earlier *)
          in
	  nlogf 5 "sem_guard %a%s0 = %a." D.fprint (eval_expr m e) (Ast.string_of_cmp sl) D.fprint t';
	  try
            Env (backeval_expr m e t')
	  with Exit -> EnvBot
      ))
      else
	t
    )
   
    | Ast.Cst (_, "true") -> t
    | Ast.Cst (_, "false") -> EnvBot
    | _ -> (* all other conditions return t. They are widely over-approximated 
 *)
       t
      (*
    | Ast.Var n -> (* Such constraint is not handled here and shall be
                      converted into a numerical constraints. See
                      BoolActivate *)
       t
    | Ast.Unop (Not, e) when match e.expr_desc with Ast.Var _ -> true | _ -> false  -> t
                                                    | Ast.Binop (
    | _ -> Format.eprintf "Unable to evaluate the expression %a used in guard.@.@?"
             Ast.fprint_expr expr; assert false

		      (* let guard_real e1 e2 t = match t with *)
		      (*   | EnvBot -> EnvBot *)
		      (*   | Env m -> *)
		      (*     let t1, t2 = eval_expr m e1, eval_expr m e2 in *)
		      (*     let t1', t2' = D.backsem_le t1 t2 in *)
		      (*     nlogf 5 "backsem_le@ %a@ %a@ = @[%a,@ %a@]." *)
		      (*       D.fprint t1 D.fprint t2 D.fprint t1' D.fprint t2'; *)
		      (*     try *)
		      (*       let t1'' = Env (backeval_expr m e1 t1') in *)
		      (*       let t2'' = Env (backeval_expr m e2 t2') in *)
		      (*       meet t1'' t2'' *)
		      (*     with Exit -> EnvBot *)
		      end
       *)
  
  (* The value of the expr is bottom *)
  exception BottomStop
  
  let project_values t1 t2 l_relation =
    match t1, t2 with
     | EnvBot, _ | _, EnvBot -> EnvBot
     | Env m1, Env m2 -> (
      try
        (* Evaluate the expr in t1 then associate it to the var corresponding in t2 *)
        let new_m2 = List.fold_left (fun m (e,(n,typ)) -> 
          if Typing.are_types_eq typ D.base_type then
            let t = eval_expr m1 e in
            if D.order t D.bottom then raise BottomStop
            else if D.order D.top t then m
            else Name.Map.add n t m
          else m
        ) m2 l_relation in
        Env new_m2
      with BottomStop -> EnvBot
     )

end
