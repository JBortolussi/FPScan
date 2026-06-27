(* The objective is to extend existing domains with the capability to
   interact with boolean expression.

   Instead of focusing only on integer or real expressions, this new
   domain shall also record booleans expressions and rely on stored
   information to perform additional reduction. It acts as a reduced
   product.

 *)
open Ast
   
module BoolEnv =
  struct

    (* We need boolean dedidacted expression to ease analysis and comparison *)
    (* binary term *)
    type bt =
      | NumCond of (expr * cmp)
      | BoolVar of Name.t 
      | NotBoolVar of Name.t
      | BoolCst of bool
      | BAsn of Name.t * bt
      | BAnd of bt * bt  (* Simple solution first: recording as_is. In
                            future devs, one could split open the And
                            and just keep the Or *)
      | BOr of bt * bt
             
    exception BoolCstDef of bool
    exception BoolOpNotHandledYet

    (* Pretty print binary terms *)        
    let rec pp_bt fmt expr =
        match expr with 
      | NumCond (e,c) -> fprint_expr fmt (mk_cond e.expr_loc e c)
      | BoolVar (n) -> Format.fprintf fmt "%s" n
      | NotBoolVar (n) -> Format.fprintf fmt "not (%s)" n
      | BoolCst b -> Format.fprintf fmt "%b" b
      | BAsn(n,e) -> Format.fprintf fmt "%s = %a" n pp_bt e
      | BAnd(e1,e2) -> Format.fprintf fmt "%a and %a" pp_bt e1 pp_bt e2
      | BOr(e1,e2) -> Format.fprintf fmt "%a or %a" pp_bt e1 pp_bt e2
      
    (* Negation of a binary expression *)
    let rec neg_be e =
      
      match e with
      | BAsn _ -> e
      | BoolCst b -> BoolCst (not b) 
      | NumCond (e,c) -> NumCond(e, neg_cmp_op c)
      | BoolVar n -> NotBoolVar n
      | NotBoolVar n -> BoolVar n
      | BAnd (e1, e2) -> BOr (neg_be e1, neg_be e2)
      | BOr (e1, e2) -> BAnd (neg_be e1, neg_be e2)
                      
    (* This domain records both boolean variable definition and set of
       (in)valid expressions. We use a pair of stm set and expr set *)  
    module S =
      Set.Make (struct type t = bt let compare = compare end)
             
    (* Recording expressions. For the moment. we only record
       conjunctions of numerical constraints. More sophisticated
       combination will require more advanced types *)
    type t = C (* onstraints *) of S.t | Bot
           
    let fprint fmt benv =
      match benv with
      | C senv ->
         Format.fprintf fmt "@[{%a}@]"
           (Utils.fprintf_list ~sep:";@ " pp_bt) (S.elements senv)
      | Bot -> Format.fprintf fmt "⊥"

               
    let json benv =
      match benv with
      | C senv ->
        `List (
          List.map (fun elt -> 
            `String (Format.asprintf "%a" pp_bt elt)) 
          (S.elements senv)
        )
      | Bot -> `String "⊥"
      
      
    let order b1 b2 =
      match b1, b2 with
      | Bot, _ -> true
      | C s1, C s2 ->
         (* all constraints of b2 are in b1, hence b2 is more general *)
         S.subset s2 s1 
      | _ -> true
           
    let top _ (* UNUSED: vars *) = C (S.empty) 
    let bottom _ (* vars *) = Bot
    let is_bottom b = b = Bot

    (* Don't really know when it is used. For the moment, return the
       variable that correspond to definitions. *)
    let get_vars b =
      match b with
      | Bot -> Ast.Var.Set.empty
      | C senv ->
         List.fold_left (fun accu stm ->
             match stm with
             | BAsn (v, _) -> Ast.Var.Set.add (v, Ast.BoolT) accu
             | _ -> accu (* No need to recursively address And and Or
                            since they are not supposed to contain
                            assignements expressions *)
           ) Ast.Var.Set.empty (S.elements senv)

    let rec get_var_uses bt =
      match bt with
      | BAsn (v, _)  
        | BoolVar v
        | NotBoolVar v -> Ast.Var.Set.singleton (v, Ast.BoolT)
      | BAnd(b1,b2) | BOr(b1,b2) -> Ast.Var.Set.union (get_var_uses b1) (get_var_uses b2)
      | _ -> Ast.Var.Set.empty
      
    let get_def v s =
      let res =
        S.fold
          (fun be res ->
            match res, be with
            | Some e, _ -> Some e
            | None, BAsn(v',e') -> if v = v' then Some e' else None
            | _ -> res
          ) s None 
      in
      match res with Some e -> e | None -> raise Not_found
                                            
    (* returns a pair (s,e) of set of statements, set of expressions:
       will be used in join, meet, guard and assign *)
    let split_env s =
      S.partition
        (fun stmt -> match stmt with BAsn _ -> true | _ -> false )
        s

    (* Merge s1 and s2. For each element e1 of s1, we gather elements
       e2 of s2 such that test e e2.

       Then we apply /apply/ on e1 and selected e2 elements: apply e1
       e2l accu *)
    let merge test apply s1 s2 =
      S.fold (fun e1 (accu,reminder_s2) ->
          let e2l, others = S.partition (test e1) reminder_s2 in
          (apply e1 e2l accu, others)
        ) s1 (C S.empty, s2)

    let join, meet =
      let test_same_var_assign =
        (fun e1 e2 ->
          match e1, e2 with
          | BAsn(v1,expr1), BAsn(v2, expr2) ->
             v1 = v2 && not(expr1 = expr2)
          | _ -> false
        )
      in
      let test_neg_assertion =
        (fun elem1 elem2 ->
          match elem1, elem2 with
          | BAsn _, _ | _, BAsn _ -> false
          | _, _ ->
             let neg_elem1 = neg_be elem1 in
             elem2 = neg_elem1
        )
      in
      let join b1 b2 = match b1, b2 with
        | Bot, b | b, Bot -> b
        | C s1, C s2 ->
           let s1, e1 = split_env s1 in
           let s2, e2 = split_env s2 in

           (* Removing incompatible assigns, ie v = e1 join v = e2 *)
           let sset, s2_reminder =
             merge
               test_same_var_assign
               (fun e1 e2l accu ->
                 match accu with
                   Bot -> assert false
                 | C accu ->
                    if S.cardinal e2l > 0 then (* don't store anything *)
                      C accu
                    else
                      C (S.add e1 accu)
               )
               s1 s2     
           in
           let sset = match sset with C sset -> S.union sset s2_reminder | Bot -> assert false in
           
           (* Filtering simple tautotogies: we do not address any boolean
            operator, only the simplest cases: (e1) join (not e1)
            
            TODO:  deal with And/Or/Not 
            *)
           let eset, e2_reminder =
             merge
               test_neg_assertion
               (fun elem1 elem2l accu ->
                 if S.cardinal elem2l > 0 then
                   accu
                 else
                   match accu with
                     Bot -> assert false
                   | C accu -> C (S.add elem1 accu)
               )
               e1 e2
           in
           let eset = match eset with C eset -> S.union eset e2_reminder | Bot -> assert false in

           C (S.union sset eset)
      in
      let meet b1 b2 = 
        (* Similar to join, in case of incompatible *)
        match b1, b2 with
        | Bot, _ (* UNUSED: b *) | _ (* b *), Bot -> Bot
        | C s1, C s2 ->
           let s1, e1 = split_env s1 in
           let s2, e2 = split_env s2 in

           (* Bot when incompatible assigns *)
           let sset, s2_reminder =
             merge
               test_same_var_assign
               (fun e1 e2l accu ->
                 match accu with
                   Bot -> Bot
                 | C accu ->
                    if S.cardinal e2l > 0 then (* don't store anything *)
                      Bot
                    else
                      C (S.add e1 accu)
               )
               s1 s2     
           in
           let sset = match sset with C sset -> C(S.union sset s2_reminder) | Bot -> Bot in
           
           (* e1  AND not e1 returns bot *)
           let eset, e2_reminder =
             merge
               test_neg_assertion
               (fun elem1 elem2l accu ->
                 if S.cardinal elem2l > 0 then
                   Bot
                 else
                   match accu with
                     Bot -> Bot
                   | C accu -> C (S.add elem1 accu)
               )
               e1 e2
           in
           let eset = match eset with
             | C eset -> C(S.union eset e2_reminder)
             | Bot -> Bot
           in
           match sset, eset with
           | Bot, _ | _, Bot -> Bot
           | C sset, C eset -> C (S.union sset eset)
      in
      join, meet

    let rec conv_expr_to_be env expr =
      let get_def n =
        match env with
          Bot -> raise Not_found
        | C set -> get_def n set 
      in
      let conv_expr_to_be = conv_expr_to_be env in
      match expr.expr_desc with
      | Var n -> (try get_def n with Not_found -> BoolVar n)
      | Cond(e, sl) -> (
        match e.expr_desc, e.expr_type, sl with
        | Var n, BoolT, Zero -> (
           try
             let e = get_def n in
             neg_be e
           with Not_found -> NotBoolVar n
        )
        | _, (IntT | RealT | FixedT _ | MIntT _ | BoolT), _ -> NumCond(e, sl)
      )
      | Cst (_, "true") -> BoolCst true
      | Cst (_, "false") -> BoolCst false
      | Rand _ -> raise (Invalid_argument "random range to define a boolean variable")
      | Unop(Not, e1) -> neg_be (conv_expr_to_be e1)
      | Binop(And, e1, e2) -> BAnd(conv_expr_to_be e1, conv_expr_to_be e2)
      | Binop(Or, e1, e2) -> BOr(conv_expr_to_be e1, conv_expr_to_be e2)
      | Binop(Eq, e1, e2) -> if e1.expr_type = BoolT then
                               let conv1 = conv_expr_to_be e1 and
                                   conv2 = conv_expr_to_be e2 in
                               BOr(BAnd(conv1,conv2), BAnd(neg_be conv1, neg_be conv2))
                             else NumCond(Ast.sub e1 e2, Zero)                      
      | Call(f,_) -> 
        (* Creating a variable per function call
        this give no information to the analyser 
        TODO : 
          * handle the cases where function have a binary expression
        so that Tiny can use this fact
        *)
        BoolVar ("call_"^f^string_of_int (Random.int 16777216))
      | _ -> Format.eprintf "Unable to convert expression %a (%a)@.@?" Ast.fprint_expr expr Ast.pp_base_type expr.expr_type; assert false

    let conv_be_to_expr be =
      match be with
      | BoolVar _ | NotBoolVar _ | BAsn _ | BoolCst _ -> assert false
      | BAnd _ | BOr _ -> assert false (* should be filtered out by now *)
      | NumCond (e, sl) -> mk_cond e.expr_loc e sl
                                                                
    (* First, simplify expr according to information in b. Then store v = expr in the new b. *)
    let assignment v expr b =
      if expr.expr_type = BoolT then
        begin
          (* Store the assignement. Remove existing one with the same variable.
             TODO: shall we rewrite it according to existing assigns and asserts ? 
           *)
          (* Format.eprintf "Recording boolean assignement@."; *)
          match b with
          | Bot -> Bot
          | C set ->
             try
               let is_same_def_or_use_same_var =
                 (fun stmt -> match stmt with BAsn(v', expr') -> (v = v' || (Ast.Var.Set.mem (v, Ast.BoolT) (get_var_uses expr')) )| _ -> false )
               in
               let _ (* definitions to remove *), remaining_definitions = S.partition is_same_def_or_use_same_var set in
                                                                         
               let new_stmt = BAsn(v, conv_expr_to_be (C remaining_definitions) expr) in
               C (S.add new_stmt remaining_definitions)
             with Invalid_argument _ -> b (* typically a random to define an input boolean variable *)
        end
          else
        b

    (* TO DO *)
    let backward_assignment _ _ _ _ _ = assert false

    let read_input bt _ = bt

    let read_state bt _ = bt

    (* Iterate over assign and store the associated numerical constraints *)
    let export_cons b =
      (* Format.eprintf "Exporting constraints of element %a@." fprint b; *)

      (* select cons be (ok, vs ,el) returns 
         - an updated ok status in case of true or false constant
         - an updated vs' set of (v,b) with v a variable and its used as b=v (true/false)
         - an updated set of numerical conditions

         OR operator arguments are neglected. This is still sound since we are not enforcing an element of a large conjunctive form.
       *)
      let rec select_cons asserts init =
        S.fold (fun be (ok, vs, el) ->
            match be with
            | BAsn _ -> ok, vs, el
            | BoolVar n -> ok, (n, true)::vs, el
            | NotBoolVar n -> ok, (n, false)::vs, el
            | NumCond (_ (* UNUSED: e *), _ (* UNUSED: sl *)) -> ok, vs, be::el
            | BOr _ -> (* Or constructs are neglected for the moment. Hard to deal with dijunction in conditions. TODO later *)
               ok, vs ,el
            | BAnd (e1, e2) -> (* And constructs can be /opened/ *)
               select_cons (S.of_list [e1; e2]) (ok, vs, el)
            | BoolCst b -> b && ok, vs, el
          ) asserts init
         
      in
      match b with
      | Bot -> false, []
      | C set ->
         let assigns, asserts = split_env set in
         (* Format.eprintf "assigns: %a@." (Utils.fprintf_list ~sep:", " pp_bt) (S.elements assigns); *)
         let ok, vars_sel, exprs =
           select_cons asserts (true,[],[])
         in
           
         let rec get_cons v (*seen*) =
           (*           if not (List.mem v seen) then*)
             let e = get_def v assigns in
             match e with
             | BoolVar n -> get_cons n (*v::seen*)
             | NotBoolVar n -> neg_be (get_cons n) (*v::seen*)
             | NumCond _ -> e
             | BoolCst b -> raise (BoolCstDef b)
             | BAnd _ | BOr _ -> raise BoolOpNotHandledYet
             | BAsn _ -> assert false
           (* else
            *   [] *)
         in
         let ok, selected_constraints =
           List.fold_left (fun (ok, accu) (v, posneg) ->
               try
                 let cons = get_cons v (*[]*) in
                 let cons = if posneg then cons else neg_be cons in
                 ok, cons::accu
               with
               | Not_found -> ok, accu
               | BoolCstDef b -> if (posneg && b) || ((not posneg) && (not b)) then ok, accu else false, []
               | BoolOpNotHandledYet -> ok, accu
             ) (ok, exprs) vars_sel
         in
          (* Format.eprintf "cons={%a}@." (Utils.fprintf_list ~sep:"@ " pp_bt) selected_constraints;  *)
         (* Exporting only numerical asserts *)
         ok, List.map conv_be_to_expr selected_constraints

    let guard expr b =
      (* Only store information involving a boolean variable, other
         guards have to be considered by individual domains. *)
      let be = conv_expr_to_be b expr in
      match be with
      | NumCond _ -> b, [expr] (* expression is directly a numerical
                              constraint. No need to look into our
                              boolean domain *)
      | _ -> 
         (* Recording expr as an assert in b *)
         let b' = meet (C (S.singleton be)) b in
         (* Format.eprintf "b' = %a inter %a = %a@.@?" fprint b fprint (C (S.singleton be)) fprint b'; *)

         let ok, other_num_cons = export_cons b' in
         (* Format.eprintf "guard ok: %b@.@?" ok; *)
         (* Exporting all numerical constraints in current element b' *)
         if ok then
           b', other_num_cons
         else
           Bot, []
      
    let project_values _ (* UNUSED: t1 *) _ (* UNUSED: t2 *) _ (* UNUSED: l_relation *) = 
        Format.eprintf "Project_values unimplanted for %s, please, inline the programe\n" "boolActivate";
        assert false

  end
  
module MakeR (D: Relational.Domain) : Relational.Domain =
  struct
    let name = D.name
    let nonrel_base = D.nonrel_base
    let is_partitioned () = false (* TODO *)
    let parse_param _ = ()
    let fprint_help _ (* UNUSED: fmt *) = ()

    type t = BoolEnv.t * D.t

    let fprint fmt (benv, e) =
      Format.fprintf fmt "@[%a,@ %a@]"
        BoolEnv.fprint benv
        D.fprint e

    let json (benv, e) = `Assoc ["domain_name", `String name; "bool_env" , (BoolEnv.json benv); "base_domain", (D.json e)]

    let order (b1,e1) (b2,e2) =
      BoolEnv.order b1 b2 && D.order e1 e2

    let get_vars (b,e) = Ast.Var.Set.union (BoolEnv.get_vars b) (D.get_vars e)

    let top vars =
      (* Format.eprintf "top {%a}@."Ast.Var.Set.pp vars; *)
      let top =     
        BoolEnv.top vars, D.top vars
      in
      (* Format.eprintf "get_vars {%a}@." Ast.Var.Set.pp (get_vars top); *)

      top
      
    let bottom vars = BoolEnv.bottom vars, D.bottom vars
    let is_bottom (b,e) = BoolEnv.is_bottom b || D.is_bottom e

    let join (b1,e1) (b2,e2) = BoolEnv.join b1 b2, D.join e1 e2
    let meet (b1,e1) (b2,e2) = BoolEnv.meet b1 b2, D.meet e1 e2
    let widening (b1,e1) (b2,e2) = BoolEnv.join b1 b2, D.widening e1 e2


    (* Record the assignement, and just apply it as well on the underlying domain D *)
    let assignment v expr (b,e) =
      BoolEnv.assignment v expr b, D.assignment v expr e


    let backward_assignment bo s v expr (b,e) =
      BoolEnv.backward_assignment bo s v expr b, D.backward_assignment bo s v expr e

    let guard expr (b,e) =
      (* Enforcing first the boolean side to discover more numerical
         constraints *)
      let b',exprl = BoolEnv.guard expr b in
      if BoolEnv.is_bottom b' then (
        (* Format.eprintf "guard bottom {%a}@."Ast.Var.Set.pp (D.get_vars e) ; *)
        let bot = D.bottom (D.get_vars e) in
        (* Format.eprintf "bottom= %a@." D.fprint bot ; *)
        b', bot
      )
      else
        let e' = List.fold_right D.guard exprl e in
        b', e'

    let to_bounds (_,e) = D.to_bounds e
    let to_properties (_,e) = D.to_properties e

    let read_input (b, e) vars = (BoolEnv.read_input b vars, D.read_input e vars)

    let read_state (b, e) vars = (BoolEnv.read_state b vars, D.read_state e vars)

    let project_values (_ (* UNUSED: b1 *),e1) (_ (* UNUSED: b2 *),e2) l_relation =
      BoolEnv.project_values e1 e2 l_relation, D.project_values e1 e2 l_relation
  end


   
    
