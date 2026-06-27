(* TODO 
- join/meet without Itv
- div without Itv: should we use MUTE ?
- add errors in a generic way
*)
module Make () =
  struct

    let name = "affine"

    let base_type = Ast.RealT
                  
    (* no option *)
    let parse_param _ = ()

    let fprint_help fmt = Format.fprintf fmt "Affine forms"
    let log = false

    type eps_elt = int * float
    type lin_form = eps_elt list
    type aff_t = float * lin_form 
    type t = Bot | Top | AF of aff_t

    module Itv = Intervals.Double

    let new_counter = let cpt = ref (-1) in fun () -> incr cpt; !cpt

    (* Convert affine forms to intervals and back *)
    let radius l =
      (* Reordering elements to minimise summation errors *)
      let l = List.sort (fun (_,v1) (_,v2) -> compare v1 v2) l in 
      List.fold_left (fun res (_,v) -> res +. abs_float v) 0. l

    let aff_to_int (c,l) =
      let radius = radius l in
      Itv.of_bounds (c -. radius) (c +. radius)

    let to_bounds x =
      match x with
      | Bot -> raise Utils.IsBottom
      | Top -> Itv.to_bounds Itv.top
      | AF x -> let itv = aff_to_int x in Itv.to_bounds itv
                                               
    let float_float_to_aff a b =
      if a = b then
        a, []
      else
        let c = (a +. b) /. 2. in
        let r = (b -. a) /. 2. in
        c, [new_counter (), r]
        
    let int_to_aff x =
      let a,b = Itv.bounds x in
      AF (float_float_to_aff a b)

    let pp_raw ff (c,l) =
      Format.fprintf ff "%f + %a"
        c
        (Utils.fprintf_list ~sep:" + "
           (fun fmt (i,v) -> Format.fprintf fmt "%f e%i" v i))
        l
      
    let fprint ff = function
      | Bot -> Format.fprintf ff "⊥"
      | Top -> Format.fprintf ff "⊤"
      | AF (c, l) ->
         pp_raw ff (c,l);
         Itv.fprint ff (aff_to_int (c,l))

    let json = function
      | Bot -> `String "⊥"
      | Top -> `String "⊤"
      | AF (c, l) ->
         (* pp_raw ff (c,l) *)
         `String (Format.asprintf "%a" Itv.fprint (aff_to_int (c,l)))

    (* Knowledge about internal representation *)
    (* The following functions are aware of the interval representation: l is sorted by decreasing indices:
  merge f l1 l2: (float -> float -> float) -> 
  add  *)
      
    let merge f l1 l2 =
      let f v1opt v2opt =
        match v1opt, v2opt with
        | None, None -> assert false
        | None, Some v | Some v, None -> v
        | Some v1, Some v2 -> f v1 v2
      in
      let add_nz i r l =
        if r = 0. then l else (i,r)::l
      in
      let rec aux l1 l2 accu =
        match l1, l2 with
        | [], [] -> accu
        | [], (i,e)::l ->
           let r = f None (Some e) in
           let accu = add_nz i r accu in
             aux [] l accu
        | (i,e)::l, [] ->
           let r = f (Some e) None in
           let accu = add_nz i r accu in
             aux l [] accu
        | (i1,e1)::l1', (i2,e2)::l2' ->
           if i1 = i2 then
             let r = f (Some e1) (Some e2) in
             let accu = add_nz i1 r accu in
             aux l1' l2' accu
           else if i1 > i2 then
             let r = f (Some e1) None in
             let accu = add_nz i1 r accu in
             aux l1' l2 accu
           else
             let r = f None (Some e2) in
             let accu = add_nz i2 r accu in
               aux l1 l2' accu
      in List.rev (aux l1 l2 [])

    (* Add  fresh term *)
    let add v l =
      let cpt = new_counter () in
      match l with
        [] -> [new_counter() ,v]
      | (i',_ (* UNUSED: v' *))::_ ->
         assert (cpt > i');
         (cpt, v)::l
         
         
    (* the order of the lattice: we project to intervals for the moment *)
    let order x y = match x, y with
      | Bot, Bot | Top, Top | Bot, Top 
        | Bot, AF _ | AF _ , Top -> true
      | AF x, AF y -> Itv.order (aff_to_int x) (aff_to_int y)
      | _ -> false
           
    (* and infimums of the lattice. *)
    let top = Top
    let bottom = Bot
    let is_bottom x = x = Bot
                    
    (* All the functions below are safe overapproximations.
     * You can keep them as this in a first implementation,
     * then refine them only when you need it to improve
     * the precision of your analyses. *)

    let join x y = match x, y with
      | Top, _ | _, Top -> top
      | AF x, Bot | Bot, AF x -> AF x
      | AF x, AF y -> (* Converting to intervals. Could be really improved *)
         int_to_aff (Itv.join (aff_to_int x) (aff_to_int y))
      | _ -> bottom

    let meet x y = match x, y with
      | Top, Top -> top
      | AF x, Top | Top, AF x -> AF x
      | AF x, AF y ->  int_to_aff (Itv.meet (aff_to_int x) (aff_to_int y))
      | _ -> Bot

    let widening = join  

    let sem_itv (n1, _) (n2, _) =
      AF(float_float_to_aff 
           (Q.to_float n1)
           (Q.to_float n2)
        )
      

    let plus_minus op x y =
      match x, y with
      | Bot, _ | _ , Bot -> bottom
      | Top, _ | _, Top -> top
      | AF (c1,l1), AF (c2,l2) ->
         let aff =
           op c1 c2,
           merge op l1 l2
         in
         AF aff
         
    let sem_plus x y = plus_minus (+.) x y   
    let sem_minus x y = plus_minus (-.) x y
                      
    let sem_times x y =
      match x, y with
      | Bot, _ | _ , Bot -> bottom
      | Top, AF (0.,[]) | AF (0.,[]), Top ->
         (* if x = 0 then 0 else Top *)
         AF (0., [])
      | Top, _ | _, Top -> top
      | AF (c1,[]), AF (c2, l2)
        | AF (c2, l2), AF(c1,[]) ->
         AF(c1 *. c2, List.map (fun (i, v) -> i, c1 *. v) l2)
      | AF (c1,l1), AF (c2, l2) ->
         let c = c1 *. c2 in
         (* First order terms *)
         let c2l1 = List.map (fun (i,v) -> i, c2 *. v) l1 in
         let c1l2 = List.map (fun (i,v) -> i, c1 *. v) l2 in
         let fst_l = merge (+.) c2l1 c1l2 in
         (* Higher order *)
         let fresh_val = (radius l1) *. (radius l2) in
         let l' = add fresh_val fst_l  in
         AF(c, l')

    (* IF int(x) contains 0 then Top else *)
    let sem_inv x =
      match x with
      | Bot -> Bot
      | Top -> Top
      | AF (c, l) ->
         let r = radius l in
         if c -. r <= 0. && c +. r >= 0. then
           Top
         else (* we could be more precise *)
           let itv_1 = Itv.sem_itv (Q.of_int 1, "") (Q.of_int 1, "") in
           int_to_aff (Itv.sem_div itv_1 (aff_to_int (c,l))) 
           
    let sem_div x y = sem_times x (sem_inv y)
                    

    let sem_geq0 x =
      match x with
      | Top | Bot -> x
      | AF x -> int_to_aff (Itv.sem_geq0 (aff_to_int x))

    let sem_call _ (* UNUSED: f*) _ (* UNUSED: args *) = top (*
      let fv = nb_free_vars e in
      let module LocalMute = Poly_mute.Make(struct
                                 let max_vars = fv
                                 let max_deg = 1
                               end)
      in
      let args_poly = List.map LocalMute.import args in
      let img_poly = LocalMute.sem_call f args_poly in
      let res = LocalMute.export 1 img_poly in
      res
                               *)
    let backsem_plus x y _ (* UNUSED: r *) = x, y
    let backsem_minus x y _ (* UNUSED: r *) = x, y
    let backsem_times x y _ (* UNUSED: r *) = x, y
    let backsem_div x y _ (* UNUSED: r *) = x, y

    let to_properties _x = Value_properties.empty (* TODO *)

    let split _ (* UNUSED: x *) = assert false (* TODO *)
  end

module Main = Make ()
