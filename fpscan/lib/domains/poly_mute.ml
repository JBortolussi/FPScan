(** TODO
- better join/meet
- geq0: project to interval than rebuild poly
- call: deal with basic elementary functions
- nb of variables should be a parameter of the module
- 
Uses:
- 
 *)

module Make () =
  struct
    
let name = "poly"

let base_type = Ast.RealT

let max_deg = 10
let max_vars = 1

module type INPUTS = sig
  type t'
  type t = t' Mute.Types.Nat.succ
  val isnat: t Mute.Types.Nat.isnat
end
                   
let rec build_input n : (module INPUTS) =
  if n <= 1 then
    let module M : INPUTS = Mute.Utils.N_1 in 
    (module  (M : INPUTS))
  else (
    let nminusone = build_input (n-1) in
    let module NMinusOne = (val nminusone : INPUTS) in
    (module (Mute.Utils.N_Succ (NMinusOne)))
  )
  
module Inputs = (val (build_input max_vars) : INPUTS)

  
let epsilons = Mute.Indices.Indices.fold Inputs.isnat [] (fun idx l -> idx :: l)

let new_eps =
  let cpt = ref 0 in
  fun () -> (
    if !cpt >= max_vars then assert false;
    Format.eprintf "new EPS!@.";
    let c = !cpt in
    incr cpt;
    List.nth epsilons c                
  )
          
module R = Mute.Values.Float (* Coeff types *)
module CTM = Mute.Ctm.Taylor (Inputs) (R)
           
let parse_param _ = ()

let fprint_help fmt = Format.fprintf fmt "Polynomial form abstraction"
let log = true

                    
type t = Bot | Top | Poly of CTM.t

let radius = R.of_float (1. /. 1000.)

let fprint ff = function
  | Bot -> Format.fprintf ff "⊥"
  | Top -> Format.fprintf ff "⊤"
  | Poly p -> CTM.force max_deg p; CTM.print_as_poly ~radius:radius ff p

          
(* the order of the lattice. For the moment we compare the projection
   to intervals. Later, we should compare at a given order (max deg). *)
let order x y = match x, y with
  | Bot, _ | _, Top  -> true
  | Poly _, Bot | Top, _ -> false
  | Poly p1, Poly p2 ->
     (* Evaluate both at order 0 and compare bounds *)
     List.iter (CTM.force 0) [p1; p2];
     true (* may stop too soon 
     assert false (* TODO *) *)
     
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
  | Bot, Bot -> bottom
  | Poly p , Bot | Bot, Poly p -> Poly p 
  | Poly p1, Poly p2 -> Top (* TODO assert false*)

let meet x y = match x, y with
  | Top, Top -> top
  | _, Bot | Bot, _ -> Bot
  | Poly p, Top | Top, Poly p -> Poly p
  | Poly p1, Poly p2 -> Bot (* TODO assert false*)

let widening = join  (* Ok, maybe you'll need to implement this one if your
                      * lattice has infinite ascending chains and you want
                      * your analyses to terminate. *)

let sem_itv (n1, _) (n2, _) =
  let a, b = Q.to_float n1, Q.to_float n2 in
  let center = (a +. b) /. 2. in
  if b > a then
    let radius = (b -. a) /. 2. in
    let fresh_var = CTM.var (new_eps ()) in
    let p = CTM.(of_float center + (of_float radius) * fresh_var) in
    Poly p
  else
    Poly (CTM.(of_float center))

let un_op f x = match x with
  | Bot -> Bot
  | Top -> Top
  | Poly p -> Poly (f p)
    
let bin_op f x y = match x,y with
  | Bot, _ | _, Bot -> Bot
  | Top, _ | _, Top -> Top
  | Poly p1, Poly p2 -> Poly (f p1 p2)
                      
let sem_plus  = bin_op CTM.(+)
let sem_minus = bin_op CTM.(-)
let sem_times = bin_op CTM.( * )
let sem_div   = bin_op CTM.(/)

let sem_geq0 = function
  | t -> t

let to_constant ctm =
  let _ = CTM.force 0 ctm in
  if Lazy.is_val (CTM.unT ctm) then
    match Lazy.force (CTM.unT ctm) with
    | Nil _ -> 0.
    | Cons (Nil, _) -> 0.
    | Cons (Leaf v, _) -> fst v
    | Cons (ve, _) -> (
      match ve with
        
        Leaf v -> fst v
    )
  else assert false
       
let sem_call f args =
  match f, args with
  | "pow", [x;i] ->
     (* Power is supposed to be a constant. Recovering the constant part *)
     bin_op (fun x i -> CTM.( x ** (to_constant i) )) x i
     
    | "sqrt", [x] -> un_op (fun x -> CTM.( x ** 0.5 )) x
    | "atan2", [x;y] -> assert false
    | "cos", [x] -> un_op CTM.cos x
    | _ -> assert false
  

let back_bin_op (fx,fy) x y r =
  match x,y,r with
  | _,_, Bot -> Bot, Bot
  | _, _, Top | Top, Top, Poly _ -> x, y
  | Bot, _, _
    | _, Bot, _ -> Bot, Bot
  | Poly p1, Poly p2, Poly pr -> Poly (fx p2 pr), Poly (fy p1 pr)
  | Top, Poly p2, Poly pr -> Poly (fx p2 pr), Poly p2
  | Poly p1, Top, Poly pr -> Poly p1, Poly (fy p1 pr)
    
let backsem_plus  = back_bin_op ((fun y r -> CTM.(r - y)),
                                (fun x r -> CTM.(r - x)))
let backsem_minus = back_bin_op ((fun y r -> CTM.(r + y)),
                                (fun x r -> CTM.(x - r)))
let backsem_times = back_bin_op ((fun y r -> CTM.(r / y)),
                                (fun x r -> CTM.(r / x)))
let backsem_div   = back_bin_op ((fun y r -> CTM.(r * y)),
                                (fun x r -> CTM.(x / r)))
end
  
module Main = Make ()
