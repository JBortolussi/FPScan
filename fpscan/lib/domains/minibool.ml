(* Template to write your own non relational abstract domain. *)

let name = "MiniBool"

let base_type = Ast.BoolT

(* no option *)
let parse_param _ = ()

let fprint_help fmt = Format.fprintf fmt "Basic abstraction of booleans"
let log = false

(* To implement your own non relational abstract domain,
 * first give the type of its elements, *)
type t = Bot | Top | T | F

(* a printing function (useful for debuging), *)
let fprint ff = function
  | Bot -> Format.fprintf ff "⊥"
  | T -> Format.fprintf ff "T"
  | F -> Format.fprintf ff "F"
  | Top -> Format.fprintf ff "⊤"

(* the order of the lattice. *)
let order x y = match x, y with
  | Bot, _ | _, Top | F,F | T,T -> true
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
  | Top, _ | _, Top| F, T | T,F -> top
  | Bot, Bot -> Bot
  | Bot, x | x, Bot -> x
  | F, F -> F
  | T, T -> T

let meet x y = match x, y with
  | Bot, _ | _, Bot | T, F | F, T -> Bot
  | Top, x | x, Top -> x
  | _ -> if x = y then x else Bot

let widening = join 

let sem_itv (n1,_) (n2,_) =
  if Q.equal n1 n2 then
    if Q.equal n1 (Q.of_int 0) then F else T 
  else
    Top

let sem_plus _ (*x*) _ (* y *) = top
let sem_minus _ (*x*) _ (* y *) = top
let sem_times _ (*x*) _ (* y *) = top
let sem_div _ (*x*) _ (* y *) = top

(* This is used to enforce positivity (it is more >0 than >=0 here *) 
let sem_geq0 = function
  | Top | T -> T
  | F | Bot -> Bot

let sem_call _ _ = top

let backsem_plus x y _ (*r*) = x, y
let backsem_minus x y _ (*r*) = x, y
let backsem_times x y _ (*r*) = x, y
let backsem_div x y _ (*r*) = x, y
