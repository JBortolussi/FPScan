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

module Make (I: sig val name_suffix: string val base_type : Ast.base_type end) =
struct
let name = "kildall" ^ I.name_suffix

let base_type = I.base_type

(* no option *)
let parse_param _ = ()

let fprint_help fmt = Format.fprintf fmt "Kildall abstraction"
let log = false

type t = Bot | Cst of Q.t | Top

let fprint ff = function
  | Bot -> Format.fprintf ff "⊥"
  | Cst n -> Format.fprintf ff "%a" Q.pp_print n
  | Top -> Format.fprintf ff "⊤"

let json = function 
  | Bot -> `String "⊥"
  | Cst n -> `String (Format.asprintf "%a" Q.pp_print n)
  | Top -> `String "⊤"

let order x y = match x, y with
  | Bot, _ -> true
  | _, Top -> true
  | Cst n1, Cst n2 -> Q.equal n1 n2
  | _ -> false

let top = Top
let bottom = Bot
let is_bottom x = x = Bot

let join x y = match x, y with
  | Top, _ | _, Top -> Top
  | Bot, _ -> y
  | _, Bot -> x
  | Cst n1, Cst n2 -> if Q.equal n1 n2 then x else Top

let meet x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Top, _ -> y
  | _, Top -> x
  | Cst n1, Cst n2 -> if Q.equal n1 n2 then x else Bot
let widening = join

let sem_itv (n1,_) (n2,_) = 
    if Q.gt n1 n2 then Bot else if Q.equal n1 n2 then Cst n1 else Top
								  

let sem_geq0 = function
  | Bot -> Bot
  | (Cst n) as c -> if Q.geq n Q.zero then c else Bot
  | Top -> Top

let sem_call f args =
  if List.exists is_bottom args then bottom else
    match f, args with
    | _ -> top (* not handled yet: TODO *)
			    
let sem_op op x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Top, _ | _, Top -> Top
  | Cst n1, Cst n2 -> try Cst (op n1 n2) with _ -> Top

(* let inf_int_op lb ub x y = *)
(*   let x = InfInt.fin x in *)
(*   let y = InfInt.fin y in *)
(*   let l = lb x y in *)
(*   let u = ub x y in *)
(*   if InfInt.eq l u then InfInt.to_int l else None *)

let sem_plus = sem_op Q.add
let sem_minus = sem_op Q.sub
let sem_times = sem_op Q.mul

let sem_div x y = 
  match base_type with
  | Ast.IntT | Ast.MIntT _ -> (
      try 
	sem_op (fun x y -> Q.of_bigint (Z.div (Q.to_bigint x) (Q.to_bigint y) )) x y
      with  Division_by_zero -> Bot)
      
  | Ast.RealT -> sem_op Q.div x y
  | Ast.BoolT -> assert false
  | Ast.FixedT _ -> assert false
     (* TO BE IMPLEMENTED
        it might depend on the underlying representation.
        Shall we use integers and compute the fixpoint operation: x * 2^f / y where f is the target precision and .
        Or shall we keep the 

        NO, this transformation of the ast is done at typing. Then it
        is regular integer programming

     *)
    
  
(* We could have more precise backward abstract semantics,
 * but that would be completely useless. *)
let backsem_op x y r = match x, y, r with
  | Bot, _, _ | _, Bot, _ | _, _, Bot -> Bot, Bot
  | _ -> x, y
let backsem_plus = backsem_op
let backsem_minus = backsem_op
let backsem_times = backsem_op
let backsem_div = backsem_op

let to_bounds x = match x with
  | Cst q -> let s = Scalar.of_q q in Bounds.mk s s
  | _ -> Bounds.mk (Scalar.of_q Q.minus_inf) (Scalar.of_q Q.inf)
           
let to_properties _x = Value_properties.empty (* TODO *)
let split _ = None
end

module Int = Make (struct let name_suffix = "_int" let base_type = Ast.IntT end)
module Real = Make (struct let name_suffix = "_real" let base_type = Ast.RealT end)
