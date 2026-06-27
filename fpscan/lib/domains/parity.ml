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

let name = "parity"

let base_type = Ast.IntT

(* no option *)
let parse_param _ = ()

let fprint_help fmt = Format.fprintf fmt "Parity abstraction for integer based variables"
let log = false

type parity = Bot | Even | Odd | Top

type t = parity

let fprint ff t = Format.fprintf ff "%s"
  (match t with
    | Bot -> "⊥"
    | Even -> "even"
    | Odd -> "odd"
    | Top -> "⊤")

let json = function
  | Bot -> `String "⊥"
  | Even -> `String "even"
  | Odd -> `String "odd"
  | Top -> `String "⊤"

let order x y = match x, y with
  | Bot, _
  | Even, Even | Even, Top
  | Odd, Odd | Odd, Top
  | Top, Top -> true
  | _ -> false

let top = Top
let bottom = Bot
let is_bottom x = x = Bot

let join x y = match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | Top, _
  | _, Top -> Top
  | Even, Even -> Even
  | Odd, Odd -> Odd
  | Even, Odd
  | Odd, Even -> Top

let meet x y = match x, y with
  | Top, _ -> y
  | _, Top -> x
  | Bot, _
  | _, Bot -> Bot
  | Even, Even -> Even
  | Odd, Odd -> Odd
  | Even, Odd
  | Odd, Even -> Bot

let widening = join

let sem_itv (n1, _) (n2, _) =
  if Q.gt n1 n2 then
    Bot
  else if Q.equal n1 n2 then
    (* n1 is necessary an integer, ie. its denominator = 1. We check that the numerator is pair *)
    let num = n1.Q.num in
    let _ (*div*), rem = Z.div_rem num (Z.of_int 2)  in
    if Z.equal rem (Z.of_int 0) then Even
    else Odd
  else
    Top

let sem_plus x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Even, Even -> Even
  | Even, Odd -> Odd
  | Odd, Even -> Odd
  | Odd, Odd -> Even
  | _ -> Top

let sem_minus = sem_plus

let sem_times x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Even, _
  | _, Even -> Even
  | Odd, Odd -> Odd
  | _ -> Top

let sem_div x y = match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | _ -> Top

let sem_geq0 = function
  | t -> t
	   
let sem_call _ _ = top
		     
let backsem_plus x y r = match x, y, r with
  | Bot, _, _ | _, Bot, _ | _, _, Bot -> Bot, Bot
  | _, _, Top
  | Top, Top, _ -> x, y
  | Even, Top, Even
  | Top, Even, Even
  | Even, Even, Even -> Even, Even
  | Odd, Top, Even
  | Top, Odd, Even
  | Odd, Odd, Even -> Odd, Odd
  | Even, Odd, Even
  | Odd, Even, Even -> Bot, Bot
  | Even, Top, Odd -> Even, Odd
  | Top, Even, Odd -> Odd, Even
  | Even, Even, Odd -> Bot, Bot
  | Odd, Top, Odd -> Odd, Even
  | Top, Odd, Odd -> Even, Odd
  | Odd, Odd, Odd -> Bot, Bot
  | Even, Odd, Odd -> Even, Odd
  | Odd, Even, Odd -> Odd, Even

let backsem_minus = backsem_plus

let backsem_times x y r = match x, y, r with
  | Bot, _, _ | _, Bot, _ | _, _, Bot -> Bot, Bot
  | _, _, Top
  | Top, Top, _ -> x, y
  | Even, Top, Even -> Even, Top
  | Top, Even, Even -> Top, Even
  | Even, Even, Even -> Even, Even
  | Odd, Top, Even -> Odd, Even
  | Top, Odd, Even -> Even, Odd
  | Odd, Odd, Even -> Bot, Bot
  | Even, Odd, Even -> Even, Odd
  | Odd, Even, Even -> Odd, Even
  | Even, Top, Odd -> Even, Odd
  | Top, Even, Odd -> Odd, Even
  | Even, Even, Odd -> Bot, Bot
  | Odd, Top, Odd -> Odd, Odd
  | Top, Odd, Odd -> Odd, Odd
  | Odd, Odd, Odd -> Odd, Odd
  | Even, Odd, Odd -> Bot, Bot
  | Odd, Even, Odd -> Bot, Bot

let backsem_div x y r = match x, y, r with
  | Bot, _, _ | _, Bot, _ | _, _, Bot -> Bot, Bot
  | _ -> x, y

let to_bounds _ = Bounds.mk (Scalar.of_q Q.minus_inf) (Scalar.of_q Q.inf)
let to_properties _x = Value_properties.empty (* TODO *)
let split _ = None
                                    
                          
