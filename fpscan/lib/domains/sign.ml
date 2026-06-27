(******************************************************************************)
(*                                                                            *)
(* Domaine des signes vu en cours :                                           *)
(*                                                                            *)
(*      STop                                                                  *)
(*      /  \                                                                  *)
(*     /    \                                                                 *)
(*    /      \                                                                *)
(* SLe0      SGe0                                                             *)
(*    \      /                                                                *)
(*     \    /                                                                 *)
(*      \  /                                                                  *)
(*       S0                                                                   *)
(*        |                                                                   *)
(*        |                                                                   *)
(*      SBot                                                                  *)
(*                                                                            *)
(* avec pour fonction de concrétisation gamma :                               *)
(* STop |-> Z                                                                 *)
(* SLe0 |-> { n \in Z | n <= 0 }                                              *)
(* SGe0 |-> { n \in Z | n >= 0 }                                              *)
(* S0   |-> { 0 }                                                             *)
(* SBot |-> \emptyset                                                         *)
(*                                                                            *)
(******************************************************************************)
type sign = SBot | S0 | SLe0 | SGe0 | STop

module Make (I: sig val name_suffix: string val base_type : Ast.base_type end) =
  struct
    let name = "signes" ^ I.name_suffix

    let base_type = I.base_type

    (* no option *)
    let parse_param _ = ()

    let fprint_help fmt = Format.fprintf fmt "Kildall abstraction"

    let log = false
          
					 
    type t = sign
    let fprint ff t = Format.fprintf ff "%s"
				     (match t with
				      | SBot -> "⊥"
				      | S0 -> "0"
				      | SLe0 -> "<=0"
				      | SGe0 -> ">=0"
				      | STop -> "⊤")

    let json = function
      | SBot -> `String "⊥"
      | S0 -> `String "0"
      | SLe0 -> `String "<=0"
      | SGe0 -> `String ">=0"
      | STop -> `String "⊤"

    let order x y = match x, y with
      | S0, S0 | S0, SGe0 | S0, SLe0 | S0, STop | SBot, S0
      | SBot, SBot | SBot, SGe0 | SBot, SLe0 | SBot, STop
      | SGe0, SGe0 | SGe0, STop | SLe0, SLe0 | SLe0, STop
      | STop, STop -> true
      | _ -> false

    let top = STop
    let bottom = SBot
    let is_bottom x = x = SBot

    (* borne supérieure : plus petit des majorants de {x, y} *)
    let join x y = match x, y with
      | SBot, SBot -> SBot
      | S0, SGe0 | SGe0, S0 | SGe0, SGe0 | SGe0, SBot | SBot, SGe0 -> SGe0
      | S0, SLe0 | SLe0, S0 | SLe0, SLe0 | SLe0, SBot | SBot, SLe0 -> SLe0
      | S0, STop | STop, S0 | STop, STop | STop, SLe0 | STop, SGe0
      | STop, SBot | SLe0, STop | SLe0, SGe0 | SGe0, STop
      | SGe0, SLe0 | SBot, STop -> STop
      | S0, S0 | S0, SBot | SBot, S0 -> S0

    (* borne supérieure : plus grand des minorants de {x, y} *)
    let meet x y = match x, y with
      | S0, SBot | STop, SBot | SLe0, SBot | SGe0, SBot | SBot, S0
      | SBot, STop | SBot, SLe0 | SBot, SGe0 | SBot, SBot -> SBot
      | STop, SGe0 | SGe0, STop | SGe0, SGe0 -> SGe0
      | S0, S0 | S0, STop | S0, SLe0 | S0, SGe0 | STop, S0
      | SLe0, S0 | SLe0, SGe0 | SGe0, S0 | SGe0, SLe0 -> S0
      | STop, SLe0 | SLe0, STop | SLe0, SLe0 -> SLe0
      | STop, STop -> STop

    (* Le treillis n'a pas de chaine strictement croissante infinie,
     * donc il suffit d'utiliser l'union comme élargissement. *)
    let widening = join

    let sem_itv (n1,_) (n2,_) =
      if Q.gt n1 n2 then SBot
      else if Q.equal n1 n2 && Q.equal n1 Q.zero then S0
      else if Q.geq n1 Q.zero then SGe0
      else if Q.leq n2 Q.zero then SLe0
      else (* n1 < 0 < n2 *)STop

    let sem_plus x y = match x, y with
      | STop, SBot | SLe0, SBot | SGe0, SBot | S0, SBot
      | SBot, STop | SBot, SLe0 | SBot, SGe0 | SBot, S0
      | SBot, SBot -> SBot
      | S0, S0 -> S0
      | SGe0, SGe0 | SGe0, S0 | S0, SGe0 -> SGe0
      | SLe0, SLe0 | SLe0, S0 | S0, SLe0 -> SLe0
      | STop, STop | STop, SLe0 | STop, SGe0 | STop, S0
      | SLe0, STop | SLe0, SGe0 | SGe0, STop | SGe0, SLe0
      | S0, STop -> STop

    let sem_minus x y = match x, y with
      | STop, SBot | SLe0, SBot | SGe0, SBot | S0, SBot
      | SBot, STop | SBot, SLe0 | SBot, SGe0 | SBot, S0
      | SBot, SBot -> SBot
      | S0, S0 -> S0
      | SLe0, SGe0 | SLe0, S0 | S0, SGe0 -> SLe0
      | SGe0, SLe0 | SGe0, S0 | S0, SLe0 -> SGe0
      | STop, STop | STop, SLe0 | STop, SGe0 | STop, S0
      | SLe0, STop | SLe0, SLe0 | SGe0, STop | SGe0, SGe0
      | S0, STop -> STop

    let sem_times x y = match x, y with
      | STop, SBot | SLe0, SBot | SGe0, SBot | S0, SBot
      | SBot, STop | SBot, SLe0 | SBot, SGe0 | SBot, S0
      | SBot, SBot -> SBot
      | STop, S0 | SLe0, S0 | SGe0, S0 | S0, STop | S0, SLe0
      | S0, SGe0 | S0, S0 -> S0
      | SLe0, SLe0 | SGe0, SGe0 -> SGe0
      | SLe0, SGe0 | SGe0, SLe0 -> SLe0
      | STop, STop | STop, SLe0 | STop, SGe0 | SLe0, STop
      | SGe0, STop -> STop

    let sem_div x y = match x, y with
      | STop, SBot | SLe0, SBot | SGe0, SBot | S0, SBot
      | SBot, STop | SBot, SLe0 | SBot, SGe0 | SBot, S0
      | SBot, SBot | STop, S0 | SLe0, S0 | SGe0, S0 | S0, S0 -> SBot
      | S0, SLe0 | S0, SGe0 -> S0
      | SLe0, SLe0 | SGe0, SGe0 -> SGe0
      | SLe0, SGe0 | SGe0, SLe0 -> SLe0
      | S0, STop | STop, STop | STop, SLe0 | STop, SGe0
      | SLe0, STop | SGe0, STop -> STop

    let sem_geq0 = function
      | SBot -> SBot
      | S0 -> S0
      | SLe0 -> S0
      | SGe0 -> SGe0
      | STop -> SGe0

    let sem_call f args =
      if List.exists is_bottom args then bottom else
	match f with
	| "sqrt" -> (
	  match args with
	  | [SLe0] | [S0] -> S0
	  | _ -> SGe0
	)
	| _ -> top
		 
    let backsem_plus x y r = match x, y, r with
      | SBot, _, _ | _, SBot, _ | _, _, SBot -> SBot, SBot
      | _, _, STop | STop, STop, _ -> x, y
      | STop, SLe0, SLe0 -> STop, SLe0
      | STop, SGe0, SLe0 -> SLe0, SGe0
      | STop, S0, SLe0 -> SLe0, S0
      | SLe0, STop, SLe0 -> SLe0, STop
      | SLe0, SGe0, SLe0 -> SLe0, SGe0
      | SLe0, SLe0, SLe0 -> SLe0, SLe0
      | SLe0, S0, SLe0 -> SLe0, S0
      | SGe0, STop, SLe0 -> SGe0, SLe0
      | SGe0, SGe0, SLe0 -> S0, S0
      | SGe0, SLe0, SLe0 -> SGe0, SLe0
      | SGe0, S0, SLe0 -> S0, S0
      | S0, STop, SLe0 -> S0, SLe0
      | S0, SGe0, SLe0 -> S0, S0
      | S0, SLe0, SLe0 -> S0, SLe0
      | S0, S0, SLe0 -> S0, S0
      | STop, SLe0, SGe0 -> SGe0, SLe0
      | STop, SGe0, SGe0 -> STop, SGe0
      | STop, S0, SGe0 -> SGe0, S0
      | SLe0, STop, SGe0 -> SLe0, SGe0
      | SLe0, SGe0, SGe0 -> SLe0, SGe0
      | SLe0, SLe0, SGe0 -> S0, S0
      | SLe0, S0, SGe0 -> S0, S0
      | SGe0, STop, SGe0 -> SGe0, STop
      | SGe0, SGe0, SGe0 -> SGe0, SGe0
      | SGe0, SLe0, SGe0 -> SGe0, SLe0
      | SGe0, S0, SGe0 -> SGe0, S0
      | S0, STop, SGe0 -> S0, SGe0
      | S0, SGe0, SGe0 -> S0, SGe0
      | S0, SLe0, SGe0 -> S0, S0
      | S0, S0, SGe0 -> S0, S0
      | STop, SLe0, S0 -> SGe0, SLe0
      | STop, SGe0, S0 -> SLe0, SGe0
      | STop, S0, S0 -> S0, S0
      | SLe0, STop, S0 -> SLe0, SGe0
      | SLe0, SGe0, S0 -> SLe0, SGe0
      | SLe0, SLe0, S0 -> S0, S0
      | SLe0, S0, S0 -> S0, S0
      | SGe0, STop, S0 -> SGe0, SLe0
      | SGe0, SGe0, S0 -> S0, S0
      | SGe0, SLe0, S0 -> SGe0, SLe0
      | SGe0, S0, S0 -> S0, S0
      | S0, STop, S0 -> S0, S0
      | S0, SGe0, S0 -> S0, S0
      | S0, SLe0, S0 -> S0, S0
      | S0, S0, S0 -> S0, S0

    let backsem_minus x y r =
      let opposite = function
	| SLe0 -> SGe0
	| SGe0 -> SLe0
	| x -> x in
      let x, y = backsem_plus x (opposite y) r in
      x, opposite y

    (* Ces fonctions ne sont pas les plus précises mais sont néanmoins correctes. *)
    let backsem_times x y r = match x, y, r with
      | SBot, _, _ | _, SBot, _ | _, _, SBot -> SBot, SBot
      | _ -> x, y
    let backsem_div x y r = match x, y, r with
      | SBot, _, _ | _, SBot, _ | _, _, SBot -> SBot, SBot
      | _ -> x, y

    let to_bounds _ = Bounds.mk (Scalar.of_q Q.minus_inf) (Scalar.of_q Q.inf)
    let to_properties _x = Value_properties.empty (* TODO *)
    let split _ = None
  end

module Int = Make (struct let name_suffix = "_int" let base_type = Ast.IntT end)
module Real = Make (struct let name_suffix = "_real" let base_type = Ast.RealT end)
