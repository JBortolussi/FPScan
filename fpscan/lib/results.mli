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

(** Maps from locations, node name and unroll id. *)
module MapInd : Map.S with type key = (Location.t*int)
module MapIndNode : Map.S with type key = (Location.t*int*Name.t)
module MapNode : Map.S with type key = (Location.t*Name.t)
module Map : Map.S with type key = Location.t      (** Maps from locations. *)
(* module MapUnroll : Map.S with type key = Location.t*int  (\** Maps from locations and unroll index. *\) *)

(** Module type for Tiny analyze results. *)
module type Register =
  sig
    (* Relational domain of Tiny analyze results *)
    module Dom: Relational.Domain

    (** Type of results *)
    type t

    (** Initial empty register *)
    val empty : t

    (** Register a new value to a location *)
    val register : ?unroll:int -> Location.t -> Dom.t -> t -> t

    (** Merge results from a sub-node analyze with results of the super-node *)
    val register_subnode : t -> t -> Name.t -> t

    (** Return the Ast.stm registered *)
    val find_aopt : Name.t option -> Location.t ->  t -> Dom.t option

    (** Return a sorted list with all values of a location *)
    val get_unrolled_info: Name.t option -> t -> Location.t -> (int * Dom.t) list
  end

(** Results relational domain *)
module ResultRegister (Dom : Relational.Domain) : Register