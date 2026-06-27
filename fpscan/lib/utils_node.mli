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

(** Various utility functions for node treatment. *)

(** Get environment associated to a node according to the variables present
    in the arguments of the call *)
val get_env_node_in : Name.t -> 
    (Name.t * (Ast.Var.Set.t * Ast.Var.t list * Ast.Var.t list * (Name.t * (Ast.expr * Ast.Var.t) list) list)) list ->
    Name.t -> Ast.expr list -> Location.t ->
    Ast.Var.Set.t * Ast.Var.t list * (Name.t * (Ast.expr * Ast.Var.t) list) list

(** Build the input relation from the inputs, the variables present in the arguments 
   of the call and the shared variables *)
val get_rel_in : Ast.expr list -> Ast.Var.t list -> (Name.t * (Ast.expr * Ast.Var.t) list) list ->
    Name.t -> Name.t -> Location.t -> (Ast.expr * Ast.Var.t) list

(** Return the environment and the entry relation for the node name *)
val get_env_rel_in : Name.t -> 
    (Name.t * (Ast.Var.Set.t * Ast.Var.t list * Ast.Var.t list * (Name.t * (Ast.expr * Ast.Var.t) list) list)) list ->
    Name.t -> Ast.expr list -> Location.t -> (Ast.Var.Set.t * (Ast.expr * Ast.Var.t) list)

(** Build the output relation from the inputs, the outputs and the shared variables *)
val get_rel_out : Name.t -> 
    (Name.t * (Ast.Var.Set.t * Ast.Var.t list * Ast.Var.t list * (Name.t * (Ast.expr * Ast.Var.t) list) list)) list ->
    Ast.expr list -> Name.t -> Name.t list -> Location.t -> (Ast.expr * Ast.Var.t) list