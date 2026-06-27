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


module MapInd = Map.Make (struct type t = Location.t * int let compare = compare end)
module MapIndNode = Map.Make (struct type t = (Location.t * int * Name.t) let compare = compare end)
module MapNode = Map.Make (struct type t = Location.t * Name.t let compare = compare end)
module Map = Map.Make (struct type t = Location.t let compare = compare end)


(** Module type for Tiny analyze results. *)
module type Register =
  sig
    (* Relational domain of Tiny analyze results *)
    module Dom: Relational.Domain

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

module ResultRegister (Dom : Relational.Domain) =
struct
  module Dom = Dom

  type t = Dom.t Map.t * Dom.t MapInd.t * Dom.t MapNode.t * Dom.t MapIndNode.t

  let empty = Map.empty, MapInd.empty, MapNode.empty, MapIndNode.empty

  let register ?(unroll=(-1)) l t (m,mi,mn,mni) =
    if unroll < 0 then
      Map.add l t m, mi, mn, mni
    else ((* Cleaning already computed values to make sure we keep only the freshest *)
      (* let m = Location.Map.remove l m in	 *)
      m, MapInd.add (l,unroll) t mi, mn, mni
    )

  let register_subnode (m1,mi1,mn1,mni1) (m2,mi2,mn2,mni2) node_name =
    let addAll m1 m2 =
      Map.fold (fun k t m -> MapNode.add (k, node_name) t m) m1 m2 in
    let addAll_ind m1 m2 =
      MapInd.fold (fun (l,i) t m -> MapIndNode.add (l,i, node_name) t m) m1 m2 in
    (* let merge_node _ a b = match a with None -> b | _ -> a in *)
    let merge _ a b = match a with 
    | None -> b
    | Some a' -> match b with 
      | None -> a
      | Some b' -> Some (Dom.join a' b') in

    let mn' = addAll m2 mn1 in
    let mni' = addAll_ind mi2 mni1 in
    let mn, mni = MapNode.merge merge mn' mn2, MapIndNode.merge merge mni' mni2 in
    m1, mi1, mn ,mni

  let find_aopt node_name l (m,_,mn,_) =
    match node_name with
    | None -> Map.find_opt l m
    | Some n -> MapNode.find_opt (l,n) mn

  let get_unrolled_info node_name (_,mi,_,mni) l =
    match node_name with
    | None ->
      let ind = MapInd.filter (fun (l',_) _ (*a*) -> l = l') mi in
      let ind = MapInd.bindings ind in
      let ind = List.sort (fun (i1, _) (i2, _) -> compare i1 i2) ind in
      List.map (fun ((_, i), x) -> i, x) ind

    | Some n ->
      let ind = MapIndNode.filter (fun (l',_,n') _ (*a*) -> l = l' && n = n') mni in
      let ind = MapIndNode.bindings ind in
      let ind = List.sort (fun (i1, _) (i2, _) -> compare i1 i2) ind in
      List.map (fun ((_,i,_), x) -> i, x) ind

end