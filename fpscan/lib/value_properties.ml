type val_t = Bool of bool | Int of int | String of string
type key = string list
module M = Map.Make(struct type t = key let compare = Stdlib.compare end)
type t = val_t M.t

(* Compute the intersection of the two list. Same labels should be
   merge according to their own rules. If a label appears only in one
   list, it is kept as is.

   We assume each label appears only once in each list of properties

   This has to be adapted depending on the value stored in these
   properties.
*)

let empty = M.empty

let add = M.add
            
let merge l1 l2 =
  M.merge (fun _key e1_opt e2_opt ->
      match e1_opt, e2_opt with
      | None, Some e | Some e, None -> Some e
      | None, None -> None
      | Some e1, Some e2 -> if e1 = e2 then Some e1 else None
      (* TODO we could have a more fine grain way of merging,
         depending on the key and the values *)
    ) l1 l2

let fprint_val_t (ff: Format.formatter) (v: val_t) =
   match v with
   | Bool (b)     -> Format.pp_print_bool ff b
   | Int (i)      -> Format.pp_print_int ff i
   | String (s)   -> Format.pp_print_string ff s

let fprint_key (ff: Format.formatter) (k: key) =
   let pp = Utils.fprintf_list ~sep:"," Format.pp_print_string in
   Format.fprintf ff "[%a]" pp k
  
let fprint (ff: Format.formatter) (t: t) =
   let fprint_bindings (ff: Format.formatter) ((k, v): key * val_t) =
      Format.fprintf ff "@[%a -> %a@]" fprint_key k fprint_val_t v
   in
   let pp = Utils.fprintf_list ~sep:";" fprint_bindings in
   pp ff (M.bindings t) 
