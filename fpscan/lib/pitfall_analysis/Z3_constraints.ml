open Z3
open Z3.Arithmetic

(* open Z3.FuncDecl *)
open Z3.Symbol
(* open Z3.Boolean *)

(* module StringMap = Map.Make(String) *)

module Z3Constraint = struct
  (* Declare the proof context *)
  let cfg = [ ("model", "true"); ("proof", "false") ]
  let ctx = mk_context cfg

  (* Declare "float" type *)
  let int_t = Integer.mk_sort ctx
  let bool_t = Boolean.mk_sort ctx

  let val_t =
    Datatype.mk_sort_s ctx "Val"
      [
        Datatype.mk_constructor_s ctx "val" (mk_string ctx "symbol")
          [
            mk_string ctx "ufp";
            mk_string ctx "nsb";
            mk_string ctx "p";
            mk_string ctx "zero_in_bound";
            mk_string ctx "zero_reachable";
          ]
          [ Some int_t; Some int_t; Some int_t; Some bool_t; Some bool_t ]
          [ 0; 0; 0; 0; 0 ];
      ]

  let acc_lst = List.hd (Datatype.get_accessors val_t)
  let ufp = List.nth acc_lst 0
  let nsb = List.nth acc_lst 1
  let p = List.nth acc_lst 2

  (* True if zero is in the computed bound *)
  let zero_in_bound = List.nth acc_lst 3

  (* True if zero is reachable *)
  let zero_reachable = List.nth acc_lst 4

  (* Create a new var *)
  let mk_var ctx s_name = Expr.mk_const ctx (mk_string ctx s_name) val_t

  (* define max *)
  let mk_max ctx e1 e2 = Boolean.mk_ite ctx (mk_le ctx e1 e2) e2 e1

  (* define min *)
  let mk_min ctx e1 e2 = Boolean.mk_ite ctx (mk_le ctx e1 e2) e1 e2
end
