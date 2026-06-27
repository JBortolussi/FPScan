(* Takes as an input a list of CVS files. One for each layer, and build the tiny code for that NN *)
open Tiny
open Nn_types
   
let print_tiny input_vars new_vars output_vars init_ranges nn_prog =
  (* Declaring vars *)
  Format.printf "%a@." Ast.Var.Set.pp (Ast.Var.Set.of_list (input_vars@new_vars@output_vars));
  (* Initializing inputs *)
  let input_defs =
    List.map2 (fun (id, _) (min_, max_) ->
        let l = Location.dummy () in
        let e = Ast.mk_expr l Ast.RealT (Rand(min_, max_)) in
        Ast.Asn(l, id, e)
        
      ) input_vars init_ranges
  in
  List.iter (Format.printf "%a@." Ast.fprint_stm) input_defs;
  (* Main *)
  Format.printf "%a@." Ast.fprint_stm nn_prog


let test () =
  let build_l nb f =
    List.init nb (fun i -> let i = f i in Q.of_int i, string_of_int i ^ ".0")
  in
  (* 5 inputs *)
  let input_vars = List.init 5 (fun i -> "i" ^ string_of_int i, Ast.RealT) in
  let l1_1 = build_l 5 (fun x -> x) in
  let l1_2 = build_l 5 (fun x -> x + 5) in
  let l1_3 = build_l 5 (fun x -> x + 10) in
  let l1 = [l1_1; l1_2; l1_3], TanH in
  (* 3 intermediate *)
  let l2_1 = build_l 3 (fun x -> x+ 15) in
  let l2_2 = build_l 3 (fun x -> x + 18) in
  let l2_3 = build_l 3 (fun x -> x + 21) in
  let l2 = [l2_1; l2_2; l2_3], Relu in
  
  (* 3 outputs *)
  let output_vars = List.init 3 (fun i -> "o" ^ string_of_int i, Ast.RealT) in
  let new_vars, nn_prog = Nn.build_nn input_vars output_vars "neuron" [l1; l2] in

  let init_ranges =
    List.init (List.length input_vars) (fun _ -> (Q.of_int (-1), "-1."), (Q.of_int (1), "1."))
  in
  print_tiny input_vars new_vars output_vars init_ranges nn_prog

let layers_files: string list ref = ref []
let act_funs: act_t list ref = ref []

                       
let register_layer l =
  layers_files := !layers_files @ [l]

let register_actfun a =
  let a =
    match a with
    | "tanh" -> TanH
    | "relu" -> Relu
    | "sat" -> Sat ((Q.of_int (-1), "-1"), (Q.of_int 1, "1"))
  in
  act_funs := !act_funs @ [a]
  
let _ =
  let usage_msg =
    Printf.sprintf
      "Usage: %s -l layer1.csv -a tanh -l layer2.csv -a relu ...." Sys.argv.(0) in
  let speclist = Arg.align [
    ("-l", Arg.String register_layer,
     "<file.csv>  Register file.cvs as the i-th layer");
    ("-a", Arg.String register_actfun,
     "<act_fun>  Use this activation function for the i-th layer (choices are relu, tanh or sat)");
                   ]
  in
  Arg.parse speclist (fun _ -> ()) usage_msg;
  if List.length !layers_files <> List.length !act_funs then (
    Format.eprintf "Error: Provide one activation function for each layer!@.";
    exit 1
  );
  (* Loading CSV files *)
  Format.eprintf "@[<v 2>Loading layers:@ ";
  let layers =
    List.map2 (fun s act ->
        Format.eprintf "%s " s;
        let s_csv = Csv.load s in
        Format.eprintf "(%i neurons with %i inputs) -- %a@ "
          (Csv.lines s_csv)
          (Csv.columns s_csv)
          pp_act act
        ;
        s, act) !layers_files !act_funs in
  Format.eprintf "@]@.";

  let input_vars, output_vars, new_vars, nn_prog =
    Nn.build_nn_from_csv layers
  in
  let init_ranges =
    List.init (List.length input_vars) (fun _ -> (Q.of_int (-1), "-1."), (Q.of_int (1), "1."))
  in
  print_tiny input_vars new_vars output_vars init_ranges nn_prog
