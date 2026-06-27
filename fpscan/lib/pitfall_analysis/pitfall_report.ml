open Subset_ast_stm

type pitfall =
  | None of Location.t
  | Absorption of Location.t * string * string
  | Cancellation of Location.t

type pitfall_report = {
  location : Location.t;
  stm : stm;
  mutable pitfall_list : pitfall list;
}

let string_of_pitfall = function
  | None _ -> ""
  | Absorption (_, x, y) ->
      Printf.sprintf "\tABSORPTION: %s absorbed by %s\n" x y
  | Cancellation _ -> Printf.sprintf "\tCANCELLATION\n"

let string_of_pitfall_report { location; stm; pitfall_list } =
  Printf.sprintf "%s => %s:\n%s"
    (Location.to_string location)
    (string_of_stm stm)
    (if pitfall_list == [] then ""
     else
       List.fold_left
         (fun acc pitfall -> acc ^ string_of_pitfall pitfall)
         "" pitfall_list)

(* Functions to convert a retport into yaml *)
let yaml_of_pitfall (pitfall : pitfall) : Yaml.value =
  match pitfall with
  | None _ -> `String "None"
  | Absorption (_, x, y) -> `String (Printf.sprintf "absorption %s %s" x y)
  | Cancellation _ -> `String (Printf.sprintf "cancellation")

let yaml_of_pitfall_report { location; stm; pitfall_list } : Yaml.value =
  let pitfalls =
    `A (List.fold_left (fun acc p -> yaml_of_pitfall p :: acc) [] pitfall_list)
  in
  `A
    [
      `String (Location.to_string location);
      `String (string_of_stm stm);
      pitfalls;
    ]

let yaml_of_pitfall_report_list (res : pitfall_report list)
    (sanity_check : bool) : Yaml.value =
  let pitfall_list =
    List.fold_left (fun acc p -> yaml_of_pitfall_report p :: acc) [] res
  in
  `O [ ("sanity_check", `Bool sanity_check); ("report", `A pitfall_list) ]
