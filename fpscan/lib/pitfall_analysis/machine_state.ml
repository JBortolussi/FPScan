module StringMap = Map.Make (String)

module MachineState = struct
  module StringMap = Map.Make (String)

  type t = Bounds.t StringMap.t

  let from_loc (module Results : Analyze.Results) (l : Location.t) : t option =
    let l = Location.end_p l in
    match Results.Res.find_aopt None l Results.results with
    | None -> Some StringMap.empty
    | Some a -> (
        try
          let bounds = Results.Res.Dom.to_bounds a in
          let m =
            List.fold_left
              (fun map ((v, _), b) -> StringMap.add (Ast.Var.get_name v) b map)
              StringMap.empty bounds
          in
          Some m
        with Relational.BotEnv -> None)

  let find_opt (v : string) (m : t) : Bounds.t option = StringMap.find_opt v m
  let empty = StringMap.empty

  let fprint ff (var_map : t) =
    StringMap.iter
      (fun v b -> Format.fprintf ff "%s : @[%a@]@." v Bounds.pp b)
      var_map

  let from_bound_map (m : Bounds.t StringMap.t) : t = m
end

module MachineStateSet = struct
  module LocationMap = Map.Make (struct
    type t = Location.t

    let compare = compare
  end)

  type t = MachineState.t option LocationMap.t

  let from_map (m : MachineState.t option LocationMap.t) : t = m

  let get_machine_state (stm : Ast.stm) (module Results : Analyze.Results) : t =
    let module Res = Results.Res in
    let module Dom = Res.Dom in
    let rec get_stm_var_map (stm : Ast.stm) acc =
      let from_loc = MachineState.from_loc (module Results : Analyze.Results) in
      match stm with
      | Ast.Asn (l, _, _) -> LocationMap.add l (from_loc l) acc
      | Ast.Seq (_, s1, s2) ->
          let acc = get_stm_var_map s1 acc in
          get_stm_var_map s2 acc
      | Ast.Ite (l, _, s_then, s_else) ->
          let acc = LocationMap.add l (from_loc l) acc in
          let acc = get_stm_var_map s_then acc in
          get_stm_var_map s_else acc
      | Nop (_) -> acc
      | _ -> failwith ("invalid stm: " ^(
        match stm with
        | Asrt (_,  _)      -> "Asrt"
        | While (_, _, _)   -> "While"
        | ReadInput (_, _)  -> "ReadInput"
        | ReadState (_, _)  -> "ReadState"
        | NN (_,_,_,_)      -> "NN"
        | Nde (_, _, _, _)  -> "Nde"
        | _ -> "none"
      ))
    in

    get_stm_var_map stm LocationMap.empty

  let print_loc_var_map ff stm (loc_var_map : t) =
    let print_var_map ff var_map =
      match var_map with
      | Some var_map -> MachineState.fprint ff var_map
      | None -> Format.fprintf ff "None"
    in
    let rec print_stm ff stm =
      match stm with
      | Ast.Asn (l, _, _) ->
          let var_map = LocationMap.find l loc_var_map in
          Format.fprintf ff "@[%a@.%a @.@.%a@]@." Location.fprint l
            Ast.fprint_stm stm print_var_map var_map
      | Ast.Ite (l, _, s_then, s_else) ->
          let var_map = LocationMap.find l loc_var_map in
          Format.fprintf ff "@[%a@.%a @.@.%a@]@." Location.fprint l
            Ast.fprint_stm stm print_var_map var_map;
          print_stm ff s_then;
          print_stm ff s_else
      | Ast.Seq (_, s1, s2) ->
          print_stm ff s1;
          print_stm ff s2
      | _ -> failwith "unsorported statement"
    in
    print_stm ff stm

  let get_location_state (loc : Location.t) (m : t) : MachineState.t option =
    LocationMap.find loc m

  let fold f m acc = LocationMap.fold f m acc
end
