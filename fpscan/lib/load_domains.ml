let domains : string list ref = ref [] (*["intervals_infint"]; "intervals_double"]*)

let decl_domain d = domains := d::!domains

let params = ref []

let get_domain s =
  if s = "" then
    match Domains.available_domains with
    | [] -> Report.error "compiled without any abstract domain!"
    | d :: _ -> d
  else
    try List.find (fun d -> Domains.get_name d = s) Domains.available_domains
    with Not_found ->
      Report.error "'%s' no such abstract domain.\n%!" s
      

let prepare_domain dom =
    try
      let module Dom = (val dom : Relational.Domain) in
      List.iter Dom.parse_param (List.rev !params) ;
      dom
    with Not_found ->
      Report.error "'%s' no such abstract domain.\n%!" (Domains.get_name dom)

let prepare_domains domlist =
  let build dom =
    let module D = (val dom: Relational.Domain) in
    if D.is_partitioned () then
      dom
    else
      let module D = BoolPartition.MakeR (D) in
      (* let module D = BoolActivate.MakeR (D) in *)
      (module D : Relational.Domain)
  in
  match domlist with
  | [] -> assert false
  | [dom] -> build (prepare_domain dom)
  | hd::tl ->
       List.fold_left (
 	   fun accu domain ->
	   let dom = prepare_domain domain in
	   let module Dom = (val dom: Relational.Domain) in
	   let module Accu = (val accu: Relational.Domain) in
	   let module D = Relational.Prod (Accu) (Dom) in
           build (module D)
 	 ) (prepare_domain hd) tl
	 
let set_param p = params := p :: !params
  
let help_domain s =
  let module Dom = (val (get_domain s)) in
  Dom.fprint_help Format.std_formatter;
  exit 0
