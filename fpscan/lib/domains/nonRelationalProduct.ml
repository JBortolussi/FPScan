module Make (D1 : NonRelational.Domain) (D2 : NonRelational.Domain) = struct

    let name = D1.name ^ "_" ^ D2.name
    let parse_param _ (* UNUSED: s *) = ()
    let fprint_help _ (* UNUSED: fmt *) = ()
    let log = D1.log || D2.log
			       
    type t = D1.t * D2.t
    let base_type =
      assert (D1.base_type = D2.base_type);
      D1.base_type (* D2 shall have the same base_type *)

    let fprint ff (t1, t2) =
      Format.fprintf ff "(%a, %a)" D1.fprint t1 D2.fprint t2

    let json (t1, t2) = `Assoc ([
      "domain_name", `String name; 
      "fst",  (D1.json t1); 
      "snd", (D2.json t2)
    ])

    let order (x1, x2) (y1, y2) = D1.order x1 y1 && D2.order x2 y2

    let top = D1.top, D2.top
    let bottom = D1.bottom, D2.bottom
    let is_bottom (x1, x2) = D1.is_bottom x1 || D2.is_bottom x2
							     
    let lift2 f1 f2 (x1, x2) (y1, y2) = f1 x1 y1, f2 x2 y2

    let join = lift2 D1.join D2.join
    let meet = lift2 D1.meet D2.meet

    let widening = lift2 D1.widening D2.widening

    let sem_itv n1 n2 = D1.sem_itv n1 n2, D2.sem_itv n1 n2

    let sem_plus = lift2 D1.sem_plus D2.sem_plus
    let sem_minus = lift2 D1.sem_minus D2.sem_minus
    let sem_times = lift2 D1.sem_times D2.sem_times
    let sem_div = lift2 D1.sem_div D2.sem_div

    let sem_geq0 (x1, x2) = D1.sem_geq0 x1, D2.sem_geq0 x2

    let sem_call f args = let args1, args2 = List.split args in
			  D1.sem_call f args1, D2.sem_call f args2
      
    let lift3 f1 f2 (x1, x2) (y1, y2) (r1, r2) =
      let x1, y1 = f1 x1 y1 r1 in
      let x2, y2 = f2 x2 y2 r2 in
      (x1, x2), (y1, y2)

    let backsem_plus = lift3 D1.backsem_plus D2.backsem_plus
    let backsem_minus = lift3 D1.backsem_minus D2.backsem_minus
    let backsem_times = lift3 D1.backsem_times D2.backsem_times
    let backsem_div = lift3 D1.backsem_div D2.backsem_div

    let to_bounds (x1, x2) =
      let b1 = D1.to_bounds x1 in
      let b2 = D2.to_bounds x2 in
      Bounds.join b1 b2

    let to_properties (x1, x2) =
      Value_properties.merge
        (D1.to_properties x1)
        (D2.to_properties x2)
      
      
    let split (x1, x2) =
      match D1.split x1, D2.split x2  with
      | None, None -> None
      | Some (x1a,x1b), None -> Some ((x1a,x2), (x1b, x2))
      | None, Some (x2a,x2b)-> Some ((x1,x2a), (x1, x2b))
      | Some (x1a,x1b), Some (x2a,x2b) -> Some ((x1a,x2a), (x1b, x2b))
  end
