module type FloatFormat = sig
  val precision : int
  val e_min : int
  val e_max : int
  val e_bit_number : int
end

module Float32Format : FloatFormat = struct
  let precision = 24
  let e_min = -126
  let e_max = 128
  let e_bit_number = 8
end

module Float16Format : FloatFormat = struct
  let precision = 11
  let e_min = -14
  let e_max = 16
  let e_bit_number = 5
end

let to_float_format (n_bit : int) =
  match n_bit with
  | 32 -> (module Float32Format : FloatFormat)
  | 16 -> (module Float16Format : FloatFormat)
  | _ -> failwith (Printf.sprintf "Unsuported format %d" n_bit)
