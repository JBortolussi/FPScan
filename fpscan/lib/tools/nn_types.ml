type act_t = Relu | Sat of (Q.t * string) * (Q.t * string) | TanH
type layer_t = (Q.t * string) list list * act_t
type nn_t = layer_t list

let pp_act fmt act =
  match act with
  | Relu -> Format.fprintf fmt "relu"
  | TanH -> Format.fprintf fmt "tanh"
  | Sat ((_,a),(_,b)) -> Format.fprintf fmt "sat(%s, %s)" a b
