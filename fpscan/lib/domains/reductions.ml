module Sign_Parity =
  struct
    module CartProd = NonRelationalProduct.Make (Sign.Int) (Parity)

    module Reduction =
      struct
	let name = "1" (* No specific name here *)
	let fprint_help _ = ()
	let parse_param _ = ()
	type t = CartProd.t
	let rho (s, p) = match s, p with
	  | Sign.SBot , _
	  | _, Parity.Bot
	  | Sign.S0, Parity.Odd -> Sign.Int.bottom, Parity.bottom
	  | Sign.S0, _ -> s, Parity.Even
	  | _ -> s, p
      end
						
  include NonRelationalReduction.Make (CartProd) (Reduction)

  end
