{ open Parser_fpcore
  exception Error

  let q_of_string c e =
  let c = Q.of_string c in
  if e = 0 then
    c
  else
    let exp = Q.of_string ("1" ^ String.make (abs e) '0') in
    if e > 0 then
      Q.div c exp
    else
      Q.mul c exp
}

rule token = parse
| '\n' { Lexing.new_line lexbuf;  (* cette instruction permet d'avoir de
                                     meilleures indications des numéros de
                                     ligne/colonne en cas d'erreur d'analyse
                                     lorsqu'il y a un retour à la ligne *)
         token lexbuf }
| [' ' '\t']                                          {token lexbuf}
| "FPCore"                                            {FPCORE}
| eof                                                 {EOF}
| "while"                                             {WHILE}
| "while*"                                            {failwith "unsuported keyword: while*"}
| "cast"                                              {CAST}
| "if"                                                {IF}
| '('                                                 {LPAREN}
| ')'                                                 {RPAREN}
| "and"                                               {AND}
| '+'                                                 {PLUS}
| '-'                                                 {MINUS}
| '*'                                                 {MULT}
| '/'                                                 {DIV}
| "fma"                                               {FMA}

| ":pre"            {PROPERTY_KEY_EXPR ":pre"}
| ":spec"           {PROPERTY_KEY_EXPR ":spec"}
| ":rosa-ensuring"  {PROPERTY_KEY_EXPR ":rosa-ensuring"}
| ":example"        {PROPERTY_KEY_EXE}
| ":herbie-time" as s {HERBIE_TIME_KEY s}
| ":herbie-error-input" as s {HERBIE_ERROR_INPUT_KEY s}
| ":herbie-error-output" as s {HERBIE_ERROR_OUTPUT_KEY s}

| "tan"     {TAN}
| "atan"    {ATAN}
| "atan2"   {ATAN2}
| "sin"     {SIN}
| "cos"     {COS}
| "hypot"   {HYPOT}
| "sqrt"    {SQRT}
| "fabs"    {FABS}
| "exp"     {EXP}
| "log"     {LOG}
| "pow"     {POW}

| "PI"                                                {PI}

| "!="                                                {NEQ}  
| '<'                                                 {LT}
| '>'                                                 {GT}
| "<="                                                {LEQ}
| ">="                                                {GEQ} 
| "let"                                               {LET}
| "let*"                                              {LET_STAR}
| "!"                                                 {EXCLAMATION}
| ":"['0'-'9''A'-'Z''a'-'z''-''_']+ as s              {PROPERTY_KEY s}
| "-"?['0'-'9']+'.'['0'-'9']* as f                    {FLOAT (f)}
| "-"?['0'-'9']+'e''-'?['0'-'9']+ as f                {
  let e, parts = String.fold_left (
    fun acc c -> (
      let buffer, acc = acc in
      if (Char.compare c 'e' == 0)
      then (
        ("", buffer :: acc)
      ) else (
        (buffer ^ (String.make 1 c), acc)
      )
    )
  ) ("", []) f in
  let a = List.hd parts in
  let a = a |> int_of_string |> Q.of_int in
  let e = e |> int_of_string in
  let rec _pow (acc: Q.t) (x: Q.t) (n: int) = if n <= 0 then acc else _pow (Q.mul x acc) x (n-1) in
  let e, q = (
    let q = Q.of_int 10 in
    if e < 0
    then -e, (Q.div Q.one q)
    else e, a
  ) in
  let pow_e = _pow Q.one q e in
  QFLOAT (Q.mul a pow_e)
}
| "-"?['0'-'9']+'/'['0'-'9']* as f                    {FRAC (f)}
| "-"?['0'-'9']+ as i                                 {INT (int_of_string i)}
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_''-''*']* as v {VAR v}
(* | '"'['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9'' ''_''-']*'"' as s   {STRING s} *)
| '"'_*'"' as s                                       {STRING s}
| _ as e{e |> Char.escaped |> print_endline; raise Error }
(* | ":"                               {COLON}
| '('                               {LPAREN}
| ')'                               {RPAREN}
| ['0'-'9']+ as i                   {INT (int_of_string i)}
| ['a'-'z']['a'-'z''0'-'9']* as s   {STRING (s)}
| '+'                               {PLUS}
| '-'                               {MINUS}
| '<'                               {LT}
| "<="                              {LEQ} *)
(* | eof {EOF}
| _ { raise Error } *)
(* | [^'.']* as a { "err: " ^ a |> print_endline; raise Error } *)




(* { open Parser
  exception Error
}

rule token = parse
| '\n' { Lexing.new_line lexbuf;  (* cette instruction permet d'avoir de
                                     meilleures indications des numéros de
                                     ligne/colonne en cas d'erreur d'analyse
                                     lorsqu'il y a un retour à la ligne *)
         token lexbuf }
| [' ' '\t']      {token lexbuf}
| "FPCore"        {FPCORE}
| eof { EOF }
| _ { raise Error } *)
