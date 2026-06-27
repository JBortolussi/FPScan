%{
    open Fpcore_ast
%}

%token EOF

%token FPCORE

%token <string> VAR

%token <int> INT
%token <string> FLOAT
%token <Q.t> QFLOAT
%token <string> STRING
// %token <int * int> DEC
%token <string> FRAC

%token PLUS MINUS MULT LT LEQ GT GEQ DIV NEQ FMA
%token AND

%token <string> PROPERTY_KEY PROPERTY_KEY_EXPR
%token <string> HERBIE_TIME_KEY HERBIE_ERROR_INPUT_KEY HERBIE_ERROR_OUTPUT_KEY
%token PROPERTY_KEY_EXE
%token LPAREN
%token RPAREN

// %token WHILE
%token IF

%token EXCLAMATION

%token TAN ATAN ATAN2 SIN COS HYPOT SQRT FABS EXP LOG POW CAST

%token PI

%token LET LET_STAR WHILE

// %left AND
// %left ATAN
// %left DIV
// %left MULT
// %left PLUS MINUS LT LEQ
// %left NEQ

%start <fpcore> prog
%%

prog:
    | LPAREN FPCORE LPAREN arg=list(argument) RPAREN properties=list(property) e=expr RPAREN EOF {FPCore (arg, properties, e)}
    ;

argument:
    | VAR {ASymbol ($1)}

%inline number:
    | INT   {NDec (Q.of_int $1)}
    | QFLOAT {NDec ($1)}
    | FLOAT {NDec (Q.of_string $1)}
    | FRAC  {NDec (Q.of_string $1)}
    | PI    {NDec (Q.of_float Float.pi)}


%inline operator:
    | PLUS  {Plus}
    | MINUS {Minus}
    | MULT  {Mult}
    | DIV   {Div}
    | GT    {Gt}
    | GEQ   {Geq}
    | AND   {And}
    | NEQ   {Neq}

%inline math_op:
    | ATAN  {"atan"}
    | ATAN2  {failwith "unsuported operator: atan2"}
    | SIN   {"sin"}
    | HYPOT   {failwith "unsuported operator: hypot"}
    | SQRT   {"sqrt"}
    | TAN   {failwith "unsuported operator: tan"}
    | COS   {"cos"}
    | FABS  {failwith "unsuported operator: fabs"}
    | EXP   {failwith "unsuported operator: exp"}
    | LOG   {failwith "unsuported operator: log"}
    // |    {failwith "unsuported operator: "}

%inline rev_operator:
    | LT    {Gt}
    | LEQ   {Geq}

expr_let_binding:
    | LPAREN VAR expr RPAREN  {($2, $3)}

%inline expr_while_binding:
    | LPAREN VAR expr expr RPAREN   {($2, $3, $4)}

expr:
    | LPAREN expr RPAREN                                                    {$2}
    | VAR                                                                   {ESymbol ($1)}
    | number                                                                {ENumber ($1)}
    | LPAREN IF expr expr expr RPAREN                                       {EIte ($3, $4, $5)}
    | LPAREN op=operator arg=list(expr) RPAREN                              {let arg = if (List.length arg) == 1 then ENumber (NDec (Q.zero)) :: arg else arg in EOp (op, arg)}
    | LPAREN op=math_op arg=list(expr) RPAREN                               {ECall (op, arg)} 
    | LPAREN rev_op=rev_operator arg=list(expr) RPAREN                      {EOp (rev_op, List.rev arg)}
    | LPAREN LET LPAREN bind=list(expr_let_binding) RPAREN expr RPAREN      {ELet (bind, $6)}
    | LPAREN LET_STAR LPAREN bind=list(expr_let_binding) RPAREN expr RPAREN {ELetStar (bind, $6)}
    | LPAREN EXCLAMATION PROPERTY_KEY VAR e=expr RPAREN                     {e}
    | LPAREN CAST e=expr RPAREN                                             {e}
    | LPAREN FMA x=expr y=expr z=expr RPAREN                                {EOp(Plus, [EOp (Mult, [x; y]); z])}
    | LPAREN POW x=expr e=number RPAREN                                     {
        let NDec (e) = e in
        let e = Q.to_int e in
        let rec pow (e:Fpcore_ast.expr) (n: int) (acc: Fpcore_ast.expr) = if n == 1 then acc else pow e (n-1) (EOp (Mult, [acc; e])) in
        pow x e x
    }
    | LPAREN WHILE cond=expr LPAREN bind=list(expr_while_binding) RPAREN ret=expr RPAREN    {EWhile (cond, bind, ret)}

data:
    // | LPAREN data RPAREN        {$2}
    // | STRING                    {DString ($1)}
    | STRING         list(data)       {DString ($1)}
    | VAR  list(data)                 {DString ($1)}
    // | nonempty_list(VAR)   {DString(List.hd $1)}
    // | nonempty_list(STRING)   {DString(List.hd $1)}
    
    // | expr                      {DExpr ($1)}
    // | LPAREN list(data) RPAREN  {List.hd $2}

example_asn:
    | LPAREN VAR expr RPAREN {()}

%inline pair:
    | LPAREN expr expr RPAREN   {""}

%inline pairpair:
    | LPAREN pair pair RPAREN   {""}

property:
    | PROPERTY_KEY data    {Prop ($1, $2)}
    | PROPERTY_KEY LPAREN data RPAREN    {Prop ($1, $3)}
    | PROPERTY_KEY_EXPR expr {Prop ($1, DExpr($2))}
    | PROPERTY_KEY_EXE LPAREN list(example_asn) RPAREN {Prop (":example", DString("example are not supported"))}
    | HERBIE_TIME_KEY expr {Prop ($1, DString("not supported"))}
    | HERBIE_ERROR_INPUT_KEY pairpair {Prop ($1, DString("not supported"))} 
    | HERBIE_ERROR_OUTPUT_KEY pairpair {Prop ($1, DString("not supported"))} 
    ;