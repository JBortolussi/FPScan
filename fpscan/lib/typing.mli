type typing_env
(* Empty environment *)
val empty_env: typing_env
(* Return a new typing environment with a new variable declaration
   Parameters:
   * Variable type
   * Previous typing environment
   * Name of the variable and location as a couple
*)
val decl_var_type: Ast.base_type -> typing_env -> (Name.t * Location.t) -> typing_env 
(* Return a set of tuple (name, variable_type) based on the given environment *)
val vars_of_env: typing_env -> Ast.Var.Set.t
(* Type an Ast expression 
  Parameters : 
    * typing environment
    * desired type 
    * untyped ast expression
  Returns : a typed ast expression
  Note : if the provided type is not unifiable with the expression, this 
  method will fail and exit the program...
*)
val type_expr: ?no_rec:bool -> typing_env -> Ast.base_type -> Ast.uexpr -> Ast.expr
(* Type an Ast Statement 
   Parameters :
    * optional argument to enforce 3 address code
    * typing environment
    * untyped ast statement
  Returns :  a typed ast statement
*)
val type_stm:  typing_env -> Ast.ustm -> Ast.stm

val are_types_eq: Ast.base_type -> Ast.base_type  -> bool

val get_vars: Ast.stm -> Ast.Var.Set.t