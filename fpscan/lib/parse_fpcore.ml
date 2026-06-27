
let prepare_program (tiny_ast: Ast.stm) : Subset_ast_stm.stm * Ast.stm = (
    print_endline "Make program SSA";
    let ast = tiny_ast |> Fpcore_ast.mk_tiny_ssa |> Fpcore_ast.remove_zero  in
    (* make sure tiny_ast and ast match *)
    let tiny_ast = Subset_ast_stm.to_tiny_stm ast in
    ast, tiny_ast
)

let parse_fpcore_prgm (path: string) : Ast.Var.Set.t * Ast.stm = (
    print_endline "\nParse FPCore program";
    let ic = open_in path in
    let prgm = In_channel.input_all ic in
    let lexbuf = Lexing.from_string prgm in
    try
        let prgm = Parser_fpcore.prog Lexer_fpcore.token lexbuf in
        let tiny_ast = Fpcore_ast.fpcore_to_tiny prgm in

        (* Get vars from env *)
        (* build env *)
        let input_vars = Fpcore_ast.input_vars prgm in
        let ast_vars = Fpcore_ast.vars_of_ast_stm tiny_ast in
        let env = (
        Fpcore_ast.StringSet.fold (
            fun v env -> Typing.decl_var_type Ast.RealT env (v, Location.dummy ())
        ) (Fpcore_ast.StringSet.union input_vars ast_vars) Typing.empty_env
        ) in
        (* extract vars *)
        let vars = Typing.vars_of_env env in

        vars, tiny_ast
    with exn -> (
        match exn with
        | Failure (s) -> print_endline ("parsing failed: " ^ s); exit 1
        | _ -> print_endline ("parsing failed: " ^ (Lexing.lexeme lexbuf)); exit 2
    )
)