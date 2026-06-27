(*
 * TINY (Tiny Is Not Yasa (Yet Another Static Analyzer)):
 * a simple abstract interpreter for teaching purpose.
 * Copyright (C) 2012  P. Roux
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Parses the file.
    If a parsing error occurs, print a message and raise Report.Error *)

let token_to_str (token:Parser.token) = match token with 
  | WHILE -> "WHILE"
  | READIN -> "READIN"
  | READS -> "READS"
  | VAR (tag) -> "VAR(" ^ tag ^")"
  | UMINUS -> "UMINUS"
  | TRUE -> "TRUE"
  | TIMES -> "TIMES"
  | SEMICOL -> "SEMICOL"
  | DBLDOT -> "DBLPDOT"
  | RPAR -> "RPAR"
  | REALTYPE -> "REALTYPE"
  | RBRA -> "RBRA"
  | RAND_ITV_REAL -> "RARAND_ITV_REALND"
  | RAND_ITV_INT -> "RAND_ITV_INT"
  | RAND_ITV_FXP _ -> "RAND_ITV_FXP"
  | FXPCONV _ -> "FXPCONV"
  | RAND_BOOL -> "RAND_BOOL"
  | PLUS2 -> "PLUS2"
  | PLUS -> "PLUS"
  | OR -> "OR"
  | NUM(_, str, _) -> "NUM(*,"^str^",*)"
  | NOT -> "NOT"
  | MINUS2 -> "MINUS2"
  | MINUS -> "MINUS"
  | LT -> "LT"
  | LPAR -> "LPAR"
  | LE -> "LE"
  | LBRA -> "LBRA"
  | INTTYPE -> "INTTYPE"
  | MACINTTYPE (s,n) -> "MACINTTYPE" ^ (string_of_bool s) ^ ", " ^ (string_of_int n)
  | IF -> "IF"
  | GT -> "GT"
  | GE -> "GE"
  | NN -> "NN"
  | FALSE -> "FALSE"
  | EQUAL -> "EQUAL"
  | EQEQ -> "EQEQ"
  | EOF -> "EOF"
  | ELSE -> "ELSE"
  | DIV -> "DIV"
  | COMMA -> "COMMA"
  | BOOLTYPE -> "BOOLTYPE"
  | FIXTYPE _ -> "FIXTYPE"
  | FXP_CST _ -> "FXP_CST"
  | SHIFTLEFT  -> "SHIFTLEFT"
  | SHIFTRIGHT  -> "SHIFTRIGHT"
  | AND -> "AND"
  | STRING (s) -> "STRING(" ^ s ^ ")"
;;


let get_all_tokens lexbuf =
    let rec g () = 
    match Lexer.token lexbuf with EOF -> []
    | t -> t :: g () in
    g ()

let report_token_stream in_ch =  
  if true && !Report.verbosity >= 50 then 
  begin
    let lexbuf = Lexing.from_channel in_ch in
    let tokens = get_all_tokens lexbuf in
    let tokens_string = List.map token_to_str tokens in 
    let str_token_stream = String.concat " " tokens_string in
    Report.nlogf 5 ".. Token stream\n %s" str_token_stream
  end

let file filename =
  let env, ast = Utils.with_in_ch (Some filename) (fun in_ch ->
    report_token_stream in_ch;
    let lexbuf = Lexing.from_channel in_ch in
    try
      Location.filename := filename;
      Parser.file Lexer.token lexbuf;
    with
    | Lexer.Lexing_error s ->
      let loc = Location.get_current_from_lexbuf lexbuf in
      Report.error_loc loc "%s." s;
  
    (*| Failure _ *)
    | Parser.Error ->
      let loc = Location.get_current_from_lexbuf lexbuf in
      Report.error_loc loc "Syntax error.") in
  Report.nlogf 1 "Input parsed.";
  let ast =  Typing.type_stm env ast in
  Report.nlogf 1 "AST typed.";
  let vars = Typing.vars_of_env env in 
  Report.nlogf 2 "%a" Ast.fprint_stm ast;
   vars, ast
