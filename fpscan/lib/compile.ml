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

let real_s = "float"

let get_int_type sign useful_sz =
  let total = useful_sz + (if sign then 1 else 0) in
  let s =
    if total <= 8 then
      "int8_t"
    else if total <= 16 then
      "int16_t"
    else if total <= 32 then
      "int32_t"
    else if total <= 64 then
      "int64_t"
    else
      assert false (* too large *)
  in
  (if sign then "" else "u") ^ s

let pp_base_type fmt t =
  match t with
  | Ast.IntT | BoolT -> Printf.fprintf fmt "int"
  | RealT -> Printf.fprintf fmt "%s" real_s
  | FixedT fxp -> 
      Printf.fprintf fmt "%s" (get_int_type fxp.sign fxp.total)
  | MIntT (sign, total_including_sign) ->
    let sz = (if sign then -1 else 0) + total_including_sign in
      Printf.fprintf fmt "%s" (get_int_type sign sz)
    
    

let pp_var_decl fmt (v,t) =
  Printf.fprintf fmt "%a %s" pp_base_type t v
    
(* Print preamble to [fmt] plus declaration of variables [vars],
   returns number of lines outputed.*)
let print_preamble fmt vars =
  let ln = ref 0 in
  let preamble1 = 
{|#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <stdbool.h>
|} in
  Format.fprintf fmt "%s" preamble1;
  ln := !ln + Utils.lines_of_string preamble1;
  
  let fixed_types = Ast.get_fixed_formats vars in
  let define_types_s =
    let fmt =  Format.str_formatter in
    let open Fxp in
    List.iter (fun fxp ->
        Format.fprintf
          fmt
          "#define fxp_%c%i_%i %s@."
          (if fxp.sign then 's' else 'u')
          fxp.total
          fxp.frac
          (get_int_types fxp)
      ) (Fxp.Set.elements fixed_types);
    Format.flush_str_formatter () 
  in 
  Format.fprintf fmt "%s" define_types_s;
  ln := !ln + Utils.lines_of_string define_types_s;
  
  let typdef_s = 
    "typedef " ^ real_s ^ " real;\n"
  in
  Format.fprintf fmt "%s" typdef_s;
  ln := !ln + Utils.lines_of_string typdef_s;
  let print_bool_s = {|
bool print_bool(bool x){
  printf(x ? "true\n" : "false\n");
}
|} and
 print_int_s = {|
bool print_int(int x){
  printf("%d\n", x);
}
|} and
 print_real_s = {|
bool print_real(|}^ real_s ^ {| x){
  printf("%f\n", x);
}
|} and
    print_fxp_s = {|
bool print_fxp(|} ^ "int i, int64_t" ^ {| x){
   printf("%f\n", ((double) x) / (2<<i));
}
|} in
  Format.fprintf fmt "%s" print_bool_s;
  Format.fprintf fmt "%s" print_int_s;
  Format.fprintf fmt "%s" print_real_s;
  Format.fprintf fmt "%s" print_fxp_s;

  let rand_int_s = {|
int rand_int(int n1, int n2)
{
  int res;

  if (n2 < n1) exit(2);

  res = n1 + rand() % (n2 - n1 + 1);
  printf("rand: %d\n", res);
  return res;
}
|}
  in

  Format.fprintf fmt "%s" rand_int_s;
  ln := !ln + Utils.lines_of_string rand_int_s;
  let rand_real_s = 
     "int rand_real(" ^ real_s ^ " n1, " ^ real_s ^ " n2)\n\
     {\n  \
       " ^ real_s ^ " res;\n\
     \n  \
       if (n2 < n1) exit(2);\n\
     \n  \
       res = (" ^ real_s ^ ")rand()/RAND_MAX * (n2 - n1) + n1;\n  \
       printf(\"rand: %f\\n\", res);\n\
     \n  \
       return res;\n\
     }\n\
      \n"
  in
  Format.fprintf fmt "%s" rand_real_s;
  ln := !ln + Utils.lines_of_string rand_real_s;
  let main_s =
    "int main(int argc, char *argv[])\n\
     {\n"
  in
  Format.fprintf fmt "%s" main_s;
  ln := !ln + Utils.lines_of_string main_s;
  let preamble2 =
    "\n  \
       srand(time(NULL));\n\
     \n" in
  Format.fprintf fmt "%s" preamble2;
  ln := !ln + Utils.lines_of_string preamble2;
  Format.fprintf fmt "#define rand(x, y) rand_itv(x, y)\n";
  incr ln;
  (* print definition of fxp types *)
  !ln

(* Print end to [out_ch] with C code to print final values of [var]. *)
let print_end fmt vars =
  Format.fprintf fmt "  printf(\"At end of execution:\\n\");\n";
  Ast.Var.Set.iter
    (fun (n,t) ->
       Format.fprintf fmt  "  printf(\"%s = \");" n;
       let _ = 
         match t with 
         | Ast.IntT | Ast.MIntT _ -> Format.fprintf fmt   " print_int(%s); " n
         | Ast.BoolT -> Format.fprintf fmt  " print_bool(%s); " n
         | Ast.RealT -> Format.fprintf fmt  " print_real(%s); " n
         | Ast.FixedT fxp -> Format.fprintf fmt  " print_fxp(%i,%s); " fxp.frac n
       in
       Format.fprintf fmt  "printf(\"\\n\");\n"
          
    )
    vars;
  Format.fprintf fmt "%s%!"
    "\n  \
       return 0;\n\
     }\n"

let compile input_filename output_filename =
  let output_filename_string =
    match output_filename with None -> "stdout" | Some f -> f in
  Report.nlogf 1 "Compile file %s to %s." input_filename output_filename_string;
  let vars, _ = Parse.file input_filename in
  Utils.with_out_ch output_filename (fun fmt ->
    let ln = ref (print_preamble fmt vars) in
    Format.fprintf fmt "#line 1 \"%s\"\n" input_filename;
    Utils.with_in_ch (Some input_filename) (fun in_ch ->
      try
        while true do
          let l = input_line in_ch in
          Format.fprintf fmt "%s\n" l;
          incr ln
        done
      with End_of_file -> ());
    Format.fprintf fmt "\n#line %d \"%s\"\n" (!ln + 4)
      output_filename_string;
    print_end fmt vars);
  Report.nlogf 1 "Compiled to %s." output_filename_string
