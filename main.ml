
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;

let rec read_multiline str = 
  try 
    flush stdout;
    sub str 0 (rindex_from str ((rindex str ';') - 1) ';' - 1)
  with 
    Not_found ->     
      let str2 = input_line stdin 
      in read_multiline (str^str2)
;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_multiline "")) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

