
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;

let rec read_multiline str = 
  try 
    flush stdout;
    if (rindex_from str ((rindex str ';') - 1) ';')  == ((rindex str ';') - 1) 
      then sub str 0 (rindex_from str ((rindex str ';') - 1) ';')
    else 
      raise(Not_found)
  with 
    Not_found ->     
      let str2 = input_line stdin 
      in read_multiline (str^str2)
;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let c = s token (from_string (read_multiline "")) in
      loop (execute (vctx, tctx) c)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyctx, emptyctx)
  ;;

top_level_loop ()
;;

