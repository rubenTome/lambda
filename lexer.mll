
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Top"       { TOP }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING } (*Add string data type*)
  | "concat"    { CONCAT } (*Add concat for string data type*)
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT } (*Add dot for projection*)
  | '='         { EQ }  (*Used for global context definitions and for records*)
  | ':'         { COLON } (*Used for records*)
  | "->"        { ARROW }
  | "{"         { LBRACE } (*Used for records and tuples*)
  | "}"         { RBRACE } (*Used for records and tuples*)
  | ","         { COMMA } (*Used for records and tuples*)
  | "["         { LBRACKET } (*Used for lists*)
  | "]"         { RBRACKET } (*Used for lists*)
  | ";"         { SEMICOLON } (*Used for lists*)
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | '"'[^ '"' ';' '\n']*'"'   
                { let s = (Lexing.lexeme lexbuf) 
                  in STRV (String.sub s 1 (String.length s - 2)) }
  | _           { raise Lexical_error } 

