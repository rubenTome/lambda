
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token TOP
%token BOOL
%token NAT
%token STRING
%token CONCAT

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF
%token LBRACE
%token RBRACE
%token COMMA
%token LBRACKET
%token RBRACKET
%token SEMICOLON

%token <int> INTV
%token <string> STRINGV
%token <string> STRV

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ term EOF
      { Bind ($1, $3) }
  | term EOF
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | CONCAT pathTerm pathTerm
      { TmConcat ($2, $3) }
  | appTerm pathTerm
      { TmApp ($1, $2) }

pathTerm:   
    pathTerm DOT INTV
      { TmProj ( $1, string_of_int $3) }
  | pathTerm DOT STRINGV
      { TmProj ( $1, $3) }
  | atomicTerm
      { $1 }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRV 
      { TmString $1 }
  | LBRACE tupleFields RBRACE
      { TmTuple $2 }
  | LBRACE recordFields RBRACE
      { TmRec $2 }
  | LBRACKET listFields RBRACKET
      { TmList $2 }

tupleFields:
    term
      { [$1] }
  | term COMMA tupleFields
      { $1::$3 }

recordFields:
      { [] }//rec vacio
  | notEmptyRecordFields
      { $1 }

notEmptyRecordFields:
    STRINGV EQ term
      { [($1, $3)] }
  | STRINGV EQ term COMMA notEmptyRecordFields
      { ($1, $3)::$5 }

listFields:
      { [] }//lista vacia
  | notEmptyList
      { $1 }

notEmptyList:
    term
      { [$1] }
  | term SEMICOLON notEmptyList
      { $1::$3 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 }
  | TOP
      { TyTop }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | LBRACE tupleFieldsTy RBRACE
      { TyTuple $2 }
  | LBRACE recordFieldsTy RBRACE
      { TyRec $2 }

tupleFieldsTy:
    ty
      { [$1] }
  | ty COMMA tupleFieldsTy
      { $1::$3 }

recordFieldsTy:
      { [] }//rec vacio
  | notEmptyRecordFieldsTy
      { $1 }

notEmptyRecordFieldsTy:
    STRINGV COLON ty
      { [($1, $3)] }
  | STRINGV COLON ty COMMA notEmptyRecordFieldsTy
      { ($1, $3)::$5 }