%{
(* This translator is adapted from SMTCoq's LFSC translator *)

open Lexing
open Format
%}

(*%token <string> IDENT*)
%token <string> SYMBOL
%token <string> ISYMBOL
%token <string> SPECCONST
%token <string> KEYWORD
%token <string> STRING
%token LPAREN RPAREN EOF COLON BANG
%token COLRULE COLSTEP COLARGS COLPREMISES
%token ASSUME STEP ANCHOR DEFINEFUN CL CHOICE AS
%token LET FORALL EXISTS MATCH

%start proof
%type <string> proof
%%

sexpr:
  | SYMBOL { "" }
  | KEYWORD { "" }
  | LPAREN sexpr* RPAREN { "" }
;

attr_val:
  | SPECCONST { "" }
  | SYMBOL { "" }
  | LPAREN sexpr* RPAREN { "" }
;

attr:
  | KEYWORD { "" }
  | KEYWORD attr_val { "" }
;

ident:
  | SYMBOL { "" }
  | ISYMBOL { "" }
;

sort:
  | ident { "" }
  | ident sort+ { "" }
;

qual_id:
  | ident { "" }
  | LPAREN AS ident sort RPAREN { "" }
;

var_binding:
  | LPAREN SYMBOL term RPAREN { "" }
;

sorted_var:
  | LPAREN SYMBOL term RPAREN { "" }
;
 
pattern:
  | SYMBOL { "" }
  | LPAREN SYMBOL SYMBOL+ RPAREN { "" }
;

match_case:
  | LPAREN pattern term RPAREN { "" }
;

term: (* term will produce many shift/reduce conflicts *)
  | SPECCONST { "" }
  | qual_id { "" }
  | LPAREN qual_id term+ RPAREN { "" }
  | LPAREN LET LPAREN var_binding+ RPAREN term RPAREN { "" }
  | LPAREN FORALL LPAREN sorted_var+ RPAREN term RPAREN { "" }
  | LPAREN EXISTS LPAREN sorted_var+ RPAREN term RPAREN { "" }
  | LPAREN MATCH term LPAREN match_case+ RPAREN RPAREN { "" }
  | LPAREN BANG term attr+ RPAREN { "" }
;

clause:
  | LPAREN CL term* RPAREN { "" }
;

proof_arg:
  | ident { "" }
  | LPAREN ident term RPAREN { "" }
;

step_annot:
  | COLPREMISES LPAREN s=ident+ RPAREN { "" }
  | COLARGS proof_arg+ { "" }
  | COLPREMISES LPAREN ident+ RPAREN COLARGS proof_arg+ { "" }
;

proof_command:
  | LPAREN ASSUME ident term RPAREN { "" }
  | LPAREN STEP ident clause COLRULE ident step_annot RPAREN { "" }
;

proof:
  | proof_command* EOF { "" }
;