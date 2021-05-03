%{
(* This translator is adapted from SMTCoq's LFSC translator *)

open Lexing
open Format
%}

%token <string> IDENT
%token <int> INT
%token LPAREN RPAREN EOF COLON
%token COLRULE COLSTEP COLARGS COLPREMISES
%token ASSUME STEP ANCHOR DEFINEFUN CL CHOICE

%start proof
%type <string> proof
%%

proof_arg:
  | IDENT { "" }
  | LPAREN IDENT term RPAREN { "" }
;

step_annot:
  | COLPREMISES LPAREN s=IDENT+ RPAREN { "" }
  | COLARGS proof_arg+ { "" }
  | COLPREMISES LPAREN IDENT+ RPAREN COLARGS proof_arg+ { "" }
;

term:
  | IDENT term* { "" } (* This will produce a shift/reduce conflict : do I reduce IDENt or shift term*? *)
;

clause:
  | LPAREN CL term* RPAREN { "" }
;
 
proof_command:
  | LPAREN ASSUME IDENT term RPAREN { "" }
  | LPAREN STEP IDENT clause COLRULE IDENT step_annot RPAREN { "" }
;

proof:
  | proof_command* EOF { "" }
;