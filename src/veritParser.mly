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

%start command
%type <string> command
%%

command:
  | EOF { "" }
;