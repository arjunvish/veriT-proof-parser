%{
(* This translator is adapted from SMTCoq's LFSC translator *)

open Lexing
open Format
%}

%token <string> IDENT
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
(*
sexpr:
  | SYM { "" }
  | KEYWORD { "" }
  | LPAREN sexpr* RPAREN { "" }
;

attr_val:
  | SPECCONST { "" }
  | SYM { "" }
  | LPAREN sexpr* RPAREN { "" }
;

attr:
  | KEYWORD { "" }
  | KEYWORD attr_val { "" }
;
*)
sort:
  | IDENT { "" }
  | IDENT sort+ { "" }
;

qual_id:
  | IDENT { "" }
  | LPAREN AS IDENT sort RPAREN { "" }
;
(*
var_binding:
  | LPAREN SYM term RPAREN { "" }
;

sorted_var:
  | LPAREN SYM term RPAREN { "" }
;
 
pattern:
  | SYM { "" }
  | LPAREN SYM SYM+ RPAREN { "" }
;

match_case:
  | LPAREN pattern term RPAREN { "" }
;
*)
term: (* term will produce many shift/reduce conflicts *)
  | SPECCONST { "" }
  | qual_id { "" }
  | LPAREN qual_id term+ RPAREN { "" }
  (*| LPAREN LET LPAREN var_binding+ RPAREN term RPAREN { "" }
  | LPAREN FORALL LPAREN sorted_var+ RPAREN term RPAREN { "" }
  | LPAREN EXISTS LPAREN sorted_var+ RPAREN term RPAREN { "" }
  | LPAREN MATCH term LPAREN match_case+ RPAREN RPAREN { "" }
  | LPAREN BANG term attr+ RPAREN { "" }*)
;

clause:
  | LPAREN CL term* RPAREN { "" }
;

proof_arg:
  | IDENT { "" }
  | LPAREN IDENT term RPAREN { "" }
;

step_annot:
  | COLPREMISES LPAREN s=IDENT+ RPAREN { "" }
  | COLARGS proof_arg+ { "" }
  | COLPREMISES LPAREN IDENT+ RPAREN COLARGS proof_arg+ { "" }
;

proof_command:
  | LPAREN ASSUME IDENT term RPAREN { "" }
  | LPAREN STEP IDENT clause COLRULE IDENT step_annot RPAREN { "" }
;

proof:
  | proof_command* EOF { "" }
;