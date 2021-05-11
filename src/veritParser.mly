%{
(* This translator is adapted from SMTCoq's LFSC translator *)

open Lexing
open Format
%}

%token <string> SYMBOL
%token <string> ISYMBOL
%token <string> SPECCONST
%token <string> KEYWORD
%token <string> STRING
%token LPAREN RPAREN EOF COLON BANG
%token COLRULE COLSTEP COLARGS COLPREMISES
%token ASSUME STEP ANCHOR DEFINEFUN CL CHOICE AS
%token LET FORALL EXISTS MATCH

%token TRUE FALSE NOT IMPLIES AND OR XOR
%token ANDP ANDN ORP ORN XORP1 XORP2 XORN1 XORN2
%token IMPP IMPN1 IMPN2 NOTOR NOTAND XOR1 XOR2
%token NXOR1 NXOR2 IMP NIMP1 NIMP2 RES

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
  | TRUE { "" }
  | FALSE { "" }
  | LPAREN NOT term RPAREN { "" }
  | LPAREN IMPLIES term term RPAREN { "" }
  | LPAREN AND term term RPAREN { "" }
  | LPAREN OR term term RPAREN { "" }
  | LPAREN XOR term term RPAREN { "" }
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

function_def:
  | SYMBOL LPAREN sorted_var* RPAREN sort term { "" }
;

proof_arg:
  | SYMBOL { "" }
  | LPAREN SYMBOL term RPAREN { "" }
;

proof_args:
  | LPAREN proof_arg+ RPAREN { "" }
;

step_annot:
  | COLPREMISES LPAREN SYMBOL+ RPAREN { "" }
  | COLARGS proof_args { "" }
  | COLPREMISES LPAREN SYMBOL+ RPAREN COLARGS proof_args { "" }
;

rulename:
  | TRUE { "" }
  | FALSE { "" }
  | NOT { "" }
  | IMPLIES { "" }
  | AND { "" }
  | OR { "" }
  | XOR { "" }
  | ANDP { "" }
  | ANDN { "" }
  | ORP { "" }
  | ORN { "" }
  | XORP1 { "" }
  | XORP2 { "" }
  | XORN1 { "" }
  | XORN2 { "" }
  | IMPP { "" }
  | IMPN1 { "" }
  | IMPN2 { "" }
  | NOTOR { "" }
  | NOTAND { "" }
  | XOR1 { "" }
  | XOR2 { "" }
  | NXOR1 { "" }
  | NXOR2 { "" }
  | IMP { "" }
  | NIMP1 { "" }
  | NIMP2  { "" }
  | RES { "" }
;

proof_command:
  | LPAREN ASSUME SYMBOL t=term RPAREN { t }
  | LPAREN STEP SYMBOL clause COLRULE r=rulename step_annot RPAREN { r }
  | LPAREN ANCHOR COLSTEP SYMBOL RPAREN { "" }
  | LPAREN ANCHOR COLSTEP SYMBOL COLARGS proof_args RPAREN { "" }
  | LPAREN DEFINEFUN function_def RPAREN { "" }
;

proof:
  | p=proof_command* EOF { (List.fold_left (^) ("") p) }
;