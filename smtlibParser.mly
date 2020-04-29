%{
(* This parser is adapted from SMTCoq's SMTLIB parser *)

open Lexing
open Format
open SmtlibAst

let concat_sp_sep_1 a = "("^a^")"
let concat_sp_sep_2 a b = "("^a^" "^b^")"
let concat_sp_sep_3 a b c = "("^a^" "^b^" "^c^")"
let concat_sp_sep_4 a b c d = "("^a^" "^b^" "^c^" "^d^")"
let concat_sp_sep_5 a b c d e = "("^a^" "^b^" "^c^" "^d^" "^e^")"
let concat_sp_sep_6 a b c d e f = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^")"
let concat_sp_sep_7 a b c d e f g = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^" "^g^")"
let concat_sp_sep_8 a b c d e f g h = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^" "^g^" "^h^")"
%}

%token <string> IDENT
%token <int> INT
%token LPAREN RPAREN COLON EOF SETLOGIC TRUE FALSE NOT AND OR IMPL XOR EQUALS ITE BOOL HASH_SEMI
%token DECLARECONST DECLAREFUN CHECKSAT EXIT ASSERT
(*%token ARRAY SORT READ WRITE BITVEC BVAND BVOR BVXOR BVNAND BVNOR BVXNOR BVMUL BVADD BVSUB BVUDIV BVUREM
%token BVSDIV BVSREM BVSMOD BVSHL BVLSHR BVASHR BVCONCAT BVNEG BVNOT BVLROTATE BVRROTATE BVCOMP
%token BVEXTRACT BVZEROEXT BVSIGNEXT BVREPEAT BVULT BVULE BVUGT BVUGE BVSLT BVSLE BVSGT BVSGE*)


%start file
%type <unit> file
%%
sort:
  | BOOL
    { "Bool" }
;

sorted_term:
  | TRUE { BTrue }
  | FALSE { BFalse }
  | LPAREN ITE f=formula s1=sorted_term s2=sorted_term RPAREN
    { Ite (f, s1, s2) }
  | IDENT { TVar $1 }
;

formula:
  | TRUE { True }
  | FALSE { False }
  | LPAREN NOT f=formula RPAREN 
    { Not f }
  | LPAREN AND f1=formula f2=formula RPAREN
    { And (f1, f2) }
  | LPAREN OR f1=formula f2=formula RPAREN
    { Or (f1, f2) }
  | LPAREN IMPL f1=formula f2=formula RPAREN
    { Impl (f1, f2) }
  | LPAREN XOR f1=formula f2=formula RPAREN
    { Xor (f1, f2) }
  | LPAREN EQUALS s1=sorted_term s2=sorted_term RPAREN
    { Eq (s1, s2) }
  | IDENT { Var $1 }
;

assertion:
  | LPAREN ASSERT formula RPAREN
    { (concat_sp_sep_2 "assert" (to_string_form $3)) }
;

args:
  | LPAREN l=list(sort) RPAREN
    { let s = (String.concat " " l) in
      (concat_sp_sep_1 s)}
;

command:
  | LPAREN SETLOGIC IDENT RPAREN 
    { (concat_sp_sep_2 "set-logic" $3) }
  | LPAREN DECLARECONST IDENT sort RPAREN 
    { (concat_sp_sep_3 "declare-const" $3 $4) }
  | LPAREN DECLAREFUN IDENT args sort RPAREN
    { (concat_sp_sep_4 "declare-fun" $3 $4 $5) }
  | LPAREN CHECKSAT RPAREN
    { (concat_sp_sep_1 "checksat") }
  | LPAREN EXIT RPAREN
    { (concat_sp_sep_1 "exit") }
  | assertion
    { $1 }
;

file:
  | l=nonempty_list(command) EOF 
    { let s = ((String.concat "\n" l)^"\n") in
      print_string s }
;