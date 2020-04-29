%{
(* This parser is adapted from SMTCoq's SMTLIB parser *)

open Lexing
open Format

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
%token DECLARECONST CHECKSAT EXIT ASSERT
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
  | TRUE { "true" }
  | FALSE { "false" }
  | LPAREN ITE formula sorted_term sorted_term RPAREN
    { concat_sp_sep_4 "ite" $3 $4 $5 }
  | IDENT { $1 }
;

formula:
  | TRUE { "true" }
  | FALSE { "false" }
  | LPAREN NOT formula RPAREN 
    { concat_sp_sep_2 "not" $3 }
  | LPAREN AND formula formula RPAREN
    { concat_sp_sep_3 "and" $3 $4 }
  | LPAREN OR formula formula RPAREN
    { concat_sp_sep_3 "or" $3 $4 }
  | LPAREN IMPL formula formula RPAREN
    { concat_sp_sep_3 "=>" $3 $4 }
  | LPAREN XOR formula formula RPAREN
    { concat_sp_sep_3 "xor" $3 $4 }
  | LPAREN EQUALS sorted_term sorted_term RPAREN
    { concat_sp_sep_3 "=" $3 $4 }
  | IDENT { $1 }
;

assertion:
  | LPAREN ASSERT formula RPAREN
    { (concat_sp_sep_2 "assert" $3) }
;

command:
  | LPAREN SETLOGIC IDENT RPAREN 
    { (concat_sp_sep_2 "set-logic" $3) }
  | LPAREN DECLARECONST IDENT sort RPAREN 
    { (concat_sp_sep_3 "declare-const" $3 $4) }
  | LPAREN CHECKSAT RPAREN
    { (concat_sp_sep_1 "checksat") }
  | LPAREN EXIT RPAREN
    { (concat_sp_sep_1 "exit") }
  | assertion
    { $1 }
;

file:
  | l=nonempty_list(command) EOF 
    { let l1 = (List.map (fun x -> x^"\n") l) in 
      let s = (List.fold_left (^) "" l1) in
      print_string s }
;