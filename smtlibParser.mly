%{
(* This parser is adapted from SMTCoq's SMTLIB parser *)

open Lexing
open Format
open SmtlibAst
open ParserHelpers

(* Hashtable to store function definitions *)
let fun_map = Hashtbl.create 10
%}

%token <string> IDENT
%token <int> INT
%token <string> BVDEC
%token <string> BVBIN
%token <string> BVHEX
%token LPAREN RPAREN COLON EOF SETLOGIC TRUE FALSE NOT AND OR IMPL XOR EQUALS ITE BOOL HASH_SEMI
%token DECLARECONST DECLAREFUN DECLARESORT DEFINEFUN CHECKSAT EXIT LET ASSERT ARRAY SELECT STORE
%token BITVEC INDEX BVAND BVOR BVXOR BVNAND BVNOR BVXNOR BVMUL BVADD BVSUB BVUDIV BVUREM
%token BVSDIV BVSREM BVSMOD BVSHL BVLSHR BVASHR BVCONCAT BVNEG BVNOT BVLROTATE BVRROTATE BVCOMP
%token BVEXTRACT BVZEROEXT BVSIGNEXT BVREPEAT BVULT BVULE BVUGT BVUGE BVSLT BVSLE BVSGT BVSGE


%start file
%type <unit> file
%%
sort:
  | BOOL
    { "Bool" }
  | IDENT
    { $1 }
  | LPAREN ARRAY sort sort RPAREN
    { (concat_sp_sep_3 "Array" $3 $4) }
  | LPAREN INDEX BITVEC INT RPAREN
    { (concat_sp_sep_3 "_" "BitVec" (string_of_int $4)) }
;

letarg:
  | LPAREN IDENT sorted_term RPAREN
    { (Var $2, $3) }
;

letargs:
  | LPAREN l=list(letarg) RPAREN
    { l }
;

sorted_term:
  | TRUE { True }
  | FALSE { False }
  | LPAREN NOT s=sorted_term RPAREN 
    { Not s }
  | LPAREN AND args=list(sorted_term) RPAREN
    { (build_tree_right_ass "and" args) }
  | LPAREN OR args=list(sorted_term) RPAREN
    { (build_tree_right_ass "or" args) }
  | LPAREN IMPL s1=sorted_term s2=sorted_term RPAREN
    { Impl (s1, s2) }
  | LPAREN XOR args=list(sorted_term) RPAREN
    { if(List.length args = 2) then
        Xor ((List.nth args 0), (List.nth args 1))
      else
        (build_tree_left_ass "xor" args) }
  | LPAREN EQUALS s1=sorted_term s2=sorted_term RPAREN
    { Eq (s1, s2) } 
  | LPAREN ITE s1=sorted_term s2=sorted_term s3=sorted_term RPAREN
    { Ite (s1, s2, s3) }
  | LPAREN LET letargs sorted_term RPAREN
    { (traverse_and_replace_all_let $3 $4) }
  | LPAREN BVULT s1=sorted_term s2=sorted_term RPAREN
    { Bvult (s1, s2) }
  | LPAREN BVULE s1=sorted_term s2=sorted_term RPAREN
    { Bvule (s1, s2) }
  | LPAREN BVUGT s1=sorted_term s2=sorted_term RPAREN
    { Bvugt (s1, s2) }
  | LPAREN BVUGE s1=sorted_term s2=sorted_term RPAREN
    { Bvuge (s1, s2) }
  | LPAREN BVSLT s1=sorted_term s2=sorted_term RPAREN
    { Bvslt (s1, s2) }
  | LPAREN BVSLE s1=sorted_term s2=sorted_term RPAREN
    { Bvsle (s1, s2) }
  | LPAREN BVSGT s1=sorted_term s2=sorted_term RPAREN
    { Bvsgt (s1, s2) }
  | LPAREN BVSGE s1=sorted_term s2=sorted_term RPAREN
    { Bvsge (s1, s2) }
  | LPAREN SELECT s1=sorted_term s2=sorted_term RPAREN
    { Select (s1, s2) }
  | LPAREN STORE s1=sorted_term s2=sorted_term s3=sorted_term RPAREN
    { Store (s1, s2, s3) }
  | BVBIN
    { Bvbin $1 }
  | BVHEX
    { Bvhex $1 }
  | LPAREN INDEX s=BVDEC l=INT RPAREN
    { let len = (String.length s) in
      let i_num = Numeral.of_string (String.sub s 2 (len-2)) in
      let l_num = Numeral.of_int l in
        Bvbin (bv_dec_to_bin i_num l_num) }
  | LPAREN BVAND args=list(sorted_term) RPAREN
    { if(List.length args = 2) then
        Bvand ((List.nth args 0), (List.nth args 1))
      else
        (build_tree_left_ass "bvand" args) }
  | LPAREN BVOR args=list(sorted_term) RPAREN
    { if(List.length args = 2) then
        Bvor ((List.nth args 0), (List.nth args 1))
      else
        (build_tree_left_ass "bvor" args) }
  | LPAREN BVXOR args=list(sorted_term) RPAREN
    { if(List.length args = 2) then
        Bvxor ((List.nth args 0), (List.nth args 1))
      else
        (build_tree_left_ass "bvxor" args) }
  | LPAREN BVNAND s1=sorted_term s2=sorted_term RPAREN
    { Bvnand (s1, s2) }
  | LPAREN BVNOR s1=sorted_term s2=sorted_term RPAREN
    { Bvnor (s1, s2) }
  | LPAREN BVXNOR s1=sorted_term s2=sorted_term RPAREN
    { Bvxnor (s1, s2) }
  | LPAREN BVMUL args=list(sorted_term) RPAREN
    { if(List.length args = 2) then
        Bvmul ((List.nth args 0), (List.nth args 1))
      else
        (build_tree_left_ass "bvmul" args) }
  | LPAREN BVADD args=list(sorted_term) RPAREN
    { if(List.length args = 2) then
        Bvadd ((List.nth args 0), (List.nth args 1))
      else
        (build_tree_left_ass "bvadd" args) }
  | LPAREN BVSUB s1=sorted_term s2=sorted_term RPAREN
    { Bvsub (s1, s2) }
  | LPAREN BVUDIV s1=sorted_term s2=sorted_term RPAREN
    { Bvudiv (s1, s2) }
  | LPAREN BVUREM s1=sorted_term s2=sorted_term RPAREN
    { Bvurem (s1, s2) }
  | LPAREN BVSDIV s1=sorted_term s2=sorted_term RPAREN
    { Bvsdiv (s1, s2) }
  | LPAREN BVSREM s1=sorted_term s2=sorted_term RPAREN
    { Bvsrem (s1, s2) }
  | LPAREN BVSMOD s1=sorted_term s2=sorted_term RPAREN
    { Bvsmod (s1, s2) }
  | LPAREN BVSHL s1=sorted_term s2=sorted_term RPAREN
    { Bvshl (s1, s2) }
  | LPAREN BVLSHR s1=sorted_term s2=sorted_term RPAREN
    { Bvlshr (s1, s2) }
  | LPAREN BVASHR s1=sorted_term s2=sorted_term RPAREN
    { Bvashr (s1, s2) }
  | LPAREN BVCONCAT s1=sorted_term s2=sorted_term RPAREN
    { Bvconcat (s1, s2) }
  | LPAREN BVNEG s=sorted_term RPAREN
    { Bvneg s }
  | LPAREN BVNOT s=sorted_term RPAREN
    { Bvnot s }
  | LPAREN LPAREN INDEX BVEXTRACT i=INT j=INT RPAREN s=sorted_term RPAREN
    { Bvextract (i,j,s) }
  | LPAREN LPAREN INDEX BVZEROEXT i=INT RPAREN s=sorted_term RPAREN
    { Bvzeroext (i,s) }
  | LPAREN LPAREN INDEX BVSIGNEXT i=INT RPAREN s=sorted_term RPAREN
    { Bvsignext (i,s) }
  | LPAREN LPAREN INDEX BVLROTATE i=INT RPAREN s=sorted_term RPAREN
    { Bvlrotate (i,s) }
  | LPAREN LPAREN INDEX BVRROTATE i=INT RPAREN s=sorted_term RPAREN
    { Bvrrotate (i,s) }
  | LPAREN LPAREN INDEX BVREPEAT i=INT RPAREN s=sorted_term RPAREN
    { Bvrepeat (i,s) }
  | LPAREN BVCOMP s1=sorted_term s2=sorted_term RPAREN
    { Bvcomp (s1,s2) }
  | IDENT 
    { match (Hashtbl.find fun_map $1) with
      | (formal_args, body) -> replace formal_args [] body
      | exception Not_found -> Var $1 }
  | LPAREN IDENT actual_args=list(sorted_term) RPAREN
    { match (Hashtbl.find fun_map $2) with
      | (formal_args, body) -> replace formal_args actual_args body
      | exception Not_found -> Appl ($2, actual_args) }
  
;

assertion:
  | LPAREN ASSERT sorted_term RPAREN
    { (concat_sp_sep_2 "assert" (to_string_sorted_term $3)) }
;

args:
  | LPAREN l=list(sort) RPAREN
    { let s = (String.concat " " l) in
      (concat_sp_sep_1 s)}
;

sortedarg:
  | LPAREN IDENT sort RPAREN
    { Var $2 }
;

defargs:
  | LPAREN l=list(sortedarg) RPAREN
    { l }
;

command:
  | LPAREN SETLOGIC IDENT RPAREN 
    { (concat_sp_sep_2 "set-logic" $3) }
  | LPAREN DECLARECONST IDENT sort RPAREN 
    { (concat_sp_sep_3 "declare-const" $3 $4) }
  | LPAREN DECLAREFUN IDENT args sort RPAREN
    { (concat_sp_sep_4 "declare-fun" $3 $4 $5) }
  | LPAREN DECLARESORT IDENT INT RPAREN
    { (concat_sp_sep_3 "declare-sort" $3 (string_of_int $4)) }
  | LPAREN DEFINEFUN IDENT defargs sort sorted_term RPAREN
    { let _ = (Hashtbl.add fun_map $3 ($4,$6)) in 
      "" }
  | LPAREN CHECKSAT RPAREN
    { (concat_sp_sep_1 "check-sat") }
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