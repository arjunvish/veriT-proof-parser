%{
(* This parser is adapted from SMTCoq's SMTLIB parser *)

open Lexing
open Format
open SmtlibAst

(* Hashtable to store function definitions *)
let fun_map = Hashtbl.create 10

(* Traverse the term "body" and replace all variable occurrences of
   x by y*)
let rec traverse_and_replace (x : sorted_term) (y : sorted_term) (body : sorted_term) : sorted_term = 
match body with
  | STrue | SFalse | SNot _ | SAnd (_,_) | SOr (_,_)
  | SImpl (_,_) | SXor (_,_) | Bvbin _ | Bvhex _ | SError _ -> body
  | SVar a -> (match x,y with 
              | SVar m, SVar n when (a = m) -> y
              | SVar _, SVar _ -> SVar a
              | _ -> SError "Non-variables involved in actual/formal parameters of define-fun")
  | SEq (a,b) -> SEq ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | SIte (f,a,b) -> SIte (f, (traverse_and_replace x y a), (traverse_and_replace x y b))
  | SBvult (a,b) -> SBvult ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | SBvule (a,b) -> SBvule ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | SBvugt (a,b) -> SBvugt ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | SBvuge (a,b) -> SBvuge ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | SBvslt (a,b) -> SBvslt ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | SBvsle (a,b) -> SBvsle ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | SBvsgt (a,b) -> SBvsgt ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | SBvsge (a,b) -> SBvsge ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Appl (s,args) -> let new_args = (List.map (traverse_and_replace x y) args) in Appl (s,new_args)
  | Select (a,b) -> Select ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Store (a,b,c) -> Store ((traverse_and_replace x y a), (traverse_and_replace x y b), (traverse_and_replace x y c))
  | Bvand (a,b) -> Bvand ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvor (a,b) -> Bvor ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvxor (a,b) -> Bvxor ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvnand (a,b) -> Bvnand ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvnor (a,b) -> Bvnor ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvxnor (a,b) -> Bvxnor ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvmul (a,b) -> Bvmul ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvadd (a,b) -> Bvadd ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvsub (a,b) -> Bvsub ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvudiv (a,b) -> Bvudiv ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvurem (a,b) -> Bvurem ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvsdiv (a,b) -> Bvsdiv ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvsrem (a,b) -> Bvsrem ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvsmod (a,b) -> Bvsmod ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvshl (a,b) -> Bvshl ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvlshr (a,b) -> Bvlshr ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvashr (a,b) -> Bvashr ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvconcat (a,b) -> Bvconcat ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvneg a -> Bvneg (traverse_and_replace x y a)
  | Bvnot a -> Bvnot (traverse_and_replace x y a)
  | Bvextract (i,j,a) -> Bvextract (i,j, (traverse_and_replace x y a))
  | Bvzeroext (i,a) -> Bvzeroext (i, (traverse_and_replace x y a))
  | Bvsignext (i,a) -> Bvsignext (i, (traverse_and_replace x y a))
  | Bvlrotate (i,a) -> Bvlrotate (i, (traverse_and_replace x y a))
  | Bvrrotate (i,a) -> Bvrrotate (i, (traverse_and_replace x y a))
  | Bvrepeat (i,a) -> Bvrepeat (i, (traverse_and_replace x y a))
  | Bvcomp (a,b) -> Bvcomp ((traverse_and_replace x y a), (traverse_and_replace x y b))

(* For each of the variable pairs (x,y) in arg pairs, 
   replace x by y in body *)  
let rec traverse_and_replace_all (arg_pairs : (sorted_term * sorted_term) list) (body : sorted_term) : sorted_term =
match arg_pairs with 
  | (x,y) :: t -> traverse_and_replace_all t (traverse_and_replace x y body)
  | [] -> body

(* Replace all formal_args by actual_args in the term body *)
let replace (formal_args : sorted_term list) (actual_args : sorted_term list) (body : sorted_term) : sorted_term=
  let arg_pairs = List.combine formal_args actual_args in
  traverse_and_replace_all arg_pairs body

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
    { (SVar $2, $3) }
;

letargs:
  | LPAREN l=list(letarg) RPAREN
    { l }
;

sorted_term:
  | TRUE { STrue }
  | FALSE { SFalse }
  | LPAREN NOT f=formula RPAREN 
    { SNot f }
  | LPAREN AND f1=formula f2=formula RPAREN
    { SAnd (f1, f2) }
  | LPAREN OR f1=formula f2=formula RPAREN
    { SOr (f1, f2) }
  | LPAREN IMPL f1=formula f2=formula RPAREN
    { SImpl (f1, f2) }
  | LPAREN XOR f1=formula f2=formula RPAREN
    { SXor (f1, f2) }
  | LPAREN EQUALS s1=sorted_term s2=sorted_term RPAREN
    { SEq (s1, s2) } 
  | LPAREN ITE f=formula s1=sorted_term s2=sorted_term RPAREN
    { SIte (f, s1, s2) }
  | LPAREN LET letargs sorted_term RPAREN
    { (traverse_and_replace_all $3 $4) }
  | LPAREN BVULT s1=sorted_term s2=sorted_term RPAREN
    { SBvule (s1, s2) }
  | LPAREN BVULE s1=sorted_term s2=sorted_term RPAREN
    { SBvule (s1, s2) }
  | LPAREN BVUGT s1=sorted_term s2=sorted_term RPAREN
    { SBvugt (s1, s2) }
  | LPAREN BVUGE s1=sorted_term s2=sorted_term RPAREN
    { SBvuge (s1, s2) }
  | LPAREN BVSLT s1=sorted_term s2=sorted_term RPAREN
    { SBvslt (s1, s2) }
  | LPAREN BVSLE s1=sorted_term s2=sorted_term RPAREN
    { SBvsle (s1, s2) }
  | LPAREN BVSGT s1=sorted_term s2=sorted_term RPAREN
    { SBvsgt (s1, s2) }
  | LPAREN BVSGE s1=sorted_term s2=sorted_term RPAREN
    { SBvsge (s1, s2) }
  | IDENT { SVar $1 }
  | LPAREN IDENT actual_args=list(sorted_term) RPAREN
    { match (Hashtbl.find fun_map $2) with
      | (formal_args, body) -> replace formal_args actual_args body
      | exception Not_found -> Appl ($2, actual_args) }
  | LPAREN SELECT s1=sorted_term s2=sorted_term RPAREN
    { Select (s1, s2) }
  | LPAREN STORE s1=sorted_term s2=sorted_term s3=sorted_term RPAREN
    { Store (s1, s2, s3) }
  | BVBIN
    { Bvbin $1 }
  | BVHEX
    { Bvhex $1 }
  | LPAREN BVAND s1=sorted_term s2=sorted_term RPAREN
    { Bvand (s1, s2) }
  | LPAREN BVOR s1=sorted_term s2=sorted_term RPAREN
    { Bvor (s1, s2) }
  | LPAREN BVXOR s1=sorted_term s2=sorted_term RPAREN
    { Bvxor (s1, s2) }
  | LPAREN BVNAND s1=sorted_term s2=sorted_term RPAREN
    { Bvnand (s1, s2) }
  | LPAREN BVNOR s1=sorted_term s2=sorted_term RPAREN
    { Bvnor (s1, s2) }
  | LPAREN BVXNOR s1=sorted_term s2=sorted_term RPAREN
    { Bvxnor (s1, s2) }
  | LPAREN BVMUL s1=sorted_term s2=sorted_term RPAREN
    { Bvmul (s1, s2) }
  | LPAREN BVADD s1=sorted_term s2=sorted_term RPAREN
    { Bvadd (s1, s2) }
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
  | LPAREN ITE f1=formula f2=formula f3=formula RPAREN
    { Ite(f1, f2, f3) }
  | IDENT { Var $1 }
  | LPAREN BVULT s1=sorted_term s2=sorted_term RPAREN
    { Bvule (s1, s2) }
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

sortedarg:
  | LPAREN IDENT sort RPAREN
    { SVar $2 }
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