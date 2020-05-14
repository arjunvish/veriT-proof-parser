%{
(* This translator is adapted from SMTCoq's LFSC translator *)

open Lexing
open Format
open SmtlibAst

(* A hashmap that maps BV variables to their bit-lengths *)
let var_map = Hashtbl.create 10

let concat_sp_sep_2 a b = "("^a^" "^b^")"
let concat_sp_sep_3 a b c = "("^a^" "^b^" "^c^")"
let concat_sp_sep_4 a b c d = "("^a^" "^b^" "^c^" "^d^")"
let concat_sp_sep_5 a b c d e = "("^a^" "^b^" "^c^" "^d^" "^e^")"
let concat_sp_sep_6 a b c d e f = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^")"
let concat_sp_sep_7 a b c d e f g = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^" "^g^")"
let concat_sp_sep_8 a b c d e f g h = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^" "^g^" "^h^")"
%}

%token <string> IDENT
%token <string> ANYTHING
%token <int> INT
%token LPAREN RPAREN EOF LAMBDA BIGLAMBDA COLON CHECK HOLE VAR HOLDS CLC CLN POS NEG
%token SATLEM_SIMPLIFY SATLEM RRES QRES CONCAT_CL CLR CNFN CNFC CNF_HOLDS CNFN_PROOF CNFC_PROOF
%token TH_HOLDS TRUE FALSE NOT AND OR IMPL TYPE
%token IFF XOR IFTE TERM EQUALS ITE LET FLET BOOL P_APP T_TRUE T_FALSE T_T_NEQ_F PRED_EQ_T PRED_EQ_F F_TO_B
%token TRUE_PREDS_EQUAL FALSE_PREDS_EQUAL PRED_REFL_POS PRED_REFL_NEG PRED_NOT_IFF_F PRED_NOT_IFF_F_2
%token PRED_NOT_IFF_T PRED_NOT_IFF_T_2 PRED_IFF_F PRED_IFF_F_2 PRED_IFF_T PRED_IFF_T_2
%token DECL_ATOM DECL_BVATOM CLAUSIFY_FORM CLAUSIFY_FORM_NOT CLAUSIFY_FALSE TH_LET_PF
%token IFF_SYMM CONTRAD TRUTH NOT_NOT_INTRO NOT_NOT_ELIM OR_ELIM_1 OR_ELIM_2 NOT_OR_ELIM AND_ELIM_1 AND_ELIM_2
%token NOT_AND_ELIM IMPL_INTRO IMPL_ELIM NOT_IMPL_ELIM IFF_ELIM_1 IFF_ELIM_2 NOT_IFF_ELIM XOR_ELIM_1 XOR_ELIM_2
%token NOT_XOR_ELIM  ITE_ELIM_1 ITE_ELIM_2 ITE_ELIM_3 NOT_ITE_ELIM_1 NOT_ITE_ELIM_2
%token NOT_ITE_ELIM_3 AST ASF BV_AST BV_ASF TRUST TRUST_F ARROW APPLY REFL SYMM TRANS NEGSYMM NEGTRANS1 NEGTRANS2 CONG
%token ARRAY SORT READ WRITE ROW1 ROW NEGATIVEROW EXT VARBV BITVEC AVARBV TRUSTBAD
%token ABV BVC BVN B0 B1 BVDISEQ BVAND BVOR BVXOR BVNAND BVNOR BVXNOR BVMUL BVADD BVSUB BVUDIV BVUREM
%token BVSDIV BVSREM BVSMOD BVSHL BVLSHR BVASHR BVCONCAT BVNEG BVNOT BVLROTATE BVRROTATE BVCOMP
%token BVEXTRACT BVZEROEXT BVSIGNEXT BVREPEAT BVULT BVULE BVUGT BVUGE BVSLT BVSLE BVSGT BVSGE
%token BBLTN BBLTC TRUSTBBLASTTERM DECLBBLAST DECLBBLASTWITHALIAS BITOF BVBBLCONST BVBBLVAR INTROASSUMPT INTROASSUMPF
%token BVBBLEQ BVBBLNEQ BVBBLEQSWAP BVBBLCONCAT BVBBLEXTRACT BVBBLZEROEXT BVBBLSIGNEXT
%token BVBBLBVAND BVBBLBVNOT BVBBLBVOR BVBBLBVXOR BVBBLBVADD BVBBLBVNEG BVBBLBVMUL BVBBLBVULT BVBBLBVSLT
%token BVBBLBVCOMP

%token HASH_SEMI SC PROGRAM AT MPQ MPZ KIND PI

%start command
%type <SmtlibAst.sorted_term list> command
%%

bblt:
  | BBLTN { "" }
  | LPAREN BBLTC formula bblt RPAREN { "" }
  | HOLE { "" }
  | IDENT { "" }
;

bblast_term:
  | LPAREN TRUSTBBLASTTERM int_or_hole sorted_term bblt RPAREN { "" }
  | LPAREN BVBBLCONST int_or_hole bblt bvconst RPAREN { "" }
  | LPAREN BVBBLVAR int_or_hole sorted_term bblt RPAREN { "" }
  | LPAREN BVBBLCONCAT int_or_hole int_or_hole int_or_hole sorted_term 
    sorted_term bblt bblt bblt bblast_term bblast_term RPAREN
    { "" }
  | LPAREN BVBBLEXTRACT int_or_hole int_or_hole int_or_hole int_or_hole sorted_term
    bblt bblt bblast_term RPAREN
    { "" }
  | LPAREN BVBBLZEROEXT int_or_hole int_or_hole int_or_hole sorted_term bblt bblt bblast_term RPAREN
    { "" }
  | LPAREN BVBBLSIGNEXT int_or_hole int_or_hole int_or_hole sorted_term bblt bblt bblast_term RPAREN
    { "" }
  | LPAREN BVBBLBVAND int_or_hole sorted_term sorted_term bblt bblt bblt bblast_term bblast_term RPAREN
    { "" }
  | LPAREN BVBBLBVNOT int_or_hole sorted_term bblt bblt bblast_term RPAREN
    { "" }
  | LPAREN BVBBLBVOR  int_or_hole sorted_term sorted_term bblt bblt bblt bblast_term bblast_term RPAREN
    { "" }
  | LPAREN BVBBLBVXOR int_or_hole sorted_term sorted_term bblt bblt bblt bblast_term bblast_term RPAREN
    { "" }
  | LPAREN BVBBLBVADD int_or_hole sorted_term sorted_term bblt bblt bblt bblast_term bblast_term RPAREN
    { "" }
  | LPAREN BVBBLBVNEG int_or_hole sorted_term bblt bblt bblast_term RPAREN
    { "" }
  | LPAREN BVBBLBVMUL int_or_hole sorted_term sorted_term bblt bblt bblt bblast_term bblast_term RPAREN
    { "" }
  | LPAREN BVBBLBVCOMP int_or_hole sorted_term sorted_term bblt bblt bblt bblast_term bblast_term RPAREN
    { "" }
  | IDENT { "" }
;

proof_term:
  | LPAREN SATLEM clause clause proof_term proof_term RPAREN { "" }
  | LPAREN SATLEM_SIMPLIFY clause clause clause proof_term proof_term RPAREN { "" }
  | LPAREN RRES clause clause proof_term proof_term IDENT RPAREN { "" }
  | LPAREN QRES clause clause proof_term proof_term IDENT RPAREN { "" }
  | LPAREN LAMBDA IDENT proof_term RPAREN { "" }
  | CNFN_PROOF { "" }
  | LPAREN CNFC_PROOF clause clause cnf proof_term proof_term RPAREN { "" }
  | T_T_NEQ_F { "" }
  | LPAREN PRED_EQ_T sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_EQ_F sorted_term holds_formula RPAREN { "" }
  | LPAREN TRUE_PREDS_EQUAL sorted_term sorted_term holds_formula holds_formula RPAREN { "" }
  | LPAREN FALSE_PREDS_EQUAL sorted_term sorted_term holds_formula holds_formula RPAREN { "" }
  | LPAREN PRED_REFL_POS sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_REFL_NEG sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_NOT_IFF_F sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_NOT_IFF_F_2 sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_NOT_IFF_T sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_NOT_IFF_T_2 sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_IFF_F sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_IFF_F_2 sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_IFF_T sorted_term holds_formula RPAREN { "" }
  | LPAREN PRED_IFF_T_2 sorted_term holds_formula RPAREN { "" }
  | LPAREN DECL_ATOM formula proof_term RPAREN { "" }
  | LPAREN DECL_BVATOM formula proof_term RPAREN { "" }
  | LPAREN CLAUSIFY_FORM formula proof_term proof_term proof_term RPAREN { "" }
  | LPAREN CLAUSIFY_FORM_NOT formula proof_term proof_term proof_term RPAREN { "" }
  | LPAREN CLAUSIFY_FALSE proof_term RPAREN { "" }
  | LPAREN TH_LET_PF formula proof_term proof_term RPAREN { "" }
  | LPAREN IFF_SYMM formula RPAREN { "" }
  | LPAREN CONTRAD formula proof_term proof_term RPAREN { "" }
  | TRUTH { "" }
  | LPAREN NOT_NOT_INTRO formula proof_term RPAREN { "" }
  | LPAREN NOT_NOT_ELIM formula proof_term RPAREN { "" }
  | LPAREN OR_ELIM_1 formula formula proof_term proof_term RPAREN { "" }
  | LPAREN OR_ELIM_2 formula formula proof_term proof_term RPAREN { "" }
  | LPAREN NOT_OR_ELIM formula formula proof_term RPAREN { "" }
  | LPAREN AND_ELIM_1 formula formula proof_term RPAREN { "" }
  | LPAREN AND_ELIM_2 formula formula proof_term RPAREN { "" }
  | LPAREN NOT_AND_ELIM formula formula proof_term RPAREN { "" }
  | LPAREN IMPL_INTRO formula formula proof_term RPAREN { "" }
  | LPAREN IMPL_ELIM formula formula proof_term RPAREN { "" }
  | LPAREN NOT_IMPL_ELIM formula formula proof_term RPAREN { "" }
  | LPAREN IFF_ELIM_1 formula formula proof_term RPAREN { "" }
  | LPAREN IFF_ELIM_2 formula formula proof_term RPAREN { "" }
  | LPAREN NOT_IFF_ELIM formula formula proof_term RPAREN { "" }
  | LPAREN XOR_ELIM_1 formula formula proof_term RPAREN { "" }
  | LPAREN XOR_ELIM_2 formula formula proof_term RPAREN { "" }
  | LPAREN NOT_XOR_ELIM formula formula proof_term RPAREN { "" }
  | LPAREN ITE_ELIM_1 formula formula formula proof_term RPAREN { "" }
  | LPAREN ITE_ELIM_2 formula formula formula proof_term RPAREN { "" }
  | LPAREN ITE_ELIM_3 formula formula formula proof_term RPAREN { "" }
  | LPAREN NOT_ITE_ELIM_1 formula formula formula proof_term RPAREN { "" }
  | LPAREN NOT_ITE_ELIM_2 formula formula formula proof_term RPAREN { "" }
  | LPAREN NOT_ITE_ELIM_3 formula formula formula proof_term RPAREN { "" }
  | LPAREN AST proof_term formula clause proof_term proof_term RPAREN { "" }
  | LPAREN ASF proof_term formula clause proof_term proof_term RPAREN { "" }
  | LPAREN BV_ASF proof_term proof_term formula clause proof_term proof_term proof_term RPAREN { "" }
  | LPAREN BV_AST proof_term proof_term formula clause proof_term proof_term proof_term RPAREN { "" }
  | TRUST { "" }
  | LPAREN TRUST_F formula RPAREN { "" }
  | LPAREN REFL sort sorted_term RPAREN
  | LPAREN SYMM sort sorted_term sorted_term proof_term RPAREN { "" }
  | LPAREN TRANS sort sorted_term sorted_term sorted_term proof_term proof_term RPAREN { "" }
  | LPAREN NEGSYMM sort sorted_term sorted_term proof_term RPAREN { "" }
  | LPAREN NEGTRANS1 sort sorted_term sorted_term sorted_term proof_term proof_term RPAREN { "" }
  | LPAREN NEGTRANS2 sort sorted_term sorted_term sorted_term proof_term proof_term RPAREN { "" }
  | LPAREN CONG sort sort sorted_term sorted_term sorted_term sorted_term proof_term proof_term RPAREN { "" }
  | LPAREN ROW1 sort sort sorted_term sorted_term sorted_term RPAREN { "" }
  | LPAREN ROW sort sort sorted_term sorted_term sorted_term sorted_term proof_term RPAREN { "" }
  | LPAREN NEGATIVEROW sort sort sorted_term sorted_term sorted_term sorted_term proof_term RPAREN { "" }
  | LPAREN EXT sort sort sorted_term sorted_term proof_term RPAREN { "" }
  | TRUSTBAD { "" }
  | LPAREN BVDISEQ int_or_hole bvconst bvconst RPAREN { "" }
  | LPAREN DECLBBLAST int_or_hole bblt sorted_term bblast_term proof_term RPAREN { "" }
  | LPAREN DECLBBLASTWITHALIAS int_or_hole bblt sorted_term sorted_term bblast_term proof_term proof_term RPAREN { "" }
  | LPAREN INTROASSUMPT formula proof_term clause proof_term proof_term proof_term RPAREN { "" }
  | LPAREN INTROASSUMPF formula proof_term clause proof_term proof_term proof_term RPAREN { "" }
  | LPAREN BVBBLEQ int_or_hole sorted_term sorted_term bblt bblt formula bblast_term bblast_term RPAREN { "" }
  | LPAREN BVBBLNEQ int_or_hole sorted_term sorted_term bblt bblt formula bblast_term bblast_term RPAREN { "" }
  | LPAREN BVBBLEQSWAP int_or_hole sorted_term sorted_term bblt bblt formula bblast_term bblast_term RPAREN { "" }
  | LPAREN BVBBLBVULT int_or_hole sorted_term sorted_term bblt bblt formula bblast_term bblast_term RPAREN { "" }
  | LPAREN BVBBLBVSLT int_or_hole sorted_term sorted_term bblt bblt formula bblast_term bblast_term RPAREN { "" }
  | HOLE { "" }
  | IDENT { "" }
;

lit:
  | LPAREN POS IDENT RPAREN
    { concat_sp_sep_2 "lit.pos" ($3) }
  | LPAREN NEG IDENT RPAREN
    { concat_sp_sep_2 "lit.neg" ($3) }
;

clause:
  | CLN { "[]" }
  | LPAREN CLC lit clause RPAREN
    { ($3^" :: "^$4) }
  | LPAREN CONCAT_CL clause clause RPAREN
    { ("concat_cl "^$3^" "^$4) }
  | LPAREN CLR lit clause RPAREN
    { ("clr "^$3^" "^$4) }
  | HOLE { "" }
  | IDENT { "" }
;

cnf:
  | CNFN { "[]" }
  | LPAREN CNFC clause cnf RPAREN
    { ("("^$3^") :: ("^$4^")") }
  | HOLE { "" }
;

fixed_sort:
  | BOOL { "Bool" }
  | LPAREN ARRAY sort sort RPAREN 
    { (concat_sp_sep_3 "Array" $3 $4) }
  | LPAREN BITVEC INT RPAREN
    { (concat_sp_sep_3 "_" "BitVec" (string_of_int($3))) }
  | IDENT { $1 }
;

arrow_sort_rec:
  | LPAREN ARROW fixed_sort arrow_sort_rec RPAREN
    { (" "^$3^$4) }
  | fixed_sort { (") "^$1) }
;

arrow_sort_init:
  | LPAREN ARROW fixed_sort arrow_sort_rec RPAREN
    { ("("^$3^$4) }
;

sort:
  | fixed_sort { $1 }
  | arrow_sort_init { $1 }
  | HOLE { "" }
;

bvconst:
  | LPAREN BVC B0 bvconst RPAREN { ("0"^$4) }
  | LPAREN BVC B1 bvconst RPAREN { ("1"^$4) }
  | BVN { "" }
;

int_or_hole:
  | INT { string_of_int($1) }
  | HOLE { "" }
;

sorted_bv_term:
  | LPAREN AVARBV INT IDENT RPAREN
    { let _ = (Hashtbl.add var_map $4 $3) in
      Var $4 }
  | LPAREN ABV int_or_hole bvconst RPAREN
    { Bvbin ("#b"^$4) }
  | LPAREN BVAND int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvand (s1, s2) }
  | LPAREN BVOR int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvor (s1, s2) }
  | LPAREN BVXOR int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvxor (s1, s2) }
  | LPAREN BVNAND int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvnand (s1, s2) }
  | LPAREN BVNOR int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvnor (s1, s2) }
  | LPAREN BVXNOR int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvxnor (s1, s2) }
  | LPAREN BVMUL int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvmul (s1, s2) }
  | LPAREN BVADD int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvadd (s1, s2) }
  | LPAREN BVSUB int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvsub (s1, s2) }
  | LPAREN BVUDIV int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvudiv (s1, s2) }
  | LPAREN BVUREM int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvurem (s1, s2) }
  | LPAREN BVSDIV int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvsdiv (s1, s2) }
  | LPAREN BVSREM int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvsrem (s1, s2) }
  | LPAREN BVSMOD int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvsmod (s1, s2) }
  | LPAREN BVSHL int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvshl (s1, s2) }
  | LPAREN BVLSHR int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvlshr (s1, s2) }
  | LPAREN BVASHR int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvashr (s1, s2) }
  | LPAREN BVCONCAT int_or_hole int_or_hole int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvconcat (s1, s2) }
  | LPAREN BVNEG int_or_hole s=sorted_term RPAREN
    { Bvneg s }
  | LPAREN BVNOT int_or_hole s=sorted_term RPAREN
    { Bvnot s }
  | LPAREN BVLROTATE i=INT s=sorted_term RPAREN
    { Bvlrotate (i, s) }
  | LPAREN BVRROTATE i=INT s=sorted_term RPAREN
    { Bvrrotate (i, s) }
  | LPAREN BVCOMP int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvcomp (s1, s2) }
  | LPAREN BVEXTRACT int_or_hole i=INT j=INT int_or_hole s=sorted_term RPAREN
    { Bvextract (i, j, s) }
  | LPAREN BVZEROEXT int_or_hole i=INT int_or_hole s=sorted_term RPAREN
    { Bvzeroext (i, s) }
  | LPAREN BVSIGNEXT int_or_hole i=INT int_or_hole s=sorted_term RPAREN
    { Bvzeroext (i, s) }
  | LPAREN BVREPEAT int_or_hole i=INT int_or_hole s=sorted_term RPAREN
    { Bvrepeat (i, s) }
;

apply_rec:
  | LPAREN APPLY sort sort apply_rec s=sorted_term RPAREN
    { s :: $5 }
  | LPAREN WRITE sort sort RPAREN 
    { (Var "store") :: [] }
  | LPAREN READ sort sort RPAREN 
    { (Var "select") :: [] }
  | IDENT 
    { (Var $1) :: [] }
;

apply_init:
  | LPAREN APPLY sort sort apply_rec s=sorted_term RPAREN
    { let l = s :: $5 in 
      let rev_l = List.rev l in
      match List.hd rev_l with
      | Var "store" -> 
        Store ((List.nth rev_l 1),(List.nth rev_l 2),(List.nth rev_l 3))
      | Var "select" -> 
        Select ((List.nth rev_l 1),(List.nth rev_l 2))
      | Var v -> Appl (v, (List.tl rev_l))
      | _ -> Error "apply_init" }
;

sorted_term:
  | LPAREN ITE sort f=formula s1=sorted_term s2=sorted_term RPAREN
    { Ite (f, s1, s2) }
  | T_TRUE 
    { True }
  | T_FALSE 
    { False }
  | LPAREN F_TO_B formula RPAREN
    { $3 }
  | apply_init 
    { $1 }
  | LPAREN WRITE sort sort RPAREN 
    { Error "sorted_term -> write"}
  | LPAREN READ sort sort RPAREN 
    { Error "sorted_term -> read"}
  | IDENT 
    { Var $1 }
  | sorted_bv_term { $1 }   
  | HOLE 
    { Error "sorted_term -> hole" }
;

formula:
  | TRUE 
    { True }
  | FALSE 
    { False }
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
  | LPAREN EQUALS sort s1=sorted_term s2=sorted_term RPAREN
    { Eq (s1, s2)}
  | LPAREN IFF f1=formula f2=formula RPAREN
    { Eq (f1, f2) }
  | LPAREN IFTE f1=formula f2=formula f3=formula RPAREN
    { Ite (f1, f2, f3)}
  | LPAREN P_APP sorted_term RPAREN 
    { $3 }
  | LPAREN BVULT int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvult (s1, s2) }
  | LPAREN BVULE int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvule (s1, s2) }
  | LPAREN BVUGT int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvugt (s1, s2) }
  | LPAREN BVUGE int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvuge (s1, s2) }
  | LPAREN BVSLT int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvslt (s1, s2) }
  | LPAREN BVSLE int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvsle (s1, s2) }
  | LPAREN BVSGT int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvsgt (s1, s2) }
  | LPAREN BVSGE int_or_hole s1=sorted_term s2=sorted_term RPAREN
    { Bvsge (s1, s2) }
  | LPAREN BITOF sorted_term int_or_hole RPAREN 
    { Error "formula -> bitof" }
  | LET sort sorted_term LPAREN LAMBDA IDENT formula RPAREN
    { Error "formula -> let" }
  | FLET formula LPAREN LAMBDA IDENT formula RPAREN
    { Error "formula -> flet" }
  | HOLE 
    { Error "formula -> hole" }
;

holds_term:
  | LPAREN HOLDS clause RPAREN
    { (Error "holds_term -> holds clause") }
  | LPAREN CNF_HOLDS cnf RPAREN
    { (Error "holds_term -> cnf_holds cnf") }
  | LPAREN TH_HOLDS formula RPAREN
    { ($3) }
;

holds_formula:
  | LPAREN TH_HOLDS formula RPAREN
    { ($3) }
  | IDENT { Error "holds_formula -> ident" }
;

typed_var:
  | IDENT VAR 
    { (Zilch ()) }
    (*{ "Parse error: typed_var->IDENT VAR" }*)
  | IDENT LPAREN TERM fixed_sort RPAREN
    { (Zilch ()) }
    (*{ (concat_sp_sep_4 "declare-fun" $1 "()" $4) }*)
  | IDENT LPAREN TERM arrow_sort_init RPAREN 
    { (Zilch ()) }
    (*{ (concat_sp_sep_3 "declare-fun" $1 $4) }*)
  | IDENT SORT 
    { (Zilch ()) }
    (*{ (concat_sp_sep_3 "declare-sort" $1 "0") }*)
  | IDENT holds_formula 
    { $2 }
    (*{ (concat_sp_sep_2 "assert" (to_string_sorted_term $2)) }*)
;

term:
  | LPAREN COLON holds_term proof_term RPAREN
    { [] }
    (*{ "" }*)
  | LPAREN BIGLAMBDA typed_var term RPAREN
    { $3 :: $4 }
    (*{ $3^"\n"^$4 }*)
  | LPAREN BIGLAMBDA IDENT VARBV term RPAREN
    { $5 }
    (*{ let rest = $5 in
      let len = (match (Hashtbl.find var_map $3) with 
      | i -> i
      | exception Not_found -> 1) in
      ((concat_sp_sep_5 "declare-fun" $3 "()" "(_ BitVec" (string_of_int(len)^")"))^"\n"^rest) } *)
;

check_command:
  | LPAREN CHECK term RPAREN
    { $3 }
    (*{ print_string 
      ("(set-logic ALL_SUPPORTED)\n"
        ^$3^"(check-sat)\n") }*)
;

command:
  | check_command EOF { $1 }
;