let concat_sp_sep_2 a b = "("^a^" "^b^")"
let concat_sp_sep_3 a b c = "("^a^" "^b^" "^c^")"
let concat_sp_sep_4 a b c d = "("^a^" "^b^" "^c^" "^d^")"
let concat_sp_sep_5 a b c d e = "("^a^" "^b^" "^c^" "^d^" "^e^")"
let concat_sp_sep_6 a b c d e f = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^")"
let concat_sp_sep_7 a b c d e f g = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^" "^g^")"
let concat_sp_sep_8 a b c d e f g h = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^" "^g^" "^h^")"

type sorted_term = 
  | Var of string
  | True
  | False
  | Not of sorted_term
  | And of sorted_term * sorted_term
  | Or of sorted_term * sorted_term
  | Impl of sorted_term * sorted_term
  | Xor of sorted_term * sorted_term
  | Eq of sorted_term * sorted_term
  | Ite of sorted_term * sorted_term * sorted_term
  | Bvult of sorted_term * sorted_term 
  | Bvule of sorted_term * sorted_term
  | Bvugt of sorted_term * sorted_term
  | Bvuge of sorted_term * sorted_term
  | Bvslt of sorted_term * sorted_term
  | Bvsle of sorted_term * sorted_term
  | Bvsgt of sorted_term * sorted_term
  | Bvsge of sorted_term * sorted_term
  | Appl of string * sorted_term list
  | Select of sorted_term * sorted_term
  | Store of sorted_term * sorted_term * sorted_term
  | Bvbin of string
  | Bvhex of string
  | Bvand of sorted_term * sorted_term
  | Bvor of sorted_term * sorted_term
  | Bvxor of sorted_term * sorted_term
  | Bvnand of sorted_term * sorted_term
  | Bvnor of sorted_term * sorted_term
  | Bvxnor of sorted_term * sorted_term
  | Bvmul of sorted_term * sorted_term
  | Bvadd of sorted_term * sorted_term
  | Bvsub of sorted_term * sorted_term
  | Bvudiv of sorted_term * sorted_term
  | Bvurem of sorted_term * sorted_term
  | Bvsdiv of sorted_term * sorted_term
  | Bvsrem of sorted_term * sorted_term
  | Bvsmod of sorted_term * sorted_term
  | Bvshl of sorted_term * sorted_term
  | Bvlshr of sorted_term * sorted_term
  | Bvashr of sorted_term * sorted_term
  | Bvconcat of sorted_term * sorted_term
  | Bvneg of sorted_term
  | Bvnot of sorted_term
  | Bvextract of int * int * sorted_term
  | Bvzeroext of int * sorted_term
  | Bvsignext of int * sorted_term
  | Bvlrotate of int * sorted_term
  | Bvrrotate of int * sorted_term
  | Bvrepeat of int * sorted_term
  | Bvcomp of sorted_term * sorted_term
  | Error of string

type t = sorted_term list

let rec to_string_sorted_term =
  function
  | Var v -> v
  | True -> "true"
  | False -> "false"
  | Not f -> concat_sp_sep_2 "not" (to_string_sorted_term f)
  | And (x,y) -> concat_sp_sep_3 "and" (to_string_sorted_term x) (to_string_sorted_term y)
  | Or (x,y) -> concat_sp_sep_3 "or" (to_string_sorted_term x) (to_string_sorted_term y)
  | Impl (x,y) -> concat_sp_sep_3 "=>" (to_string_sorted_term x) (to_string_sorted_term y)
  | Xor (x,y) -> concat_sp_sep_3 "xor" (to_string_sorted_term x) (to_string_sorted_term y)
  | Eq (x,y) -> concat_sp_sep_3 "=" (to_string_sorted_term x) (to_string_sorted_term y)
  | Ite (x,y,z) -> concat_sp_sep_4 "ite" (to_string_sorted_term x) (to_string_sorted_term y) 
                    (to_string_sorted_term z)
  | Bvult (x,y) -> concat_sp_sep_3 "bvult" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvule (x,y) -> concat_sp_sep_3 "bvule" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvugt (x,y) -> concat_sp_sep_3 "bvugt" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvuge (x,y) -> concat_sp_sep_3 "bvuge" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvslt (x,y) -> concat_sp_sep_3 "bvslt" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvsle (x,y) -> concat_sp_sep_3 "bvsle" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvsgt (x,y) -> concat_sp_sep_3 "bvsgt" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvsge (x,y) -> concat_sp_sep_3 "bvsge" (to_string_sorted_term x) (to_string_sorted_term y)
  | Appl (f, args) -> "("^f^" "^(String.concat " " (List.map to_string_sorted_term args))^")"
  | Select (x,y) -> concat_sp_sep_3 "select" (to_string_sorted_term x) (to_string_sorted_term y)
  | Store (x,y,z) -> concat_sp_sep_4 "store" (to_string_sorted_term x) (to_string_sorted_term y) 
                      (to_string_sorted_term z)
  | Bvbin x -> x
  | Bvhex x -> x
  | Bvand (x,y) -> concat_sp_sep_3 "bvand" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvor (x,y) -> concat_sp_sep_3 "bvor" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvxor (x,y) -> concat_sp_sep_3 "bvxor" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvnand (x,y) -> concat_sp_sep_3 "bvnand" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvnor (x,y) -> concat_sp_sep_3 "bvnor" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvxnor (x,y) -> concat_sp_sep_3 "bvxnor" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvmul (x,y) -> concat_sp_sep_3 "bvmul" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvadd (x,y) -> concat_sp_sep_3 "bvadd" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvsub (x,y) -> concat_sp_sep_3 "bvsub" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvudiv (x,y) -> concat_sp_sep_3 "bvudiv" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvurem (x,y) -> concat_sp_sep_3 "bvurem" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvsdiv (x,y) -> concat_sp_sep_3 "bvsdiv" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvsrem (x,y) -> concat_sp_sep_3 "bvsrem" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvsmod (x,y) -> concat_sp_sep_3 "bvsmod" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvshl (x,y) -> concat_sp_sep_3 "bvshl" (to_string_sorted_term x) (to_string_sorted_term y)  
  | Bvlshr (x,y) -> concat_sp_sep_3 "bvlshr" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvashr (x,y) -> concat_sp_sep_3 "bvashr" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvconcat (x,y) -> concat_sp_sep_3 "bvconcat" (to_string_sorted_term x) (to_string_sorted_term y)
  | Bvneg x -> concat_sp_sep_2 "bvneg" (to_string_sorted_term x)
  | Bvnot x -> concat_sp_sep_2 "bvnot" (to_string_sorted_term x)
  | Bvextract (i,j,x) -> concat_sp_sep_2 
                          (concat_sp_sep_4 "_" "extract" (string_of_int i) (string_of_int j))
                          (to_string_sorted_term x)
  | Bvzeroext (i,x) -> concat_sp_sep_2 
                          (concat_sp_sep_3 "_" "zero_extend" (string_of_int i))
                          (to_string_sorted_term x)
  | Bvsignext (i,x) -> concat_sp_sep_2 
                          (concat_sp_sep_3 "_" "sign_extend" (string_of_int i))
                          (to_string_sorted_term x)
  | Bvlrotate (i,x) -> concat_sp_sep_2 
                          (concat_sp_sep_3 "_" "rotate_left" (string_of_int i))
                          (to_string_sorted_term x)
  | Bvrrotate (i,x) -> concat_sp_sep_2 
                          (concat_sp_sep_3 "_" "rotate_right" (string_of_int i))
                          (to_string_sorted_term x)
  | Bvrepeat (i,x) -> concat_sp_sep_2 
                          (concat_sp_sep_3 "_" "repeat" (string_of_int i))
                          (to_string_sorted_term x)
  | Bvcomp (x,y) -> concat_sp_sep_3 "bvcomp" (to_string_sorted_term x) (to_string_sorted_term y)
  | Error x -> ("Error: "^x)

let rec to_string (l : t) = 
  let l_str = (List.map (to_string_sorted_term) (l)) in
  (String.concat "\n" l_str)