open Lexing
open Format
open SmtlibAst

(* Traverse the term "body" and replace all variable occurrences of
   x by y*)
let rec traverse_and_replace_let (x : sorted_term) (y : sorted_term) (body : sorted_term) : sorted_term = 
match body with
  | True | False | Bvbin _ | Bvhex _ | Error _ | Zilch _ -> body
  | Var a -> (match x with 
              | Var m when (a = m) -> y
              | Var _ -> body
              | _ -> Error "Non-variables involved in formal parameters of let")
  | Not a -> Not (traverse_and_replace_let x y a)
  | And (a,b) -> And ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Or (a,b) -> Or ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Impl (a,b) -> Impl ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Xor (a,b) -> Xor ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))      
  | Eq (a,b) -> Eq ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Ite (f,a,b) -> Ite (f, (traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvult (a,b) -> Bvult ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvule (a,b) -> Bvule ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvugt (a,b) -> Bvugt ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvuge (a,b) -> Bvuge ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvslt (a,b) -> Bvslt ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvsle (a,b) -> Bvsle ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvsgt (a,b) -> Bvsgt ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvsge (a,b) -> Bvsge ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Appl (s,args) -> let new_args = (List.map (traverse_and_replace_let x y) args) in Appl (s,new_args)
  | Select (a,b) -> Select ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Store (a,b,c) -> Store ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b), (traverse_and_replace_let x y c))
  | Bvand (a,b) -> Bvand ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvor (a,b) -> Bvor ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvxor (a,b) -> Bvxor ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvnand (a,b) -> Bvnand ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvnor (a,b) -> Bvnor ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvxnor (a,b) -> Bvxnor ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvmul (a,b) -> Bvmul ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvadd (a,b) -> Bvadd ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvsub (a,b) -> Bvsub ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvudiv (a,b) -> Bvudiv ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvurem (a,b) -> Bvurem ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvsdiv (a,b) -> Bvsdiv ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvsrem (a,b) -> Bvsrem ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvsmod (a,b) -> Bvsmod ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvshl (a,b) -> Bvshl ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvlshr (a,b) -> Bvlshr ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvashr (a,b) -> Bvashr ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvconcat (a,b) -> Bvconcat ((traverse_and_replace_let x y a), (traverse_and_replace_let x y b))
  | Bvneg a -> Bvneg (traverse_and_replace_let x y a)
  | Bvnot a -> Bvnot (traverse_and_replace_let x y a)
  | Bvextract (i,j,a) -> Bvextract (i,j, (traverse_and_replace_let x y a))
  | Bvzeroext (i,a) -> Bvzeroext (i, (traverse_and_replace_let x y a))
  | Bvsignext (i,a) -> Bvsignext (i, (traverse_and_replace_let x y a))
  | Bvlrotate (i,a) -> Bvlrotate (i, (traverse_and_replace_let x y a))
  | Bvrrotate (i,a) -> Bvrrotate (i, (traverse_and_replace_let x y a))
  | Bvrepeat (i,a) -> Bvrepeat (i, (traverse_and_replace_let x y a))
  | Bvcomp (a,b) -> Bvcomp ((traverse_and_replace_let x y a)  , (traverse_and_replace_let x y b))

(* For each of the variable pairs (x,y) in arg pairs, 
   replace x by y in body *)  
let rec traverse_and_replace_all_let (arg_pairs : (sorted_term * sorted_term) list) (body : sorted_term) : sorted_term =
match arg_pairs with 
  | (x,y) :: t -> traverse_and_replace_all_let t (traverse_and_replace_let x y body)
  | [] -> body


(* Traverse the term "body" and replace all variable occurrences of
   x by y*)
let rec traverse_and_replace (x : sorted_term) (y : sorted_term) (body : sorted_term) : sorted_term = 
match body with
  | True | False | Bvbin _ | Bvhex _ | Error _ | Zilch _ -> body
  | Var a -> (match x with 
              | Var m when (a = m) -> y
              | Var _ -> Var a
              | _ -> Error "Non-variables involved in actual/formal parameters of define-fun")
  | Not a -> Not (traverse_and_replace x y a)
  | And (a,b) -> And ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Or (a,b) -> Or ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Impl (a,b) -> Impl ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Xor (a,b) -> Xor ((traverse_and_replace x y a), (traverse_and_replace x y b))      
  | Eq (a,b) -> Eq ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Ite (f,a,b) -> Ite (f, (traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvult (a,b) -> Bvult ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvule (a,b) -> Bvule ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvugt (a,b) -> Bvugt ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvuge (a,b) -> Bvuge ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvslt (a,b) -> Bvslt ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvsle (a,b) -> Bvsle ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvsgt (a,b) -> Bvsgt ((traverse_and_replace x y a), (traverse_and_replace x y b))
  | Bvsge (a,b) -> Bvsge ((traverse_and_replace x y a), (traverse_and_replace x y b))
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
let rec replace (formal_args : sorted_term list) (actual_args : sorted_term list) (body : sorted_term) : sorted_term =
  let arg_pairs = List.combine formal_args actual_args in
  traverse_and_replace_all arg_pairs body

(* Take a multi-arity operator in SMTLIB and build a tree for its 
   right associative version in LFSC *)
let rec build_tree_right_ass (s : string) (args : sorted_term list) : sorted_term =
  if (List.length args = 2) then
    match s with
      | "and" -> And ((List.nth args 0),(List.nth args 1))
      | "or" -> Or ((List.nth args 0),(List.nth args 1))
      | _ -> Error "Error with multi-arity right associative operator"
  else
    match s with
      | "and" -> And ((List.nth args 0),(build_tree_right_ass s (List.tl args)))
      | "or" -> Or ((List.nth args 0),(build_tree_right_ass s (List.tl args)))
      | _ -> Error "Error with multi-arity right associative operator"

(* Take a multi-arity operator in SMTLIB and build a tree for its 
   keft associative version in LFSC *)
let rec build_tree_left_ass (s : string) (args : sorted_term list) : sorted_term =
  let rev_args = (List.rev args) in
  if (List.length args = 2) then
    match s with
      | "xor" -> Xor ((List.nth rev_args 0),(List.nth rev_args 1))
      | "bvand" -> Bvand ((List.nth rev_args 0),(List.nth rev_args 1))
      | "bvor" -> Bvor ((List.nth rev_args 0),(List.nth rev_args 1))
      | "bvxor" -> Bvxor ((List.nth rev_args 0),(List.nth rev_args 1))
      | "bvadd" -> Bvadd ((List.nth rev_args 0),(List.nth rev_args 1))
      | "bvmul" -> Bvmul ((List.nth rev_args 0),(List.nth rev_args 1))
      | _ -> Error "Error with multi-arity left associative operator"
  else
    match s with
      | "xor" -> Xor ((build_tree_left_ass s (List.tl rev_args)),(List.nth rev_args 0))
      | "bvand" -> Bvand ((build_tree_left_ass s (List.tl rev_args)),(List.nth rev_args 0))
      | "bvor" -> Bvor ((build_tree_left_ass s (List.tl rev_args)),(List.nth rev_args 0))
      | "bvxor" -> Bvxor ((build_tree_left_ass s (List.tl rev_args)),(List.nth rev_args 0))
      | "bvadd" -> Bvadd ((build_tree_left_ass s (List.tl rev_args)),(List.nth rev_args 0))
      | "bvmul" -> Bvmul ((build_tree_left_ass s (List.tl rev_args)),(List.nth rev_args 0))
      | _ -> Error "Error with multi-arity left associative operator"

(* The mod operator in OCaml implements remainder 
with respect to numeral division. Since numeral division
in OCaml rounds toward 0, we design modulo which considers 
division that rounds toward negative infinity. 
For example, -1 mod 8 is -1 (with quotient 0) in OCaml, 
we want it to be 7 (with quotient -1).
While considering a mod b, the OCaml mod operator will do what 
we want when a and b are positive. The following function will 
additionally do what we want when a is negative; it wont do what 
we want when b is negative, but that's okay since 
we don't consider cases of 
modulo-n arithmetic where n is negative. *)
let modulo (x : Numeral.t) (y : Numeral.t) : Numeral.t =
  let result = (Numeral.rem x y) in
    if (Numeral.geq result Numeral.zero) then result
  else (Numeral.add result y)

(* Function that calculates the nth power of two *)
let rec pow2 (n : Numeral.t) : Numeral.t =
  if (Numeral.equal n Numeral.zero) then
    Numeral.one
  else
    Numeral.mult (Numeral.succ (Numeral.one)) 
                 (pow2 (Numeral.sub n Numeral.one))

(* Function that returns unsigned fixed-width int or bitvector version of a numeral *)
let bv_dec_to_bin (i : Numeral.t) (size : Numeral.t) : string =
  (* x = 2^N for ubvN, where we need to 
  do n modulo x on the input n *)
  let m = pow2 size in
  let n = modulo i m in
  (* Tail-recursive function that converts n to type t,
  which is a list of bools *)
  let rec convert (acc : bool list) (l : Numeral.t) (n : Numeral.t) =
    if (Numeral.gt n Numeral.zero) then
      convert ((Numeral.equal (Numeral.rem n (Numeral.of_int 2)) Numeral.one) :: acc) 
              (Numeral.add l Numeral.one) (Numeral.div n (Numeral.of_int 2))
    else (acc, l)
  in
  let bv, l = convert [] Numeral.zero n in
  (* For n-bit BV, pad upto n bits with 0s *)
  let rec pad (bv : bool list) (l :Numeral.t) =
    if (Numeral.gt l Numeral.zero) then 
      pad (false :: bv) (Numeral.sub l Numeral.one) 
    else 
      bv
  in
  let rec bv_to_string (b : bool list) : string = 
    match b with
    | true :: t -> "1"^(bv_to_string t)
    | false :: t -> "0"^(bv_to_string t)
    | [] -> ""
  in
  ("#b"^(bv_to_string (pad bv (Numeral.sub size l))))

let concat_sp_sep_1 a = "("^a^")"
let concat_sp_sep_2 a b = "("^a^" "^b^")"
let concat_sp_sep_3 a b c = "("^a^" "^b^" "^c^")"
let concat_sp_sep_4 a b c d = "("^a^" "^b^" "^c^" "^d^")"
let concat_sp_sep_5 a b c d e = "("^a^" "^b^" "^c^" "^d^" "^e^")"
let concat_sp_sep_6 a b c d e f = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^")"
let concat_sp_sep_7 a b c d e f g = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^" "^g^")"
let concat_sp_sep_8 a b c d e f g h = "("^a^" "^b^" "^c^" "^d^" "^e^" "^f^" "^g^" "^h^")"