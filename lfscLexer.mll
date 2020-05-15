{
(**************************************************************************)
(*                                                                        *)
(*     SMTCoq                                                             *)
(*     Copyright (C) 2011 - 2019                                          *)
(*                                                                        *)
(*     See file "AUTHORS" for the list of authors                         *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)


(* This parser is adapted from Jane Street sexplib parser *)

  open Printf
  open Lexing
  open LfscParser

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c -> c

  let lf = '\010'

  let dec_code c1 c2 c3 =
    100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

  let hex_code c1 c2 =
    let d1 = Char.code c1 in
    let val1 =
      if d1 >= 97 then d1 - 87
      else if d1 >= 65 then d1 - 55
      else d1 - 48 in
    let d2 = Char.code c2 in
    let val2 =
      if d2 >= 97 then d2 - 87
      else if d2 >= 65 then d2 - 55
      else d2 - 48 in
    val1 * 16 + val2

  let found_newline ({ lex_curr_p; _ } as lexbuf) diff =
    lexbuf.lex_curr_p <-
      {
        lex_curr_p with
        pos_lnum = lex_curr_p.pos_lnum + 1;
        pos_bol = lex_curr_p.pos_cnum - diff;
      }

  (* same length computation as in [Lexing.lexeme] *)
  let lexeme_len { lex_start_pos; lex_curr_pos; _ } = lex_curr_pos - lex_start_pos

  let main_failure lexbuf msg =
    let { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } = lexeme_start_p lexbuf in
    let msg =
      sprintf
        "Sexplib.Lexer.main: %s at line %d char %d"
        msg pos_lnum (pos_cnum - pos_bol)
    in
    failwith msg

  module type T = sig
    module Quoted_string_buffer : sig
      type t
      val create : int -> t
      val add_char : t -> char -> unit
      val add_substring : t -> string -> int -> int -> unit
      val add_lexeme : t -> lexbuf -> unit
      val clear : t -> unit
      val of_buffer : Buffer.t -> t
    end
    module Token : sig
      type t
      val lparen : t
      val rparen : t
      val lambda : t
      val biglam : t
      val pi : t
      val colon : t
      val hole : t
      val sc : t
      val at : t
      val integer : string -> t
      val ident : string -> t
      val anything : string -> t
      val eof : t
      val simple_string : string -> t
      val hash_semi : t
      val quoted_string : Lexing.position -> Quoted_string_buffer.t -> t
      val var : t
      val holds : t
      val clc : t
      val cln : t
      val pos : t
      val neg : t
      val satlem_simplify : t
      val satlem : t
      val concat_cl : t
      val clr : t
      val cnf_holds : t
      val cnfn : t
      val cnfc : t
      val cnfn_proof : t
      val cnfc_proof : t
      val th_holds : t
      val ftrue : t
      val ffalse : t
      val l_not : t
      val l_and : t
      val l_or : t
      val l_impl : t
      val l_iff : t
      val l_xor : t
      val l_ifte : t
      val term : t
      val equals : t
      val ite : t
      val t_let : t
      val flet : t
      val bool : t
      val p_app : t
      val t_true : t
      val t_false : t
      val t_t_neq_f : t
      val pred_eq_t : t
      val pred_eq_f : t
      val f_to_b : t
      val true_preds_equal : t
      val false_preds_equal : t
      val pred_refl_pos : t
      val pred_refl_neg : t
      val pred_not_iff_f : t
      val pred_not_iff_f_2 : t
      val pred_not_iff_t : t
      val pred_not_iff_t_2 : t
      val pred_iff_f : t
      val pred_iff_f_2 : t
      val pred_iff_t : t
      val pred_iff_t_2 : t
      val decl_atom : t
      val decl_bvatom : t
      val clausify_form : t
      val clausify_form_not : t
      val clausify_false : t
      val th_let_pf : t
      val iff_symm : t
      val contrad : t
      val truth : t
      val not_not_intro : t
      val not_not_elim : t
      val or_elim_1 : t
      val or_elim_2 : t
      val not_or_elim : t
      val and_elim_1 : t
      val and_elim_2 : t
      val not_and_elim : t
      val impl_intro : t
      val impl_elim : t
      val not_impl_elim : t
      val iff_elim_1 : t
      val iff_elim_2 : t
      val not_iff_elim : t
      val xor_elim_1 : t
      val xor_elim_2 : t
      val not_xor_elim : t
      val ite_elim_1 : t
      val ite_elim_2 : t
      val ite_elim_3 : t
      val not_ite_elim_1 : t
      val not_ite_elim_2 : t
      val not_ite_elim_3 : t
      val ast : t
      val asf : t
      val bv_ast : t
      val bv_asf : t
      val arrow : t
      val apply : t
      val trust : t
      val trust_f : t
      val refl : t
      val symm : t
      val trans : t
      val negsymm : t
      val negtrans1 : t
      val negtrans2 : t
      val sort : t
      val cong : t
      val arr : t
      val read : t
      val write : t
      val row1 : t
      val row : t
      val negativerow : t
      val ext : t
      val varbv : t
      val bitvec : t
      val avarbv : t
      val trustbad : t
      val abv : t
      val bvc : t
      val bvn : t
      val b0 : t
      val b1 : t
      val bvdiseq : t
      val bvand : t
      val bvor : t
      val bvxor : t
      val bvnand : t
      val bvnor : t
      val bvxnor : t
      val bvmul : t
      val bvadd : t
      val bvsub : t
      val bvudiv : t
      val bvurem : t
      val bvsdiv : t
      val bvsrem : t
      val bvsmod : t
      val bvshl : t
      val bvlshr : t
      val bvashr : t
      val bvconcat : t
      val bvneg : t
      val bvnot : t
      val bvrleft : t
      val bvrright : t
      val bvcomp : t
      val bvextract : t
      val bvzeroext : t
      val bvsignext : t
      val bvrepeat : t
      val bvult : t
      val bvule : t
      val bvugt : t
      val bvuge : t
      val bvslt : t
      val bvsle : t
      val bvsgt : t
      val bvsge : t
      val bbltn : t
      val bbltc : t
      val trust_bblast_term : t
      val decl_bblast : t
      val decl_bblast_with_alias : t
      val bitof : t
      val bv_bbl_const : t
      val bv_bbl_var : t
      val intro_assump_t : t
      val intro_assump_f : t
      val bv_bbl_eq : t
      val bv_bbl_neq : t
      val bv_bbl_eq_swap : t
      val bv_bbl_concat : t
      val bv_bbl_extract : t
      val bv_bbl_zero_extend : t
      val bv_bbl_sign_extend : t
      val bv_bbl_bvand : t
      val bv_bbl_bvnot : t
      val bv_bbl_bvor : t
      val bv_bbl_bvxor : t
      val bv_bbl_bvadd : t
      val bv_bbl_bvneg : t
      val bv_bbl_bvmul : t
      val bv_bbl_bvult : t
      val bv_bbl_bvslt : t
      val bv_bbl_bvcomp : t
      val rr_bv_eq : t
      val rr_bv_default : t
      val rres : t
      val qres : t
      type s = Quoted_string_buffer.t -> Lexing.lexbuf -> t
      val comment : string -> main:s -> s
      val block_comment : Lexing.position -> main:s -> s
    end
  end

  (* Create and populate a hashtable *)
  let mk_hashtbl init =
    let tbl = List.length init |> Hashtbl.create in
    init |> List.iter (fun (k, v) -> Hashtbl.add tbl k v) ;
    tbl

  let keywords = mk_hashtbl [
    ("check", CHECK);
    ("type", TYPE);
    ("kind", KIND);
    ("mpz", MPZ);
    ("mpq", MPQ);
    ("program", PROGRAM);
  ]

  module Make (X : T) : sig
    val main : ?buf:Buffer.t -> Lexing.lexbuf -> X.Token.t
  end = struct (* BEGIN FUNCTOR BODY CONTAINING GENERATED CODE *)
    open X

}

let anything = _*
let lf = '\010'
let lf_cr = ['\010' '\013']
let dos_newline = "\013\010"
let blank = [' ' '\009' '\012']
let unquoted = [^ ';' '(' ')' '"' '\\' ':' '@' '!' ] # blank # lf_cr
let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f' 'A'-'F']

let unquoted_start =
  unquoted # ['#' '|'] | '#' unquoted # ['|'] | '|' unquoted # ['#']

let integer = digit+
let ident = ('_')* ['a'-'z' 'A'-'Z' '\'' ]['a'-'z' 'A'-'Z' '0'-'9' '\\' '_']*


rule main buf = parse
  | lf | dos_newline { found_newline lexbuf 0;
                       main buf lexbuf }
  | blank+ { main buf lexbuf }
  | (';' (_ # lf_cr)*) as text { Token.comment text ~main buf lexbuf }
  | '(' { Token.lparen }
  | ')' { Token.rparen }
  | '\\' { Token.lambda }
  | '!' { Token.pi }
  | '%' { Token.biglam }
  | '_' { Token.hole }
  | ':' { Token.colon }
  | '^' { Token.sc }
  | '@' { Token.at }
  | "var" { Token.var }
  | "holds" { Token.holds }
  | "clc" { Token.clc }
  | "cln" { Token.cln }
  | "pos" { Token.pos }
  | "neg" { Token.neg }
  | "satlem_simplify" { Token.satlem_simplify }
  | "satlem" { Token.satlem }
  | "concat_cl" { Token.concat_cl }
  | "clr" { Token.clr }
  | "cnf_holds" { Token.cnf_holds }
  | "cnfn" { Token.cnfn }
  | "cnfc" { Token.cnfc }
  | "cnfn_proof" { Token.cnfn_proof }
  | "cnfc_proof" { Token.cnfc_proof }
  | "th_holds" { Token.th_holds }
  | "true" { Token.ftrue }
  | "false" { Token.ffalse }
  | "not" { Token.l_not }
  | "and" { Token.l_and }
  | "or" { Token.l_or }
  | "impl" { Token.l_impl }
  | "iff" { Token.l_iff }
  | "xor" { Token.l_xor }
  | "ifte" { Token.l_ifte }
  | "term" { Token.term }
  | "=" { Token.equals }
  | "ite" { Token.ite }
  | "let" { Token.t_let }
  | "flet" { Token.flet }
  | "Bool" { Token.bool }
  | "p_app" { Token.p_app }
  | "t_true" { Token.t_true }
  | "t_false" { Token.t_false }
  | "t_t_neq_f" { Token.t_t_neq_f }
  | "pred_eq_t" { Token.pred_eq_t }
  | "pred_eq_f" { Token.pred_eq_f }
  | "f_to_b" { Token.f_to_b }
  | "true_preds_equal" { Token.true_preds_equal }
  | "false_preds_equal" { Token.false_preds_equal }
  | "pred_refl_pos" { Token.pred_refl_pos }
  | "pred_refl_neg" { Token.pred_refl_neg }
  | "pred_not_iff_f" { Token.pred_not_iff_f }
  | "pred_not_iff_f_2" { Token.pred_not_iff_f_2 }
  | "pred_not_iff_t" { Token.pred_not_iff_t }
  | "pred_not_iff_t_2" { Token.pred_not_iff_t_2 }
  | "pred_iff_f" { Token.pred_iff_f }
  | "pred_iff_f_2" { Token.pred_iff_f_2 }
  | "pred_iff_t" { Token.pred_iff_t }
  | "pred_iff_t_2" { Token.pred_iff_t_2 }
  | "decl_atom" { Token.decl_atom }
  | "decl_bvatom" { Token.decl_bvatom }
  | "clausify_form" { Token.clausify_form }
  | "clausify_form_not" { Token.clausify_form_not }
  | "clausify_false" { Token.clausify_false }
  | "th_let_pf" { Token.th_let_pf }
  | "iff_symm" { Token.iff_symm }
  | "contra" { Token.contrad }
  | "truth" { Token.truth }
  | "not_not_intro" { Token.not_not_intro }
  | "not_not_elim" { Token.not_not_elim }
  | "or_elim_1" { Token.or_elim_1 }
  | "or_elim_2" { Token.or_elim_2 }
  | "not_or_elim" { Token.not_or_elim }
  | "and_elim_1" { Token.and_elim_1 }
  | "and_elim_2" { Token.and_elim_2 }
  | "not_and_elim" { Token.not_and_elim }
  | "impl_intro" { Token.impl_intro }
  | "impl_elim" { Token.impl_elim }
  | "not_impl_elim" { Token.not_impl_elim }
  | "iff_elim_1" { Token.iff_elim_1 }
  | "iff_elim_2" { Token.iff_elim_2 }
  | "not_iff_elim" { Token.not_iff_elim }
  | "xor_elim_1" { Token.xor_elim_1 }
  | "xor_elim_2" { Token.xor_elim_2 }
  | "not_xor_elim" { Token.not_xor_elim }
  | "ite_elim_1" { Token.ite_elim_1 }
  | "ite_elim_2" { Token.ite_elim_2 }
  | "ite_elim_3" { Token.ite_elim_3 }
  | "not_ite_elim_1" { Token.not_ite_elim_1 }
  | "not_ite_elim_2" { Token.not_ite_elim_2 }
  | "not_ite_elim_3" { Token.not_ite_elim_3 }
  | "ast" { Token.ast }
  | "asf" { Token.asf }
  | "bv_ast" { Token.bv_ast }
  | "bv_asf" { Token.bv_asf }
  | "arrow" { Token.arrow }
  | "apply" { Token.apply }
  | "trust" { Token.trust }
  | "trust_f" { Token.trust_f }
  | "refl" { Token.refl }
  | "symm" { Token.symm }
  | "trans" { Token.trans }
  | "negsymm" { Token.negsymm }
  | "negtrans1" { Token.negtrans1 }
  | "negtrans2" { Token.negtrans2 }
  | "cong" { Token.cong }
  | "Array" { Token.arr }
  | "sort" { Token.sort }
  | "read" { Token.read }
  | "write" { Token.write }
  | "row1" { Token.row1 }
  | "row" { Token.row }
  | "negativerow" { Token.negativerow }
  | "ext" { Token.ext }
  | "var_bv" { Token.varbv }
  | "BitVec" { Token.bitvec }
  | "a_var_bv" { Token.avarbv }
  | "trust-bad" { Token.trustbad }
  | "a_bv" { Token.abv }
  | "bvc" { Token.bvc }
  | "bvn" { Token.bvn }
  | "b0" { Token.b0 }
  | "b1" { Token.b1 }
  | "bv_disequal_constants" { Token.bvdiseq }
  | "bvand" { Token.bvand }
  | "bvor" { Token.bvor }
  | "bvxor" { Token.bvxor }
  | "bvnand" { Token.bvnand }
  | "bvnor" { Token.bvnor }
  | "bvxnor" { Token.bvxnor }
  | "bvmul" { Token.bvmul }
  | "bvadd" { Token.bvadd }
  | "bvsub" { Token.bvsub }
  | "bvudiv" { Token.bvudiv }
  | "bvurem" { Token.bvurem }
  | "bvsdiv" { Token.bvsdiv }
  | "bvsrem" { Token.bvsrem }
  | "bvsmod" { Token.bvsmod }
  | "bvshl" { Token.bvshl }
  | "bvlshr" { Token.bvlshr }
  | "bvashr" { Token.bvashr }
  | "concat" { Token.bvconcat }
  | "bvneg" { Token.bvneg }
  | "bvnot" { Token.bvnot }
  | "rotate_left" { Token.bvrleft }
  | "rotate_right" { Token.bvrright }
  | "bvcomp" { Token.bvcomp }
  | "extract" { Token.bvextract }
  | "zero_extend" { Token.bvzeroext }
  | "sign_extend" { Token.bvsignext }
  | "repeat" { Token.bvrepeat }
  | "bvult" { Token.bvult }
  | "bvule" { Token.bvule }
  | "bvugt" { Token.bvugt }
  | "bvuge" { Token.bvuge }
  | "bvslt" { Token.bvslt }
  | "bvsle" { Token.bvsle }
  | "bvsgt" { Token.bvsgt }
  | "bvsge" { Token.bvsge }
  | "bbltn" { Token.bbltn }
  | "bbltc" { Token.bbltc }
  | "trust_bblast_term" { Token.trust_bblast_term }
  | "decl_bblast" { Token.decl_bblast }
  | "decl_bblast_with_alias" { Token.decl_bblast_with_alias }
  | "bitof" { Token.bitof }
  | "bv_bbl_const" { Token.bv_bbl_const }
  | "bv_bbl_var" { Token.bv_bbl_var }
  | "intro_assump_t" { Token.intro_assump_t }
  | "intro_assump_f" { Token.intro_assump_f }
  | "bv_bbl_=" { Token.bv_bbl_eq }
  | "bv_bbl_=_false" { Token.bv_bbl_eq }
  | "bv_bbl_=_swap" { Token.bv_bbl_eq_swap }
  | "bv_bbl_concat" { Token.bv_bbl_concat }
  | "bv_bbl_extract" { Token.bv_bbl_extract }
  | "bv_bbl_zero_extend" { Token.bv_bbl_zero_extend }
  | "bv_bbl_sign_extend" { Token.bv_bbl_sign_extend }
  | "bv_bbl_bvand" { Token.bv_bbl_bvand }
  | "bv_bbl_bvnot" { Token.bv_bbl_bvnot }
  | "bv_bbl_bvor" { Token.bv_bbl_bvor }
  | "bv_bbl_bvxor" { Token.bv_bbl_bvxor }
  | "bv_bbl_bvadd" { Token.bv_bbl_bvadd }
  | "bv_bbl_bvneg" { Token.bv_bbl_bvneg }
  | "bv_bbl_bvmul" { Token.bv_bbl_bvmul }
  | "bv_bbl_bvult" { Token.bv_bbl_bvult }
  | "bv_bbl_bvslt" { Token.bv_bbl_bvslt }
  | "bv_bbl_bvcomp" { Token.bv_bbl_bvcomp } 
  | "rr_bv_eq" { Token.rr_bv_eq }
  | "rr_bv_default" { Token.rr_bv_default }
  | 'R' { Token.rres }  
  | 'Q' { Token.qres }
  | '(' '~' (integer as i) ')' {Token.integer ("-"^i) }
  | integer as i { Token.integer i }
  | '"'
      { 
        let pos = Lexing.lexeme_start_p lexbuf in
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf pos lexbuf;
        let tok = Token.quoted_string pos buf in
        Quoted_string_buffer.clear buf;
        tok
      }
  | "#;" { Token.hash_semi }
  | "#|"
      {
        let pos = Lexing.lexeme_start_p lexbuf in
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_block_comment buf [pos] lexbuf;
        let tok = Token.block_comment pos ~main buf lexbuf in
        Quoted_string_buffer.clear buf;
        tok
      }
  | "|#" { main_failure lexbuf "illegal end of comment" }
  | "#" "#"+ "|" unquoted* (* unquoted_start can match ##, so ##| (which should be
                              refused) would not not be parsed by this case if the regexp
                              on the left was not there *)
  | "|" "|"+ "#" unquoted*
  | unquoted_start unquoted* ("#|" | "|#") unquoted*
      { main_failure lexbuf "comment tokens in unquoted atom" }
  | "#" | "|" | unquoted_start unquoted* as str { Token.simple_string str }
  | eof { Token.eof }

and scan_string buf start = parse
  | '"' { Quoted_string_buffer.add_lexeme buf lexbuf; () }
  | '\\' lf [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 2 in
        found_newline lexbuf len;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' dos_newline [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 3 in
        found_newline lexbuf len;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
      {
        Quoted_string_buffer.add_char buf (char_for_backslash c);
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' (digit as c1) (digit as c2) (digit as c3)
      {
        let v = dec_code c1 c2 c3 in
        if v > 255 then (
          let { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } = lexeme_end_p lexbuf in
          let msg =
            sprintf
              "Sexplib.Lexer.scan_string: \
               illegal escape at line %d char %d: `\\%c%c%c'"
              pos_lnum (pos_cnum - pos_bol - 3)
              c1 c2 c3 in
          failwith msg);
        Quoted_string_buffer.add_char buf (Char.chr v);
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' 'x' (hexdigit as c1) (hexdigit as c2)
      {
        let v = hex_code c1 c2 in
        Quoted_string_buffer.add_char buf (Char.chr v);
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | '\\' (_ as c)
      {
        Quoted_string_buffer.add_char buf '\\';
        Quoted_string_buffer.add_char buf c;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | lf
      {
        found_newline lexbuf 0;
        Quoted_string_buffer.add_char buf lf;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | ([^ '\\' '"'] # lf)+
      {
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Quoted_string_buffer.add_substring buf (Bytes.to_string lexbuf.lex_buffer) ofs len;
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf start lexbuf
      }
  | eof
      {
        let msg =
          sprintf
            "Sexplib.Lexer.scan_string: unterminated string at line %d char %d"
            start.pos_lnum (start.pos_cnum - start.pos_bol)
        in
        failwith msg
      }

and scan_block_comment buf locs = parse
  | ('#'* | '|'*) lf
      { Quoted_string_buffer.add_lexeme buf lexbuf;
        found_newline lexbuf 0; scan_block_comment buf locs lexbuf }
  | (('#'* | '|'*) [^ '"' '#' '|'] # lf)+
      { Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_block_comment buf locs lexbuf }
  | ('#'* | '|'*) '"'
      {
        Quoted_string_buffer.add_lexeme buf lexbuf;
        let cur = lexeme_end_p lexbuf in
        let start = { cur with pos_cnum = cur.pos_cnum - 1 } in
        scan_string buf start lexbuf;
        scan_block_comment buf locs lexbuf
      }
  | '#'+ '|'
    {
      Quoted_string_buffer.add_lexeme buf lexbuf;
      let cur = lexeme_end_p lexbuf in
      let start = { cur with pos_cnum = cur.pos_cnum - 2 } in
      scan_block_comment buf (start :: locs) lexbuf
    }
  | '|'+ '#'
      {
        Quoted_string_buffer.add_lexeme buf lexbuf;
        match locs with
        | [_] -> () (* the comment is finished *)
        | _ :: (_ :: _ as t) -> scan_block_comment buf t lexbuf
        | [] -> assert false  (* impossible *)
      }
  | eof
      {
        match locs with
        | [] -> assert false
        | { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } :: _ ->
            let msg =
              sprintf "Sexplib.Lexer.scan_block_comment: \
                unterminated block comment at line %d char %d"
                pos_lnum (pos_cnum - pos_bol)
            in
            failwith msg
      }

{ (* RESUME FUNCTOR BODY CONTAINING GENERATED CODE *)

    let main ?buf =
      let buf =
        match buf with
        | None -> Quoted_string_buffer.create 64
        | Some buf ->
          Buffer.clear buf;
          Quoted_string_buffer.of_buffer buf
      in
      main buf

  end (* END FUNCTOR BODY CONTAINING GENERATED CODE *)

  module Vanilla =
    Make (struct
      module Quoted_string_buffer = struct
        include Buffer
        let add_lexeme _ _ = ()
        let of_buffer b = b
      end
      module Token = struct
        open LfscParser
        type t = token
        type s = Quoted_string_buffer.t -> Lexing.lexbuf -> t
        let eof = EOF
        let lparen = LPAREN
        let rparen = RPAREN
        let lambda = LAMBDA
        let pi = PI
        let sc = SC
        let hole = HOLE
        let colon = COLON
        let biglam =  BIGLAMBDA
        let at = AT
        let hash_semi = HASH_SEMI
        let integer i = INT (int_of_string i)
        let ident i =
          try Hashtbl.find keywords i with Not_found -> IDENT i
        let simple_string x =
          try Hashtbl.find keywords x with Not_found -> IDENT x
        let quoted_string _ buf = IDENT (Buffer.contents buf)
        let anything i = ANYTHING i
        let var = VAR
        let holds = HOLDS
        let clc = CLC
        let cln = CLN
        let pos = POS
        let neg = NEG
        let satlem_simplify = SATLEM_SIMPLIFY
        let satlem = SATLEM
        let concat_cl = CONCAT_CL
        let clr = CLR
        let cnf_holds = CNF_HOLDS
        let cnfn = CNFN
        let cnfc = CNFC
        let cnfn_proof = CNFN_PROOF
        let cnfc_proof = CNFC_PROOF
        let th_holds = TH_HOLDS
        let ftrue = TRUE
        let ffalse = FALSE
        let l_not = NOT
        let l_and = AND
        let l_or = OR
        let l_impl = IMPL
        let l_iff = IFF
        let l_xor = XOR
        let l_ifte = IFTE
        let term = TERM
        let equals = EQUALS
        let ite = ITE
        let t_let = LET
        let flet = FLET
        let bool = BOOL
        let p_app = P_APP
        let t_true = T_TRUE
        let t_false = T_FALSE
        let t_t_neq_f = T_T_NEQ_F
        let pred_eq_t = PRED_EQ_T
        let pred_eq_f = PRED_EQ_F
        let f_to_b = F_TO_B
        let true_preds_equal = TRUE_PREDS_EQUAL
        let false_preds_equal = FALSE_PREDS_EQUAL
        let pred_refl_pos = PRED_REFL_POS
        let pred_refl_neg = PRED_REFL_NEG
        let pred_not_iff_f = PRED_NOT_IFF_F
        let pred_not_iff_f_2 = PRED_NOT_IFF_F_2
        let pred_not_iff_t = PRED_NOT_IFF_T
        let pred_not_iff_t_2 = PRED_NOT_IFF_T_2
        let pred_iff_f = PRED_IFF_F
        let pred_iff_f_2 = PRED_IFF_F_2
        let pred_iff_t = PRED_IFF_T
        let pred_iff_t_2 = PRED_IFF_T_2
        let decl_atom = DECL_ATOM
        let decl_bvatom = DECL_BVATOM
        let clausify_form = CLAUSIFY_FORM
        let clausify_form_not = CLAUSIFY_FORM_NOT
        let clausify_false = CLAUSIFY_FALSE
        let th_let_pf = TH_LET_PF
        let iff_symm = IFF_SYMM
        let contrad = CONTRAD
        let truth = TRUTH
        let not_not_intro = NOT_NOT_INTRO
        let not_not_elim = NOT_NOT_ELIM
        let or_elim_1 = OR_ELIM_1
        let or_elim_2 = OR_ELIM_2
        let not_or_elim = NOT_OR_ELIM
        let and_elim_1 = AND_ELIM_1
        let and_elim_2 = AND_ELIM_2
        let not_and_elim = NOT_AND_ELIM
        let impl_intro = IMPL_INTRO
        let impl_elim = IMPL_ELIM
        let not_impl_elim = NOT_IMPL_ELIM
        let iff_elim_1 = IFF_ELIM_1
        let iff_elim_2 = IFF_ELIM_2
        let not_iff_elim = NOT_IFF_ELIM
        let xor_elim_1 = XOR_ELIM_1
        let xor_elim_2 = XOR_ELIM_2
        let not_xor_elim = NOT_XOR_ELIM
        let ite_elim_1 = ITE_ELIM_1
        let ite_elim_2 = ITE_ELIM_2
        let ite_elim_3 = ITE_ELIM_3
        let not_ite_elim_1 = NOT_ITE_ELIM_1
        let not_ite_elim_2 = NOT_ITE_ELIM_2
        let not_ite_elim_3 = NOT_ITE_ELIM_3
        let ast = AST
        let asf = ASF
        let bv_ast = BV_AST
        let bv_asf = BV_ASF
        let arrow = ARROW
        let apply = APPLY
        let trust = TRUST
        let trust_f = TRUST_F
        let refl = REFL
        let symm = SYMM
        let trans = TRANS
        let negsymm = NEGSYMM
        let negtrans1 = NEGTRANS1
        let negtrans2 = NEGTRANS2
        let cong = CONG
        let arr = ARRAY
        let sort = SORT
        let read = READ
        let write = WRITE
        let row1 = ROW1
        let row = ROW
        let negativerow = NEGATIVEROW
        let ext = EXT
        let varbv = VARBV
        let bitvec = BITVEC
        let avarbv = AVARBV
        let trustbad = TRUSTBAD
        let abv = ABV
        let bvc = BVC
        let bvn = BVN
        let b0 = B0
        let b1 = B1
        let bvdiseq = BVDISEQ
        let bvand = BVAND
        let bvor = BVOR
        let bvxor = BVXOR
        let bvnand = BVNAND
        let bvnor = BVNOR
        let bvxnor = BVXNOR
        let bvmul = BVMUL
        let bvadd = BVADD
        let bvsub = BVSUB
        let bvudiv = BVUDIV
        let bvurem = BVUREM
        let bvsdiv = BVSDIV
        let bvsrem = BVSREM
        let bvsmod = BVSMOD
        let bvshl = BVSHL
        let bvlshr = BVLSHR
        let bvashr = BVASHR
        let bvconcat = BVCONCAT
        let bvneg = BVNEG
        let bvnot = BVNOT
        let bvrleft = BVLROTATE
        let bvrright = BVRROTATE
        let bvcomp = BVCOMP
        let bvextract = BVEXTRACT
        let bvzeroext = BVZEROEXT
        let bvsignext = BVSIGNEXT
        let bvrepeat = BVREPEAT
        let bvult = BVULT
        let bvule = BVULE
        let bvugt = BVUGT
        let bvuge = BVUGE
        let bvslt = BVSLT
        let bvsle = BVSLE
        let bvsgt = BVSGT
        let bvsge = BVSGE
        let bbltn = BBLTN
        let bbltc = BBLTC
        let trust_bblast_term = TRUSTBBLASTTERM
        let decl_bblast = DECLBBLAST
        let decl_bblast_with_alias = DECLBBLASTWITHALIAS
        let bitof = BITOF
        let bv_bbl_const = BVBBLCONST
        let bv_bbl_var = BVBBLVAR
        let intro_assump_t = INTROASSUMPT
        let intro_assump_f = INTROASSUMPF
        let bv_bbl_eq = BVBBLEQ
        let bv_bbl_neq = BVBBLNEQ
        let bv_bbl_eq_swap = BVBBLEQSWAP
        let bv_bbl_concat = BVBBLCONCAT
        let bv_bbl_extract = BVBBLEXTRACT
        let bv_bbl_zero_extend = BVBBLZEROEXT
        let bv_bbl_sign_extend = BVBBLSIGNEXT
        let bv_bbl_bvand = BVBBLBVAND 
        let bv_bbl_bvnot = BVBBLBVNOT
        let bv_bbl_bvor = BVBBLBVOR 
        let bv_bbl_bvxor = BVBBLBVXOR
        let bv_bbl_bvadd = BVBBLBVADD
        let bv_bbl_bvneg = BVBBLBVNEG
        let bv_bbl_bvmul = BVBBLBVMUL
        let bv_bbl_bvult = BVBBLBVULT
        let bv_bbl_bvslt = BVBBLBVSLT
        let bv_bbl_bvcomp = BVBBLBVCOMP
        let rr_bv_eq = RR_BV_EQ
        let rr_bv_default = RR_BV_DEFAULT
        let rres = RRES
        let qres = QRES
        let block_comment _pos ~main buf lexbuf =
          main buf lexbuf
        let comment _text ~main buf lexbuf =
          main buf lexbuf (* skip and continue lexing *)
      end
    end)


  let main = Vanilla.main

}