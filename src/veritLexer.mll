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
  open VeritParser

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
      val colon : t
      val bang : t
      val col_rule : t
      val col_step : t
      val col_args : t
      val col_premises : t
      val assume : t
      val step : t
      val anchor : t
      val define_fun : t
      val cl : t
      val as_tok : t
      val choice : t
      val flet : t
      val forall : t
      val exists : t
      val pmatch : t
      val quoted_string : Lexing.position -> Quoted_string_buffer.t -> t
      val spec_constant : string -> t
      val keyword : string -> t
      val symbol : string -> t
      val isymbol : string -> t
      type s = Quoted_string_buffer.t -> Lexing.lexbuf -> t
      val comment : string -> main:s -> s
      val sat : t
      val eof : t
    end
  end

  (* Create and populate a hashtable *)
  let mk_hashtbl init =
    let tbl = List.length init |> Hashtbl.create in
    init |> List.iter (fun (k, v) -> Hashtbl.add tbl k v) ;
    tbl

  let keywords = mk_hashtbl [
  (* SMTLIB formulas *)
    ("true", TRUE);
    ("false", FALSE);
    ("not", NOT);
    ("=>", IMPLIES);
    ("and", AND);
    ("or", OR);
    ("xor", XOR);

  (*Basic proof rules*)
    ("assume", ASSUME);
    (*true*)
    (*false*)
    ("not_not", NOTNOT);

  (* Resolution rules and clause simplifications *)
    ("th_resolution", THRES);
    ("resolution", RES);
    ("tautology", TAUT);
    ("contraction", CONT);

  (* Equality and congruence reasoning *)
    ("refl", REFL);
    ("trans", TRANS);
    ("cong", CONG);
    ("eq_reflexive", EQREFL);
    ("eq_transitive", EQTRANS);
    ("eq_congruent", EQCONG);
    ("eq_congruent_pred", EQCONGPRED);

  (* Clausification of Boolean operators *)
    (*and*)
    ("not_or", NOTOR);
    (*or*)
    ("not_and", NOTAND);
    ("xor1", XOR1);
    ("xor2", XOR2);
    ("not_xor1", NXOR1);
    ("not_xor2", NXOR2);
    ("implies", IMP);
    ("not_implies1", NIMP1);
    ("not_implies2", NIMP2);
    ("equiv1", EQ1);
    ("equiv2", EQ2);
    ("not_equiv1", NEQ1);
    ("not_equiv2", NEQ2);
    ("and_pos", ANDP);
    ("and_neg", ANDN);
    ("or_pos", ORP);
    ("or_neg", ORN);
    ("xor_pos1", XORP1);
    ("xor_pos2", XORP2);
    ("xor_neg1", XORN1);
    ("xor_neg2", XORN2);
    ("implies_pos", IMPP);
    ("implies_neg1", IMPN1);
    ("implies_neg2", IMPN2);
    ("equiv_pos1", EQP1);
    ("equiv_pos2", EQP2);
    ("equiv_neg1", EQN1);
    ("equiv_neg2", EQN2);

  (* Clausification of ITE *)
    ("ite1", ITE1);
    ("ite2", ITE2);
    ("ite_pos1", ITEP1);
    ("ite_pos2", ITEP2);
    ("ite_neg1", ITEN1);
    ("ite_neg2", ITEN2);
    ("not_ite1", NITE1);
    ("not_ite2", NITE2);

  (* Simplifications on Boolean operators *)
    ("connective_def", CONNDEF);
    ("and_simplify", ANDSIMP);
    ("or_simplify", ORSIMP);
    ("not_simplify", NOTSIMP);
    ("implies_simplify", IMPSIMP);
    ("equiv_simplify", EQSIMP);
    ("bool_simplify", BOOLSIMP);
    ("ac_simp", ACSIMP);

  (* Simplifications on ITE operators *)
    ("ite_simplify", ITESIMP);

  (* Simplifications on equalities *)
    ("eq_simplify", EQUALSIMP);
    ]

  module Make (X : T) : sig
    val main : ?buf:Buffer.t -> Lexing.lexbuf -> X.Token.t
  end = struct (* BEGIN FUNCTOR BODY CONTAINING GENERATED CODE *)
    open X

}

let lf = '\010'
let lf_cr = ['\010' '\013']
let dos_newline = "\013\010"
let blank = [' ' '\009' '\012']
let wspace = ['\009' '\010' '\013' '\032']
let printable_char = ['\032'-'\126' '\128'-'\255']
let digit = ['0'-'9']
let non_zero_digit = ['1'-'9']
let hexdigit = digit | ['a'-'f' 'A'-'F']
let bindigit = ['0'-'1']
let letter = ['a'-'z' 'A'-'Z']
let spl = [ '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']

let simple_symbol = (letter | spl) (letter | digit | spl)*
let symbol = simple_symbol | '|' (wspace | printable_char # ['|' '\\'])* '|'
let numeral = '0' | non_zero_digit digit*
let decimal = numeral '.' '0'* numeral
let hexadecimal = '#' 'x' hexdigit+
let binary = '#' 'b' bindigit+
let qstring = '"' (wspace | printable_char)* '"'
let spec_constant = numeral | decimal | hexadecimal | binary | qstring
let index = numeral | symbol
let isymbol = '(' '_' symbol index+ ')'
let keyword = ':' simple_symbol

rule main buf = parse
  | lf | dos_newline { found_newline lexbuf 0;
                       main buf lexbuf }
  | blank+ { main buf lexbuf }
  | (';' (_ # lf_cr)*) as text { Token.comment text ~main buf lexbuf }
  | '(' { Token.lparen }
  | ')' { Token.rparen }
  | ':' { Token.colon }
  | '!' { Token.bang }
  | "assume" { Token.assume }
  | "step" { Token.step }
  | "anchor" { Token.anchor }
  | "define_fun" { Token.define_fun }
  | "cl" { Token.cl }
  | "as" { Token.as_tok }
  | "choice" { Token.choice }
  | "let" { Token.flet }
  | "forall" { Token.forall }
  | "exists" { Token.exists }
  | "match" { Token.pmatch }
  | ":rule" { Token.col_rule }
  | ":premises" { Token.col_premises}
  | ":args" { Token.col_args }
  | "Formula is Satisfiable" { Token.sat }
  | '"'
      { 
        let pos = Lexing.lexeme_start_p lexbuf in
        Quoted_string_buffer.add_lexeme buf lexbuf;
        scan_string buf pos lexbuf;
        let tok = Token.quoted_string pos buf in
        Quoted_string_buffer.clear buf;
        tok
      }
  | spec_constant as sc { Token.spec_constant sc }
  | keyword as kw { Token.keyword kw }
  | symbol as sym { Token.symbol sym }
  | isymbol as isym { Token.isymbol isym }
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
        open VeritParser
        type t = token
        type s = Quoted_string_buffer.t -> Lexing.lexbuf -> t
        let lparen = LPAREN
        let rparen = RPAREN
        let colon = COLON
        let bang = BANG
        let col_rule = COLRULE
        let col_step = COLSTEP
        let col_args = COLARGS
        let col_premises = COLPREMISES
        let sat = SAT
        let assume = ASSUME
        let step = STEP
        let anchor = ANCHOR
        let define_fun = DEFINEFUN
        let cl = CL
        let as_tok = AS
        let choice = CHOICE
        let flet = LET
        let forall = FORALL
        let exists = EXISTS
        let pmatch = MATCH
        let spec_constant c = SPECCONST c
        let keyword k = KEYWORD k
        let quoted_string _ buf = STRING (Buffer.contents buf)
        let symbol i =
          try Hashtbl.find keywords i with Not_found -> SYMBOL i
        let isymbol i = ISYMBOL i
        let comment _text ~main buf lexbuf =
          main buf lexbuf (* skip and continue lexing *)
        let eof = EOF
      end
    end)


  let main = Vanilla.main

}