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

open Format

let main () =
  let chan1 =
    try
      let filename1 = Sys.argv.(1) in
      open_in filename1
    with Invalid_argument _ -> stdin
  in
  let chan2 =
    try
      let filename2 = Sys.argv.(2) in
      open_in filename2
    with Invalid_argument _ -> stdin
  in
  let buf1 = Lexing.from_channel chan1 in
  let buf2 = Lexing.from_channel chan2 in
  let smtTree = (SmtlibParser.file SmtlibLexer.main buf1) in
  let lfscTree = (LfscParser.command LfscLexer.main buf2) in
  if (SmtlibAst.eq lfscTree smtTree) then
    Format.printf "Trees match!\n"
  else
    Format.printf "From SMT:\n%s\n\nFrom LFSC:\n%s\n" 
      (SmtlibAst.to_string smtTree)
      (SmtlibAst.to_string lfscTree)

let _ = main ()




(* 
   Local Variables:
   compile-command: "make"
   indent-tabs-mode: nil
   End: 
*)
