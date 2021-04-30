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
  let chan =
    try
      let filename = Sys.argv.(1) in
      open_in filename
    with Invalid_argument _ -> stdin
  in
  let buf = Lexing.from_channel chan in
  let veritTree = (VeritParser.command VeritLexer.main buf) in
    Format.printf "veriT proof:\n%s\n" 
      veritTree

let _ = main ()




(* 
   Local Variables:
   compile-command: "make"
   indent-tabs-mode: nil
   End: 
*)
