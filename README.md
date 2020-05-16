This tool externally checks whether the proof produced by CVC4 for an SMTLIB script is indeed proving the assertions in the SMTLIB script. It does this by generating the proved assertions from the (LFSC) proof file produced by CVC4 and comparing them to the assertions in the SMTLIB script.

Prerequisites:
- Linux or Mac OS X
- OCaml 4.04 or later
- Ocamlbuild
- GNU Make
- Menhir
- num (part of OCaml distribution until 4.06)
- CVC4 in system path
- LFSC checker in system path

Usage:
Call the bash script `p.sh` with the SMTLIB script as the argument. This does the following:
- Call the makefile to build the LFSC-SMT-checker.
- Call CVC4 on the SMTLIB script and check if its unsat.
- If it is unsat, produce a proof for it.
- Check the proof using the LFSC checker.
- Use the LFSC-SMT-checker to check that the proof corresponds to the SMTLIB script.
Alternately, if you have an SMTLIB script and an LFSC file, you can check that the proof corresponds to the SMTLIB script by first building the LFSC-SMT-checker and then calling the executable with the LFSC file as the first argument and the SMTLIB script as the second argument:
```
make
./main.native lfsc-file.plf smt-file.smt2
```

Limitations:
- It currently works for QF_AUFBV theory combination only.
- It doesn't support `check-sat-assuming` commands. SMTLIB scripts with this command need to be extnally converted to possibly multiple SMTLIB scripts with the arguments to the `check-sat-assuming` given as assertions and a `check-sat` command instead.
- It doesn't support `define-sort` commands.
- It doesn't support declaration of sorts that take non-zero arguments using `declare-sort`.
- It doesn't support the usage of `=` with more than 2 operands. For example, it will fail on `(assert (= true false true))`.
