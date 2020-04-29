; An example of an unsat boolean formula with a resolution proof.
; To get the proof, run:
; cvc4 -simplification=none --dump-proof bool1.smt2
(set-logic QF_LIA)
(declare-fun a () Bool)
(declare-fun b () Bool)
(assert (or a (and (not b) b)))
(assert (not a))
(check-sat)
