; An example of an unsat boolean formula with a resolution proof.
; To get the proof, run:
; cvc4 -simplification=none --dump-proof bool1.smt2
(set-logic QF_LIA)
(declare-fun a () Bool)
(declare-fun b () Bool)
(declare-fun c () Bool)
(assert (not b))
(assert (or b (not c)))
(assert c)
(check-sat)