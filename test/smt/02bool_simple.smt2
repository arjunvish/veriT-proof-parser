; A simple example of an unsat boolean formula with a resolution proof.
; To get the proof, run:
; cvc4 -simplification=none --dump-proof bool_simple.smt2

(set-logic ALL)
(declare-fun a () Bool)
(declare-fun b () Bool)
(declare-fun c () Bool)
(assert (not a))
(assert (or a b))
(assert (or (not b) c))
(assert (not c))
(check-sat)

