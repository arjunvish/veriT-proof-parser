(set-logic QF_BV)
(declare-fun x () (_ BitVec 8))
(declare-fun y () (_ BitVec 8))

(assert (_ bv18446744073709551615 64))
(check-sat)