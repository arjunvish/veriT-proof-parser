(set-logic ALL_SUPPORTED)
(declare-fun x () Bool)
(declare-fun y () Bool)
(declare-fun z () (_ BitVec 4))
(assert (= x (and true false true)))
(assert (= x (or false false true)))
(assert (= y (xor true false true true)))
(assert (= true false true))
(assert (= z (bvand #b0000 #b0101 #b1010)))
(assert (= z (bvor #b1111 #b0101 #b1010)))
(assert (= z (bvxor #b1111 #b0101 #b1010)))
(assert (= z (bvmul #b1111 #b0101 #b1010)))
(assert (= z (bvadd #b1111 #b0101 #b1010)))
;(assert (= z (bvsub #b1111 #b0101 #b1010)))
;(assert (= z (bvnand #b1111 #b0101 #b1010)))
;(assert (= z (bvnor #b1111 #b0101 #b1010)))
;(assert (= z (bvxnor #b1111 #b0101 #b1010)))
(check-sat)
(exit)
