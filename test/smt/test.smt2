(set-logic ALL_SUPPORTED)

(declare-fun a () (_ BitVec 4))
(declare-fun b () (_ BitVec 4))

(assert (= a ((_ rotate_right 2) #b0000)))
(assert (= a #b1110))

(check-sat)

;unsat
;(check
 ;; Declarations
;(% a var_bv
;(% A2 (th_holds true)
;(% A1 (th_holds (= (BitVec 4) (a_var_bv 4 a) (a_bv 4 (bvc b1 (bvc b1 (bvc b1 (bvc b0  bvn)))))))
;(% A0 (th_holds (= (BitVec 4) (a_var_bv 4 a) (rotate_right 4  (a_bv 4 (bvc b0 (bvc b0 (bvc b0 (bvc b0  bvn))))))))
;(: (holds cln)