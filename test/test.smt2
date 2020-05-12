(set-logic ALL_SUPPORTED)

(declare-fun a () (_ BitVec 4))
(declare-fun b () (_ BitVec 4))


(assert (= a 
           (let ((x #b0000) (y #b1111)) (bvand x y))))
(assert (= b #b1111))
(assert (= a b))
(check-sat)