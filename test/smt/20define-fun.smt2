(set-logic ALL_SUPPORTED)

(declare-fun a () (_ BitVec 4))
(declare-fun b () (_ BitVec 4))
(declare-fun c () (_ BitVec 4))
(define-fun f ((x (_ BitVec 4)) (y (_ BitVec 4))) 
    (_ BitVec 4)
    (bvand x y))

(assert (= a #b0000))
(assert (= b #b1111))
(assert (= (f a b) #b1111))

(check-sat)