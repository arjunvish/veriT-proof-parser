(set-logic ALL_SUPPORTED)

(define-fun g 
    ((x (_ BitVec 2)) (y (_ BitVec 2))) 
    Bool
    (let 
        ((e (bvadd x y))) 
         (bvule x e)))
(define-fun f 
    ((a (_ BitVec 2)) (b (_ BitVec 2))) 
    Bool 
    (let 
        ((e (bvsub a b))) 
        (g a b)))

(assert (f #b11 #b01))
(check-sat)
(exit)
