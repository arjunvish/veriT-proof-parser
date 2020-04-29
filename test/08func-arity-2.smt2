; EXPECT: unsat
(set-logic QF_UF)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status unsat)

(declare-fun f (Bool Bool) Bool)

(declare-fun x () Bool)
(declare-fun y () Bool)
(declare-fun a () Bool)
(declare-fun b () Bool)

(assert (not (= (f a b) (f x y))))
(assert (= a x))
(assert (= b y))

(check-sat)

(exit)
