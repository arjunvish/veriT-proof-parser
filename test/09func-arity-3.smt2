; EXPECT: unsat
(set-logic QF_UF)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status unsat)

(declare-fun f (Bool Bool Bool) Bool)

(declare-fun x () Bool)
(declare-fun y () Bool)
(declare-fun z () Bool)
(declare-fun a () Bool)
(declare-fun b () Bool)
(declare-fun c () Bool)

(assert (not (= (f a b c) (f x y z))))
(assert (= a x))
(assert (= b y))
(assert (= c z))

(check-sat)

(exit)
