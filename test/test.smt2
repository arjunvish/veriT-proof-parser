;(set-option :print-success false)
(set-logic ALL_SUPPORTED)
(declare-const p Bool)
(declare-fun f (Bool) Bool)
(assert (f (= p (not p))))
(check-sat) ; returns 'unsat'
(exit)
