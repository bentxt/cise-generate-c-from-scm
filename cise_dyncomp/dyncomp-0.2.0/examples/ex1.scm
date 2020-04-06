(use dyncomp)

(define-cproc add (x::<int> y::<int>)
  (let* ((z :: int))
    (set! z (+ x y))
    (return (SCM_MAKE_INT z))))

(define-cproc sub (x::<double> y::<double>)
  (let* ((z :: double (- x y)))
    (return (Scm_MakeFlonum z))))

(define-cproc print-number (x::<double>)
  (printf "%f\n" x)
  (return SCM_UNDEFINED))

(print (add 1 2))
(print-number (sub 3 2))

(print (map (clambda (x::<int>)
              (return (SCM_MAKE_INT (+ x 1))))
            '(1 2 3)))
