(define-module foo
  (use dyncomp)

  (cinclude "stdio.h")
  
  (define-cfn show_msg () :: void
    :static
    (printf "module: foo\n"))

  (define-cproc show-msg ()
    (show_msg)
    (return SCM_UNDEFINED)))

(define-module bar
  (use dyncomp)

  (cinclude "stdio.h")
  
  (define-cfn show_msg () :: void
    :static
    (printf "module: bar\n"))

  (define-cproc show-msg ()
    (show_msg)
    (return SCM_UNDEFINED)))

(with-module foo (show-msg))
(with-module bar (show-msg))

