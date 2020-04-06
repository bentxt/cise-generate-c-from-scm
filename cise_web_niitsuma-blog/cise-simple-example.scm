;save as cise-simple-example.scm

(define-cise-stmt (mywhen test . body)
  `(if ,test (begin ,@body)))

(define-cfn test1 (aa::int  ) ::void
  (mywhen (> aa  50 )  (test2)) 
)

(define-cfn test2 (argc::int argv::char**) ::int
  (dotimes [i (strlen (aref argv 0))] (printf "%02x" (aref (aref argv 0) i)))
)

;(define-cfn test3 (aa::int  ) ::void
;            (let* ([f 88::int]) (printf "%i" f)) )

(define-cfn main (argc::int argv::char**) ::int
  (dotimes [i (strlen (aref argv 0))] (printf "%02x" (aref (aref argv 0) i)))
)
