;save as stub-simple-example.scm
(declcode (.include <stdio.h>)
 "#ifndef M_PI
#define M_PI 3.1415926535897932384
#endif"
)

(define-cise-stmt (mywhen test . body)
  `(if ,test (begin ,@body)))

(define-cfn test1 (aa::int  ) ::void
  (mywhen (> aa  50 )  (test2)) 
)

(define-cfn test2 (argc::int argv::char**) ::int
  (dotimes [i (strlen (aref argv 0))] (printf "%02x" (aref (aref argv 0) i)))
)

