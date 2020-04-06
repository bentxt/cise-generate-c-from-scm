(use gauche.parameter)
(use gauche.cgen)

(define *unit* (make <cgen-unit> :name "my-cfile"))

(parameterize ([cgen-current-unit *unit*])
  (cgen-decl "#include <stdio.h>")
  (cgen-init "printf(stderr, \"initialization function\\n\");")
  (cgen-body "void foo(int n) { printf(stderr, \"got %d\\n\", n); }")
  (cgen-extern "void foo(int n);")
  )


(cgen-emit-c *unit*)
(cgen-emit-h *unit*)
