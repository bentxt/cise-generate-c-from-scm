;compile cise-simple-example.scm
(use gauche.cgen)
(use gauche.cgen.cise)

(call-with-output-file "cise-simple-example.c"
    (lambda (po)
      (cise-translate (open-input-file "cise-simple-example.scm") po)))
