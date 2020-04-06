;compile stub-simple-example.scm
(use gauche.parseopt)
(use gauche.parameter)
(use gauche.cgen)
(use gauche.cgen.stub)
(use file.util)
(use util.match)

(cgen-genstub "stub-simple-example.scm")

