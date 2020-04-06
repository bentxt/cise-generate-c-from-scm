#!/usr/bin/env gosh

(use gauche.config)

(define (main args)
  (format #t "#define SYS_INCLUDE_PATHS {~a}~%"
          (string-join (map (cut string-append "\"" <> "\"")
                            (append (list (ref args 1) "/usr/include")
                                    (map (cut regexp-replace #/^-I/ <> "")
                                         (string-split (gauche-config "-I") " "))))
                       ","))
  0)

