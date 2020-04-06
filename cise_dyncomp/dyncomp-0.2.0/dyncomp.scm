;; -*- coding: utf-8; mode: scheme -*-
;;
;; dyncomp.scm
;; 
;;  Copyright (c) 2008 KOGURO, Naoki (naoki@koguro.net)
;; 
;;  Permission is hereby granted, free of charge, to any person 
;;  obtaining a copy of this software and associated 
;;  documentation files (the "Software"), to deal in the 
;;  Software without restriction, including without limitation 
;;  the rights to use, copy, modify, merge, publish, distribute, 
;;  sublicense, and/or sell copies of the Software, and to 
;;  permit persons to whom the Software is furnished to do so, 
;;  subject to the following conditions:
;; 
;;  The above copyright notice and this permission notice shall 
;;  be included in all copies or substantial portions of the 
;;  Software.
;; 
;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY 
;;  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
;;  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
;;  PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS 
;;  OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
;;  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
;;  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;; 

(define-module dyncomp
  (use gauche.cgen.cise)
  (use gauche.cgen.unit)
  (use gauche.cgen.type)
  (use gauche.cgen.stub)
  (use util.list)
  (use util.match)
  (use srfi-11)
  (use srfi-13)
  (use text.tr)

  (export define-cproc
          define-cfn
          define-type
          clambda
          cinclude
          make-cfunc
          compile-c-code
          add-cinclude-path
          ccode
          load-library
          dyncomp-debug-mode
          dyncomp-reset!)
  )

(select-module dyncomp)

;; Loads extension
(dynamic-load "dyncomp")

;; import some functions from gauche.cgen.stub
(define-values
  (process-cproc-args
   <cproc>
   process-body
   emit-arg-decl
   emit-arg-unbox
   emit-keyword-args-unbox
   have-rest-arg?)

  (with-module gauche.cgen.stub
    (values process-cproc-args
            <cproc>
            process-body
            emit-arg-decl
            emit-arg-unbox
            emit-keyword-args-unbox
            have-rest-arg?)))

;; import some non-exported functions from gauche.cgen.cise
(define cise-render-identifier (with-module gauche.cgen.cise cise-render-identifier))
(define cise-render-type (with-module gauche.cgen.cise cise-render-type))

(define (make-cproc c-name scheme-name argspec . body)
  (receive (args keyargs nreqs nopts rest? other-keys?)
      (process-cproc-args scheme-name argspec)
    (let ((cproc (make <cproc>
                   :scheme-name scheme-name
                   :c-name c-name
                   :proc-name (x->string scheme-name)
                   :args args
                   :keyword-args keyargs
                   :num-reqargs nreqs
                   :num-optargs nopts
                   :have-rest-arg? rest?
                   :allow-other-keys? other-keys?)))
      (process-body cproc body)
      cproc)))

(define debug-mode #f)

(define (split-prologue&body lst)
  (match lst
    ((('prologue prologue ...) bodies ...)
     (values prologue bodies))
    (else
     (values '() lst))))

(define (prologue->ccode prologue)
  (with-output-to-string
    (lambda ()
      (for-each (lambda (form)
                  (match form
                    (('include name)
                     (print "#include "name))
                    (else
                     (error "Unknown form: ~s" form))))
                prologue))))

(define (make-src-code prologue cproc)
  (with-output-to-string
    (lambda ()
      (define p print)
      (define f (cut format #t <...>))
      (p (prologue->ccode prologue))
      (p "ScmObj "[ref cproc'c-name] "(ScmObj *SCM_FP, int SCM_ARGCNT, void *data_)")
      (p "{")
      ;; argument decl
      (for-each emit-arg-decl [ref cproc'args])
      (for-each emit-arg-decl [ref cproc'keyword-args])
      (when (or (> [ref cproc'num-optargs] 0)
                (not (null? [ref cproc'keyword-args])))
        (p "  ScmObj SCM_OPTARGS = SCM_ARGREF(SCM_ARGCNT-1);"))
      (p "  SCM_ENTER_SUBR(\""[ref cproc'scheme-name]"\");")
      ;; argument count check (for optargs)
      (when (and (> [ref cproc'num-optargs] 0)
                 (null? [ref cproc'keyword-args])
                 (not (have-rest-arg? cproc)))
        (p "  if (Scm_Length(SCM_OPTARGS) > "[ref cproc'num-optargs]")")
        (p "    Scm_Error(\"too many arguments: up to "(+ [ref cproc'num-reqargs]
                                                          [ref cproc'num-optargs])
           " is expected, %d given.\", Scm_Length(SCM_OPTARGS)+"[ref cproc'num-reqargs]");"))
      ;; argument assertions & unbox op.
      (for-each emit-arg-unbox [ref cproc'args])
      (unless (null? [ref cproc'keyword-args])
        (emit-keyword-args-unbox cproc))
      ;; body
      (p "  {")
      (for-each p (reverse [ref cproc'stmts]))
      (p "  }")
      ;; closing the function
      (p "}")
      (p))))  

(define-values (cfunc-cache-table cfunc-cache-clear!)
  (let ((table-key (gensym)))
    (values
     (lambda (mod)
       (or (global-variable-ref mod table-key #f)
           (begin
             (eval `(define ,table-key (make-hash-table 'equal?)) mod)
             (cfunc-cache-table mod))))
     (lambda (mod)
       (eval `(define ,table-key (make-hash-table 'equal?)) mod)))))
  
(define (make-cfunc mod name arg-specs prologue body)
  (define (cache proc)
    (let ((tbl (cfunc-cache-table mod))
          (key (list name arg-specs prologue body)))
      (or (hash-table-get tbl key #f)
          (let1 val (proc)
            (hash-table-put! tbl key val)
            val))))
  (cache
   (lambda ()
     (let* ((c-name (symbol->string (gensym "__anonymous_")))
            (cproc (make-cproc c-name name arg-specs
                               (list 'code (with-output-to-string
                                             (lambda ()
                                               (cise-render `(begin ,@body)))))))
            (src-code
             (with-output-to-string
               (lambda ()
                 (define p print)
                 (define f (cut format #t <...>))
                 (p (prologue->ccode prologue))
                 (p "ScmObj "[ref cproc'c-name] "(ScmObj *SCM_FP, int SCM_ARGCNT, void *data_)")
                 (p "{")
                 ;; argument decl
                 (for-each emit-arg-decl [ref cproc'args])
                 (for-each emit-arg-decl [ref cproc'keyword-args])
                 (when (or (> [ref cproc'num-optargs] 0)
                           (not (null? [ref cproc'keyword-args])))
                   (p "  ScmObj SCM_OPTARGS = SCM_ARGREF(SCM_ARGCNT-1);"))
                 (p "  SCM_ENTER_SUBR(\""[ref cproc'scheme-name]"\");")
                 ;; argument count check (for optargs)
                 (when (and (> [ref cproc'num-optargs] 0)
                            (null? [ref cproc'keyword-args])
                            (not (have-rest-arg? cproc)))
                   (p "  if (Scm_Length(SCM_OPTARGS) > "[ref cproc'num-optargs]")")
                   (p "    Scm_Error(\"too many arguments: up to "(+ [ref cproc'num-reqargs]
                                                                     [ref cproc'num-optargs])
                      " is expected, %d given.\", Scm_Length(SCM_OPTARGS)+"[ref cproc'num-reqargs]");"))
                 ;; argument assertions & unbox op.
                 (for-each emit-arg-unbox [ref cproc'args])
                 (unless (null? [ref cproc'keyword-args])
                   (emit-keyword-args-unbox cproc))
                 ;; body
                 (p "  {")
                 (for-each p (reverse [ref cproc'stmts]))
                 (p "  }")
                 ;; return #<undef> when the body doesn't have no return value.
                 (p "  SCM_RETURN(SCM_UNDEFINED);")
                 ;; closing the function
                 (p "}")
                 (p)))))
       (when debug-mode
         (print src-code))
       (compile-c-code mod src-code)
       (make-subr mod name (ref cproc 'num-reqargs) (ref cproc 'num-optargs) c-name)))))

(define (gen-cfn cls name arg-specs rettype body)
  (let ((src-code 
         (with-output-to-string
           (lambda ()
             (format #t "~a ~a ~a(~a) {~%"
                     (cise-render-identifier cls)
                     (cise-render-type rettype)
                     (cise-render-identifier name)
                     (string-join (map (lambda (pair)
                                         (format "~a ~a"
                                                 (cise-render-type (cdr pair))
                                                 (cise-render-identifier (car pair))))
                                       (map (match-lambda
                                             [(var ':: type) (cons var type)]
                                             [(var) (cons var 'ScmObj)])
                                            arg-specs))
                                  ","))
             (cise-render `(begin ,@body))
             (format #t "}")))))
    (when debug-mode
      (print src-code))
    src-code))

(define-macro (define-cproc name arg-specs . rest)
  (let-values (((prologue body) (split-prologue&body rest)))
    `(define ,name
       (make-cfunc (current-module) ',name ',arg-specs ',prologue ',body))))

(define-macro (define-cfn . form)
  (match form
    [(name (args ...) ':: ret-type ':static . body)
     `(ccode ,(gen-cfn "static" name args ret-type body))]
    [(name (args ...) ':: ret-type . body)
     `(ccode ,(gen-cfn "" name args ret-type body))]
    [(name (args ...) ':static . body)
     `(ccode ,(gen-cfn "static" name args 'ScmObj body))]
    [(name (args ...) . body)
     `(ccode ,(gen-cfn "" name args 'ScmObj body))]))

(define-macro (clambda arg-specs . rest)
  (let-values (((prologue body) (split-prologue&body rest)))
    `(make-cfunc (current-module) #f ',arg-specs ',prologue ',body)))

(define-macro (cinclude filename)
  `(ccode ,(format "#include \"~a\"" filename)))

(define-macro (ccode . args)
  (let loop ((rest args)
             (include-dirs '())
             (codes '()))
    (cond
     ((null? rest)
      `(begin
         ,@(map (cut list 'add-cinclude-path <>) include-dirs)
         (compile-c-code (current-module)
                         ,(apply string-append (reverse codes)))))
     ((string? (car rest))
      (loop (cdr rest) include-dirs (cons (car rest) codes)))
     ((eq? (car rest) :include-dir)
      (loop (cddr rest) (cons (cadr rest) include-dir) codes))
     (else
      (error "ccode doesn't recognize ~s" (car rest))))))

(define-macro (add-cinclude-path path)
  `((with-module dyncomp add-include-path) (current-module) ,path))

(define-macro (define-type . args)
  `((with-module gauche.cgen.type make-cgen-type) ,@args))

(define-macro (dyncomp-reset!)
  `(begin
     ((with-module dyncomp clear-code) (current-module))
     ((with-module dyncomp cfunc-cache-clear!) (current-module))
     (undefined)))

(define (dyncomp-debug-mode mode)
  (set! debug-mode mode))


;; Epilogue
(provide "dyncomp")

