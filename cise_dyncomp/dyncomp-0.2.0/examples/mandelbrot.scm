;;
;; Calculate and show Mandelbrot set.
;;

(use gauche.uvector)
(use gl)
(use gl.glut)
(use math.const)
(use dyncomp)
(use util.match)
(use gauche.cgen.cise)
(use gauche.parseopt)

(define *tex* #f)

;(dyncomp-debug-mode #t)

(define (fill-image image size)
  (dotimes (y size)
    (dotimes (x size)
      (let ((i (* (+ (* y size) x) 3))
            (z (make-rectangular (- (* 3 (/ x size)) 2)
                                 (- (* 3 (/ y size)) 1.5))))
        (letrec ((rank (lambda (zn n)
                         (cond ((>= n 16) 0)
                               ((>= (magnitude zn) 2) n)
                               (else (rank (+ (* zn zn) z) (+ n 1)))))))
          (let ((r (rank z 0)))
            (u8vector-set! image i       (ash (logand r #xc) 4))
            (u8vector-set! image (+ i 1) (ash (logand r #x2) 6))
            (u8vector-set! image (+ i 2) (ash (logand r #x1) 7))
            ))))))

(cinclude "gauche/uvector.h")

(define-cise-macro (dotimes form env)
  (match form
    [(_ (var expr) . body)
     `(for ((set! ,var 0) (< ,var ,expr) (pre++ ,var))
           ,@body)]))

(define-cproc fill-image-fast (image size::<int>)
  (let* ((x :: int)
         (y :: int)
         (buf :: |unsigned char*| (SCM_U8VECTOR_ELEMENTS image)))
    (dotimes (y size)
      (dotimes (x size)
        (let* ((i :: int (* (+ (* y size) x) 3))
               (z_r :: double (- (* 3.0 (/ (cast double x) size)) 2.0))
               (z_i :: double (- (* 3.0 (/ (cast double y) size)) 1.5))
               (n :: int 0)
               (r :: int)
               (zn_r :: double z_r)
               (zn_i :: double z_i))
          (loop
           (cond
            ((>= n 16)
             (set! r 0)
             (break))
            ((>= (+ (* zn_r zn_r) (* zn_i zn_i)) 4)
             (set! r n)
             (break))
            (else
             (pre++ n)
             (let* ((zt_r :: double (+ (- (* zn_r zn_r) (* zn_i zn_i)) z_r))
                    (zt_i :: double (+ (* 2 (* zn_r zn_i)) z_i)))
               (set! zn_r zt_r)
               (set! zn_i zt_i)))))
          (set! (aref buf i      ) (<< (logand r #xc) 4))
          (set! (aref buf (+ i 1)) (<< (logand r #x2) 6))
          (set! (aref buf (+ i 2)) (<< (logand r #x1) 7))))))
  (return SCM_UNDEFINED))
         
(define (init image size)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)
  (set! *tex* (u32vector-ref (gl-gen-textures 1) 0))
  (gl-bind-texture GL_TEXTURE_2D *tex*)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (gl-tex-image-2d GL_TEXTURE_2D 0 GL_RGB size size 0
                   GL_RGB GL_UNSIGNED_BYTE image)
  )

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-enable GL_TEXTURE_2D)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_REPLACE)
  (gl-bind-texture GL_TEXTURE_2D *tex*)
  (gl-push-matrix)
  (gl-load-identity)
  (gl-translate 0.5 0.5 0.0)
  (gl-rotate 0 1.0 0.0 0.0)
  (gl-rotate 0 0.0 1.0 0.0)
  (gl-translate -0.5 -0.5 0.0)
  (gl-begin GL_QUADS)
  (gl-tex-coord '#f32(0.0 0.0)) (gl-vertex '#f32(0.0 0.0))
  (gl-tex-coord '#f32(0.0 1.0)) (gl-vertex '#f32(0.0 1.0))
  (gl-tex-coord '#f32(1.0 1.0)) (gl-vertex '#f32(1.0 1.0))
  (gl-tex-coord '#f32(1.0 0.0)) (gl-vertex '#f32(1.0 0.0))
  (gl-end)
  (gl-pop-matrix)
  (glut-swap-buffers)
  (gl-disable GL_TEXTURE_2D)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d 0 1 0 1)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  )

(define (keyboard key x y)
  (cond
   ((= key 27) (exit 0))))

(define (usage cmd)
  (for-each print
            (list
             #`"Usage: ,cmd [options]"
             "Options:"
             "  -s, --size    : specifies window size."
             "  -c, --compile : use compiled function to calculate mandelbrot set."
             "  -q, --quiet   : don't show display."))
  (exit 1))

(define (main args)
  (let-args (cdr args)
      ((size "s|size=i" 512)
       (compile? "c|compile" #f)
       (quiet? "q|quiet" #f)
       (help "h|help" => (cut usage (car args))))
    (let ((image (make-u8vector (* size size 3) 0)))
      (if compile?
          (time
           (fill-image-fast image size))
          (time
           (fill-image image size)))
      (unless quiet?
        (glut-init args)
        (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB))
        (glut-init-window-size size size)
        (glut-create-window "mandelbrot")
        (init image size)
        (glut-reshape-func reshape)
        (glut-keyboard-func keyboard)
        (glut-display-func disp)
        (glut-main-loop))))
  0
  )
