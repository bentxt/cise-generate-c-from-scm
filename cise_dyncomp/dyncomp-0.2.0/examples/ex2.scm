(use dyncomp)

(cinclude "stdio.h")

(define-cfn show_message ((str :: |const char*|)) :: void
  :static
  (printf "showmsg: %s\n" str))

(define-cproc show-msg (str::<const-cstring>)
  (show_message str)
  (return SCM_UNDEFINED))

(show-msg "Hello, world")
                    
