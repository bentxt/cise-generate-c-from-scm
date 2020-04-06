(define-cfn foo (argc::int argv::char**) ::int
         (let* ([p::int 88 ])
            (printf "jsdf %i" p)))

(define-cfn main (argc::int argv::char**) ::int
  (setlocale LC_ALL "")
  (cond [(isatty STDOUT_FILENO)
         (= termwidth 80)
         (let* ([p::char* (getenv "COLUMNS")]
                [win::(struct winsize)])
           (cond [(and p (!= (* p) #\null)) (= termwidth (atoi p))]
                 [(and (!= (ioctl STDOUT_FILENO TIOCGWINSZ (& win)) -1)
                       (> (ref win ws_col) 0))
                  (= termwidth (ref win ws_col))])
           (= f_nonprint 1))]))
