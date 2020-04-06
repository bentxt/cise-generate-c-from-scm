(use gauche.config)
(use gauche.process)
(use file.util)

(define p print)

(define (eb str)
  (regexp-replace-all #/\\/ str "\\\\\\\\"))


(define gauche-include-dir
  (let ((m ((string->regexp #`"-I(.*)$")
            (process-output->string '("gauche-config" "-I")))))
    (m 1)))

(define gauche-lib-dir
  (let ((m ((string->regexp #`"-L(.*)$")
            (process-output->string '("gauche-config" "-L")))))
    (m 1)))

(define datadir
  (let ((m ((string->regexp
             #`"-I(.*)\\\\lib\\\\gauche\\\\,(gauche-version)\\\\include")
            (process-output->string '("gauche-config" "-I")))))
    (build-path (m 1) "share")))

(define libdir
  (let ((m ((string->regexp #`"-I(.*)\\\\gauche\\\\,(gauche-version)\\\\include")
            (process-output->string '("gauche-config" "-I")))))
    (m 1)))

(define pkgarchdir
  (regexp-replace #/\$\{libdir\}/ (gauche-config "--pkgarchdir") (eb libdir)))

(define pkglibdir
  (regexp-replace #/\$\{datadir\}/ (gauche-config "--pkglibdir") (eb datadir)))

(define tcc-include-dir
  (build-path pkglibdir "dyncomp" "include"))


;;
(p "setlocal")

(p "cd tcc-0.9.24\\win32")
(p "call build-tcc-for-dyncomp.cmd")
(p "cd ..\\..\\")

(p "set srcdir=.")
(p "set TCC_INCLUDE_DIR=" tcc-include-dir)
(p "set CFLAGS=-c -I. -I\"" gauche-include-dir "\" -o")
(p "set LDFLAGS=-L\"" gauche-lib-dir
   "\" -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc -shared -o")
(p "echo #define SYS_INCLUDE_PATHS {\"" (eb tcc-include-dir) "\", \""
   (eb gauche-include-dir) "\"} > %srcdir%\\sys_inc_paths.h")

(p "gauche-config --fixup-extension dyncomp")
(p "gosh genstub dyncomplib.stub")
(p "gcc -std=gnu99 %CFLAGS% dyncomp_head.o dyncomp_head.c")
(p "gcc -std=gnu99 %CFLAGS% dyncomp.o dyncomp.c")
(p "gcc -std=gnu99 %CFLAGS% dyncomplib.o dyncomplib.c")
(p "gcc -std=gnu99 %CFLAGS% dyncomp_tail.o dyncomp_tail.c")
(p "gcc -std=gnu99 %LDFLAGS% dyncomp.dll dyncomp_head.o dyncomp.o dyncomplib.o dyncomp_tail.o -lgauche -lm -L.\\tcc-0.9.24\\win32\\libtcc -ltcc -ltcc1 -lpsapi")

(p "del *.o")
(p "del dyncomp_head.c dyncomp_tail.c dyncomplib.c sys_inc_paths.h")
(p "copy /Y dyncomp.dll \"" pkgarchdir "\"")
(p "copy /Y dyncomp.scm \"" pkglibdir "\"")
(p "mkdir \"" tcc-include-dir "\"")
(p "copy /Y .\\dyncomp_helper_win32.h \""
   (build-path tcc-include-dir "dyncomp_helper.h") "\"")
(p "copy /Y .\\tcc-0.9.24\\win32\\include\\*.h \"" tcc-include-dir "\"")
(p "copy /Y .\\tcc-0.9.24\\win32\\include\\winapi\\*.h \"" tcc-include-dir "\"")
(p "mkdir \"" (build-path tcc-include-dir "sys") "\"")
(p "copy /Y .\\tcc-0.9.24\\win32\\include\\sys\\*.h \""
   (build-path tcc-include-dir "sys") "\"")
(p "mkdir \"" (build-path tcc-include-dir "gauche") "\"")
(p "copy /Y .\\gauche\\*.h \"" (build-path tcc-include-dir "gauche") "\"")

