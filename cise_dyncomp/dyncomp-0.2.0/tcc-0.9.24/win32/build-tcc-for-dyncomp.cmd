@rem ----------------------------------------------------
@rem batch file to build tcc using gcc and ar from mingw
@rem ----------------------------------------------------
:
@echo>..\config.h #define TCC_VERSION "0.9.24"
@echo>>..\config.h #define TCC_TARGET_PE 1
@echo>>..\config.h #define CONFIG_TCCDIR "."
:
gcc -Os -fno-strict-aliasing ../tcc.c -D LIBTCC -c -o libtcc.o
gcc -O2 -Wall -c -o libtcc1.o ../libtcc1.c
mkdir libtcc
ar rcs libtcc/libtcc.a libtcc.o
ar rcs libtcc/libtcc1.a libtcc1.o
del libtcc.o
del libtcc1.o
copy ..\libtcc.h libtcc
:
