
#line 1 "fizz.scm"
#include <stdio.h>

#line 3 "fizz.scm"
 int main(int argc,char** argv){{
#line 5 "fizz.scm"
{int i=0;int cise__887=n;for (; (i)<(cise__887); (i)++){
switch (((i)+(1))%(15)) {
case 0 : {printf("FizzBuzz\n");
#line 783 "/usr/local/Cellar/gauche/0.9.8/share/gauche-0.97/0.9.8/lib/gauche/cgen/cise.scm"
break;}
#line 8 "fizz.scm"
case 3 : case 6 : case 9 : case 12 : {printf("Fizz\n");
#line 783 "/usr/local/Cellar/gauche/0.9.8/share/gauche-0.97/0.9.8/lib/gauche/cgen/cise.scm"
break;}
#line 9 "fizz.scm"
case 5 : case 10 : {printf("Buzz\n");
#line 783 "/usr/local/Cellar/gauche/0.9.8/share/gauche-0.97/0.9.8/lib/gauche/cgen/cise.scm"
break;}default: {
#line 10 "fizz.scm"
printf("%d\n",(i)+(1));
#line 783 "/usr/local/Cellar/gauche/0.9.8/share/gauche-0.97/0.9.8/lib/gauche/cgen/cise.scm"
break;}}}}
#line 11 "fizz.scm"
return (0);}}
