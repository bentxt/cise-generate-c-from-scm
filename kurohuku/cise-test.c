
#line 2 "cise-test.cise"
#include <stdio.h>

#line 4 "cise-test.cise"
 int main(int argc,char** argv){{
#line 5 "cise-test.cise"
{int i=0;int cise__778=30;for (; (i)<(cise__778); (i)++){
switch (((i)+(1))%(15)) {
case 0 : {printf("FizzBuzz\n");
#line 685 "/usr/local/Cellar/gauche/0.9.6/share/gauche-0.9/0.9.6/lib/gauche/cgen/cise.scm"
break;}
#line 8 "cise-test.cise"
case 3 : case 6 : case 9 : case 12 : {printf("Fizz\n");
#line 685 "/usr/local/Cellar/gauche/0.9.6/share/gauche-0.9/0.9.6/lib/gauche/cgen/cise.scm"
break;}
#line 9 "cise-test.cise"
case 5 : case 10 : {printf("Buzz\n");
#line 685 "/usr/local/Cellar/gauche/0.9.6/share/gauche-0.9/0.9.6/lib/gauche/cgen/cise.scm"
break;}default: {
#line 10 "cise-test.cise"
printf("%d\n",(i)+(1));
#line 685 "/usr/local/Cellar/gauche/0.9.6/share/gauche-0.9/0.9.6/lib/gauche/cgen/cise.scm"
break;}}}}
#line 11 "cise-test.cise"
return (0);}}
