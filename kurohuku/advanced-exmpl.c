
#line 1 "advanced-exmpl.sc"
 int foo(int argc,char** argv){{
#line 2 "advanced-exmpl.sc"
{int p=88;
printf("jsdf %i",p);}}}
#line 5 "advanced-exmpl.sc"
 int main(int argc,char** argv){{
#line 6 "advanced-exmpl.sc"
setlocale(LC_ALL,"");
if (isatty(STDOUT_FILENO)){
termwidth=(80);
{char* p=getenv("COLUMNS");struct winsize win;
#line 11 "advanced-exmpl.sc"
if ((p)&&((*(p))!=('\x00'))){termwidth=(atoi(p));}else if(
((ioctl(STDOUT_FILENO,TIOCGWINSZ,&(win)))!=(-1))&&(
((win).ws_col)>(0))){
termwidth=((win).ws_col);}
f_nonprint=(1);}}}}
