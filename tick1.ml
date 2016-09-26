fun rootplus (a, b, c) = ( ~b + Math.sqrt (b*b-4.0*a*c)) / (2.0*a);

fun evalquad (a,b,c,x) : real = ((a*(x*x)) + (b*x) + c);

fun facr (n) = 
if n=0 then 1
else if n < 0 then 0
else n * facr(n-1);

fun faciTotal(n, total) = 
if n = 0 then total
else faciTotal(n-1, n*total);

fun faci (n) = faciTotal(n,1);

fun sumt2(x ,n) :real = if n >0 then x + sumt2(x/2.0, n-1) else 0.0;
fun sumt(n) =  sumt2(1.0,n);