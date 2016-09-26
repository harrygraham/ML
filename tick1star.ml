fun facr (n) = 
if n=0 then 1
else if n < 0 then 0
else n * facr(n-1);


fun sumt(x, y, n, z) = if y <= n then x/z + sumt(x,y+1,n, z*(real(y+1))) else 0.0;
fun sumt2(x ,n) :real = if n > 0 then x + sumt(x, 1, n-1, 1.0) else 0.0;
fun eapprox(n) =  sumt2(1.0,n);

fun npower(x, n) : real = 
if n=0
then 1.0
else x * npower(x, n-1);


(*test the following with non factorial method*)

fun sumt3(z ,n, x) :real = if x < n then npower(z,x)/real(facr(x)) + sumt3(z,n, x+1) else 0.0;
fun exp(z,n) : real = sumt3(z,n,0);



fun gcd(a,b) =
if a < b then gcd(b,a)
else
if a=b then a
else if a mod 2 = 0 andalso b mod 2 = 0 then 2 * gcd(a div 2,b div 2)
else if a mod 2 = 1 andalso b mod 2 = 0 then gcd(a, b div 2)
else if a mod 2 = 0 andalso b mod 2 = 1 then gcd(a div 2, b)
else if a mod 2 = 1 andalso b mod 2 = 1 then gcd(b, (((a-1) div 2) - ((b-1) div 2)))
else if a = 0 andalso b = 0 then 0
else if a = 0 andalso b <> 0 then b
else if b = 0 andalso a <> 0 then a
else 0;





fun gcd2(a, b) =
if b=0 then a 
else 
gcd (b, a mod b);