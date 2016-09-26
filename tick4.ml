fun nfold f 0 x = x
	| nfold f n x = f(nfold f (n-1) x);

fun sum a b = nfold (fn x => x + 1) (b) a;
(*could do it like this, nfold excepts two arguments*)
val sum2 = nfold(fn x => x + 1);
sum2 4 5;

fun product m n = nfold (fn x => x + m) (n-1) m;  

fun power g h = nfold (fn x => x * g) (h-1) g; 

sum 4 5;
product 4 5;
power 4 5;

datatype 'a stream = Cons of 'a * (unit -> 'a stream);

fun from k = Cons(k, fn()=> from(k+1));

fun head (Cons(x, _)) = x;
fun tail ((Cons(_, xf))) = xf();

fun nth(s,1) = head(s)
	| nth(s, n) = nth(tail(s), n-1);

nth(from 1, 100);

fun calcnextsquare x = Cons((x+1)*(x+1), fn()=> calcnextsquare(x+1)); 
val squares = Cons(1, fn()=> calcnextsquare(1));

nth(squares, 49);


(*fun mapq f Nil  = Nil*)
  (*| mapq f (Cons(x,xf)) = Cons(f x, fn()=> mapq f (xf()));*)

fun map2 f (Cons(x, xf)) (Cons(y, yf)) = Cons(f x y, fn()=> map2 f (xf()) (yf()));