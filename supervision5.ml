type 'a mutstack = 'a list ref;

val newstack = fn () => ref [];

fun push x s = s := x :: !s ; 

exception empty;
fun pop s =
case !s of
	nil => raise empty
	| someList => (s:= tl(someList); hd(someList));

pop(ref [1,2,3,4]);

fun mutrev xs = let val localstack = newstack()
				in map(fn () => pop localstack ) (map (fn x => push x localstack) xs)

					(*map (fn x => push x localstack) o (fn () => pop localstack )*)
				end; 


o;
(*
mutrev [1,2,3,4,5,6,7,8] ;

fun replicate 0 x = []
	| replicate n x = x::replicate (n-1) x;

val rlist = (replicate 4 (ref 0)) @ (map ref [1, 2, 3, 4]);
val slist = map (fn r => ref (!r)) rlist;





map (fn r => (r := !r + 1)) rlist;
map (fn r => (r := !r - 1; !r)) rlist;
map (fn r => (r := !r +3; !r)) slist;*)