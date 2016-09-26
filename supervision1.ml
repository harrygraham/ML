(* --- 1 ---*)
fun ins x _ [] = [x] 
	| ins x f (y::ys) = if f (x, y) then x::y::ys else y::(ins x f ys);

fun sort f []  = []
	| sort f (x::xs)  = ins  x f (sort f xs ) ;

(* --- 2 ---*)
fun lex f (x::xs, y::ys) = if x = y then lex f (xs, ys) else f (x, y);

(* --- 3 ---*)
fun lexsort f xss = sort (lex f) xss;

(* --- 4 ---*)
datatype 'a tree = empty | node of 'a * 'a tree list
  
type 'a forest = 'a tree list

(*-from tick2 star-*)
fun allcons(n, []) = [] |
allcons(n, xs::xss) = (n::xs)::allcons(n, xss);


fun getmaxpaths(node(a, [])) = [[a]] 
	| getmaxpaths (node(a,x::xs)) =  allcons(a , getmaxpaths(x)) @ (if xs = [] then [] else getmaxpaths(node(a, xs)));

fun traces ([]) = []
	| traces (hd::tl) = getmaxpaths(hd) @ traces(tl);

val testForest = [node(1, [node(3, []), node(4, [])]), node(2, [node(3, [node(1, []), node(3, [])]), node(5, [])])];

traces(testForest);