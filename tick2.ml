fun last0(x::xs) = if (xs =[]) then x else last0(xs) |
last0([]) = [];

fun last(xs) = if (tl(xs) =[]) then hd(xs) else last(tl(xs));

(*doesnt need extra case for empty list? - ask *)
fun butlast(xs) = if tl(xs) = [] then [] else hd(xs) :: butlast(tl(xs));

fun butlast0(x::xs) = if xs = [] then [] else x :: butlast0(xs) |
butlast0([]) = [];

fun nth(xs,n) = let fun nthcounter(xs,n,c) = if n=c then hd(xs) else nthcounter(tl(xs),n, c+1)
				in nthcounter(xs, n, 0) end;