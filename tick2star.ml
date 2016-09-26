fun allcons(n, []) = [] |
allcons(n, xs::xss) = (n::xs)::allcons(n, xss);

fun concat(xs::xss, ys::yss) = xs::xss@ys::yss |
	concat(xs::xss, []) = xs::xss |
	concat([], ys::yss) = ys::yss |
	concat([], []) = [];

fun choose(0, x::xs) = []
| choose (k, []) = [] |
choose(k, x::xs) = if k <= length(x::xs) then
	if k = length(x::xs) then [x::xs] else 
		if k=1 then (x::[])::choose(1,xs) else
		concat(allcons(x,choose(k-1,xs)),choose(k,xs))
else 
	[];

