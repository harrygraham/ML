datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

(*why do we need brackets for this to work? *)
fun tcons(v,Lf) = Br(v, Lf, Lf)
      | tcons (v,Br(w, t1, t2)) = Br (v, tcons(w, t2), t1);

fun arrayoflist([]) = Lf | arrayoflist(x::xs) = tcons(x, arrayoflist(xs));


fun tconsrev(Br(a, Lf, Lf)) = Lf
      | tconsrev (Br(w, Br(a, t3,t4), t2)) = Br (a, t2, tconsrev(Br(a, t3,t4)));

fun listofarray(Br(a, Lf, Lf)) = [a]
	| listofarray(Br(a, t1, t2)) = a::listofarray(tconsrev(Br(a, t1,t2)));

val testTree = Br(1, Br(2, Br(4, Br(8, Lf,Lf), Lf), Br(6, Lf,Lf)),Br(3, Br(5, Lf, Lf), Br(7, Lf ,Lf)));
(*
fun addlist([], []) = []
	|addlist(m::ms, []) = m::ms
	|addlist([], n::ns) = n::ns
	|addlist(m::ms, n::ns) = m::n::(addlist(ms, ns));
	
fun listofarray(Lf) = []
	|listofarray(Br(v, t1, t2)) = v::addlist(listofarray(t1), listofarray(t2)); *)
fun acc(Br(v, Lf, Lf), n) = if v mod 2 = 0 then [v] else []
	| acc(Br(v, t1, t2), n) = if v mod 2 = 0 then n::acc(tconsrev(Br(v, t1, t2)), n+1) else acc(tconsrev(Br(v, t1, t2)), n+1) ;
fun getSubsOfEvens (treeinput) = acc(treeinput, 1);

getSubsOfEvens testTree;

listofarray (arrayoflist [1.0,2.1,3.2,4.3,5.4,6.5,7.6]);

arrayoflist([1,2,3,4,5,6]);