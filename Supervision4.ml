(* 1 *)

(* i *)

fun pushstack(x, stack) = x::stack;
	
	exception emptyStack;
fun popstack([]) = raise emptyStack
	| popstack(x::stack) = x;

datatype 'a queue = Q of 'a list * 'a list 

fun pushqueue(x, Q(ys, zs)) = Q(ys,x::zs);
	exception emptyQueue;
fun popqueue(Q([],[])) = raise emptyQueue
	| popqueue(Q([],zs)) = popqueue(Q(rev(zs),[]))
	| popqueue(Q(y::ys, zs)) = y;

exception foundElement;
exception noItems;
fun lookup1 p [] = raise noItems
	| lookup1 p (x::xs) = if p(x) then foundElement else lookup1 p (xs) 
	handle foundElement => x;

datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree

fun lookup2 p (Lf) = raise noItems
	| lookup2 p (Br(v, t1, t2)) = if p(v) then raise foundElement else lookup2 p (t1) 
		handle noItems => lookup2 p (t2)
			handle foundElement => v;

datatype 'a tree = empty | node of 'a * 'a tree list 

fun lookup3 p (empty) = raise noItems
	| lookup3 p (node(v, [])) = if p(v) then raise foundElement else raise noItems
	| lookup3 p (node(v, x::xs)) = if p(v) then raise foundElement else lookup3 p (x) 
		handle noItems => lookup3 p (node(v,xs))
			handle foundElement => v;

datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

fun lookup4 p (Nil) = raise noItems
	| lookup4 p (Cons(x, xf)) = if p(x) then raise foundElement else lookup4 p (xf()) handle foundElement => x; 
	
datatype 'a lbtree = Lb of 'a * (unit -> 'a ltree) * (unit -> 'a ltree);

fun lookup5 p ()





(*(2) Write a function  

  from : int -> int seq
  
that given an integer  n  returns the lazy list of integers consisting of
n  and all its successors in order; that is,  n,n+1,...,n+k,... *)

fun from k = Cons(k,fn() => from(k+1));

(*(3) Write a function 

  multiple : int -> int seq -> int seq

such that 

  multiple n s

is the sub-sequence of  s  obtained by retaining all the multiples of  n .*)

fun hd (Cons(x,xf)) = x;

fun tail (Cons(x,xf)) = xf();

fun multiple n (s) = if ((hd s) mod n = 0) then Cons((hd s),fn() => multiple n (tail(s)))
											else multiple n (tail(s));

(*(4) Repeat the above for a function 

  notmultiple : int -> int seq -> int seq

where now the integers that are not multiples are retained.*)

fun notmultiple n (s) = if ((hd s) mod n = 0) then notmultiple n (tail(s))
											else Cons((hd s),fn() => notmultiple n (tail(s)));

(*(5) Write a function

  primes : int seq -> int seq

such that 

  primes( from 2 )

is the lazy list of prime numbers.

The Sieve of Erathosthenes does this by recurrently applying the following
procedure: given 

  n1,n2,...,nk,...

it keeps n1 and eliminates all multiples of it form the rest.  Thus, when
this is applied to

  2,3,4,...,k,...

the procedure keeps 2 and eliminates all other even numbers, it then keeps
3 and eliminates all the multiples of 3 that had not been eliminated (that
is, the non-even multiples of 3), it then keeps 5 (as 4 has already been
eliminated) and eliminates all multiples of 5 that are not multiples of 2
and 3 (as these have already been eliminated), etc. *)

fun primes (Cons(x,xf)) = Cons(x,fn() => primes(notmultiple x (xf())));

primes(from 2);
