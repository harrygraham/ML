datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun insert (s :string, Lf) = Br(s, Lf, Lf)
	| insert(s, Br(t, t1,t2)) = if s < t then Br(t, insert(s, t1), t2) else if s=t then Br(t, t1, t2) else Br(t, t1, insert(s, t2));

insert("Harry", Br("Jun Siang", Br("Richard", Lf, Lf), Br("Henry", Lf , Lf)));

fun member(s :string,  Lf ) = false
   | member(s,  Br(t, t1,t2) ) = if s = t then true else 
						if s < t then member(s, t1) else member(s, t2);

val testTree = Br
    ("Jun Siang", Br ("Richard", Br ("Harry", Lf, Lf), Lf),
     Br ("Henry", Lf, Lf));


fun inorder (Lf) = []
	| inorder(Br(v, t1, t2)) = inorder(t1) @ [v] @ inorder(t2);

fun merge([],[]) = []
	| merge(x::xs, []) = x::xs
	| merge([], y::ys) = y::ys
	| merge(x::xs, y::ys) = if x<y then x::y::merge(xs,ys) else y::x::merge(xs,ys);

fun union(tree1, tree2) = let fun maketree(newTree, []) = newTree
									| maketree(newTree, aList) = maketree(insert(hd(aList), newTree), tl(aList))
							in maketree(Lf, inorder(tree1) @ inorder(tree2)) end;

fun inter(tree1, tree2) = let fun maketree(newTree, []) = newTree
									| maketree(newTree, aList) = if member(hd(aList), tree2) then maketree(insert(hd(aList), newTree), tl(aList)) else maketree(newTree, tl(aList))
							in maketree(Lf, inorder(tree1)) end;

fun removefromlist(s, x::xs) =  if s = x then xs else x::removefromlist(s, xs);

fun remove(s, Lf) = Lf
	| remove(s, tree1) = if member(s, tree1) then let fun createtree(newTree, []) = newTree
							| createtree(newTree, aList) = createtree(insert(hd(aList),newTree), tl(aList))
							in createtree(Lf, removefromlist(s, inorder(tree1))) end
						else tree1;



