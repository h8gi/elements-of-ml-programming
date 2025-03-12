signature TOTALORDER = sig
    type element
    val lt : element * element -> bool
end

functor MakeBST(Lt: TOTALORDER):
	sig			(* this signature is optional *)
	    type 'label btree
	    exception EmptyTree
	    val create : Lt.element btree
	    val lookup : Lt.element * Lt.element btree -> bool
	    val insert : Lt.element * Lt.element btree ->
			 Lt.element btree
	    val deletemin : Lt.element btree ->
			    Lt.element * Lt.element btree
	    val delete : Lt.element * Lt.element btree ->
			 Lt.element btree
	end
=
struct
open Lt

datatype 'label btree =
	 Empty |
	 Node of 'label * 'label btree * 'label btree
val create = Empty
fun lookup (_, Empty) = false
  | lookup (x, Node(y, left, right)) =
    if lt(x, y) then lookup(x, left)
    else if lt(y, x) then lookup(x, right)
    else true
fun insert (x, Empty) = Node(x, Empty, Empty)
  | insert (x, tree as Node(y, left, right)) =
    if lt(x,y) then Node(y, insert(x, left), right)
    else if lt(y,x) then Node(y, left, insert(x, right))
    else tree
exception EmptyTree
fun deletemin Empty = raise EmptyTree
  | deletemin (Node(x, Empty, right)) = (x, right)
  | deletemin (Node(x, left, right)) =
    let
	val (min, subtree) = deletemin left
    in
	(min, Node(x, subtree, right))
    end
fun delete (_, Empty) = Empty
  | delete (x, Node(y, left, right)) =
    if lt(x,y) then Node(y, delete(x, left), right)
    else if lt(y,x) then Node(y, left, delete(x, right))
    else (*x=y*)
	case (left, right) of
	    (_, Empty) => left
	  | (Empty, _) => right
	  | (_, _) => let
	      val (newRoot, newRight) = deletemin right
	  in
	      Node(newRoot, left, newRight)
	  end
end
