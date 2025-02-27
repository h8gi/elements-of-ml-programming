(* 6.3 Case Study: Binary Tree *)
(* BST = Binary Search Tree *)
(* BST property: *)
(* left subtree は 小さいラベル。right subtree は大きいラベル *)

val lower = String.map Char.toLower;
fun strLT (x, y) = lower x < lower y;

datatype 'label btree =
	 Empty |
	 Node of 'label * 'label btree * 'label btree;

fun lookup _ Empty x = false
  | lookup lt (Node(y, left, right)) x =
    if lt(x, y) then lookup lt left x
    else if lt(y, x) then lookup lt right x
    else true;

val t = Node("ML",
	     Node("as",
		  Node("a", Empty, Empty),
		  Node("in", Empty, Empty)),
	     Node("types", Empty, Empty));

lookup strLT t "function";

fun insert _ Empty x = Node(x, Empty, Empty)
  | insert lt (tree as Node(y, left, right)) x =
    if lt(x, y) then Node(y, insert lt left x, right)
    else if lt(y, x) then Node(y, left, insert lt right x)
    else tree;

val t1 = insert strLT t "function";
lookup strLT t1 "function";

exception EmptyBtree;
fun deletemin lt Empty = raise EmptyBtree
  | deletemin lt (Node(x, Empty, right)) = (x, right)
  | deletemin lt (Node(x, left, right)) =
    let
	val (min, subtree) = deletemin lt left
    in
	(min, Node(x, subtree, right))
    end;

fun delete _ Empty _ = Empty
  | delete lt (Node(y, left, right)) x =
    if lt(x, y) then Node(y, delete lt left x, right)
    else if lt(y, x) then Node(y, left, delete lt right x)
    else (* x = y *)
	case (left, right) of
	    (_, Empty) => left
	  | (Empty, _) => right
	  | (_, _) => let
	      val (newRoot, newRight) = deletemin lt right
	  in
	      Node(newRoot, left, newRight)
	  end;

fun sum Empty = 0
  | sum (Node(x, left, right)) = x + sum left + sum right;
sum( Node(3, Node(10, Empty, Empty), Empty) );

(* 6.3.8 *)
(* ex 6.3.1 *)
fun preorder Empty = []
  | preorder (Node(x, left, right)) =
    x :: (preorder left @ preorder right);

fun postorder Empty = []
  | postorder (Node(x, left, right)) =
    postorder left @ postorder right @ [x];

fun inorder Empty = []
  | inorder (Node(x, left, right)) =
    inorder left @ [x] @ inorder right;

(* 6.3.2 *)
type ('key, 'value) mapTree = ('key * 'value) btree;

exception Missing;
fun mlookup lt Empty x = raise Missing
  | mlookup lt (Node((key, value), left, right)) x =
    if lt(x, key) then mlookup lt left x
    else if lt(key, x) then mlookup lt right x
    else value;

fun massign _ Empty a b = Node((a, b), Empty, Empty)
  | massign lt (Node(pair as (key, _), left, right)) a b =
    if lt(a, key) then Node(pair, massign lt left a b, right)
    else if lt(key, a) then Node(pair, left,  massign lt right a b)
    else Node((key, b), left, right);

val mt1 = massign strLT Empty "hello" 12;
val mt2 = massign strLT mt1 "foo" 20;
(* 6.3.5 *)
fun preOrder1 (Empty, acc) = acc
  | preOrder1 (Node(x, left, right), acc) =
    let
	val pre = preOrder1 (left, x::acc)
    in
	preOrder1 (right, pre)
    end;

fun preOrder tree = rev (preOrder1 (tree, []));

(* 差分リストの型は 'a list -> 'a list として扱う *)
fun preOrderDL Empty = (fn xs => xs)
  | preOrderDL (Node(x, left, right)) =
      let
          val l = preOrderDL left
          val r = preOrderDL right
      in
          (* 現在のノード x を先頭に付け、左右の結果関数を合成 *)
          fn xs => x :: (l (r xs))
      end;

(* 差分リストの関数に空リストを渡して通常のリストに変換 *)
fun preOrderDiff tree = preOrderDL tree [];
