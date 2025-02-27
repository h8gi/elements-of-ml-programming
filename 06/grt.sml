(* general rooted trees *)

datatype 'label tree = Node of 'label * 'label tree list;
val t =
    Node(1, [
	    Node(2, nil),
	    Node(3, [
		    Node(4, nil),
		    Node(5, [Node(7, nil)]),
		    Node(6, nil)
		])
	]);

val t2 =
    Node(1, [
	    Node(2, [Node(2, nil)]),
	    Node(3, [
		    Node(4, nil),
		    Node(5, [Node(2, [Node(10, nil)]),
			     Node(7, nil),
			     Node(2,nil)]),
		    Node(6, nil)
		]),
		Node(2, nil)
	]);

fun sum (Node(x, nil)) = x
  | sum (Node(x, t::ts)) = sum(t) + sum(Node(x, ts));

fun sumF (Node(a, L)) = foldr (op +) a (map sumF L);

(* 6.4.4 ex*)
(* ex 6.4.1 *)
fun lookup (x : ''a, Node(a, nil)) = x = a
  | lookup (x, Node(a, t::ts)) =
    lookup(x, t) orelse lookup(x, Node(a, ts));

fun lookupF (x : ''a) =
  let
    fun lookupHelper (Node(a, children)) =
      a = x orelse List.exists lookupHelper children
  in
    lookupHelper
  end;

(* ex 6.4.2 *)
fun count x =
    let
	fun helper (Node(a, nil)) = if x = a then 1 else 0
	  | helper (Node(a, t::ts)) =
	    helper t + helper(Node(a, ts))
    in
	helper
    end;

val test_count_t_2 = count 2 t;
val test_count_t_7 = count 7 t;
val test_count_t_8 = count 8 t;

val test_count_t2_2 = count 2 t2;
val test_count_t2_7 = count 7 t2;
val test_count_t2_8 = count 8 t2;

fun countF x =
    let
	fun helper (Node(a, children)) =
	    foldr (op +) (if a = x  then 1 else 0) (map helper children)
    in
	helper
    end;

(* ex 6.4.3 *)
fun depth (Node(a, nil)) = 1
  | depth (Node(a, t::ts)) =
    Int.max (1+depth t, depth (Node(a, ts)));

fun depthF (Node(a, children))
    = 1 + foldr Int.max 0 (map depthF children);

(* ex 6.4.4 preorder *)
fun preorder (Node(a, nil)) = [a]
  | preorder (Node(a, children)) =
    a :: foldl (fn (child, acc) => acc @ preorder child) [] children;
