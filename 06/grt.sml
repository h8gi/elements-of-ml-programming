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

fun sum (Node(x, nil)) = x
  | sum (Node(x, t::ts)) = sum(t) + sum(Node(x, ts));

fun sumF (Node(a, L)) = foldr (op +) a (map sumF L);

(* 6.4.4 ex*)
(* ex 6.4.1 *)
fun lookup (x : ''a, Node(a, nil)) = x = a
  | lookup (x, Node(a, t::ts)) =
    lookup(x, t) orelse lookup(x, Node(a, ts));

(* ex 6.4.2 *)
