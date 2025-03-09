(* exercise 8.2.1 *)
structure Tree = struct
datatype 'label tree = Node of 'label * 'label tree list

exception Missing

fun create a = Node (a, nil)
fun build (a, L) = Node (a, L)
fun find (_, Node(_, nil)) = raise Missing
  | find (i, Node(x, t::ts)) =
    if i = 0 then t else find (i-1, Node(x, ts))
end

(* exercise 8.2.2 *)
signature SIMPLE = sig
    exception Missing
    val build : (int * int Tree.tree list) -> int Tree.tree
    val find : (int * int Tree.tree) -> int Tree.tree
end

structure SimpleTree: SIMPLE = Tree
