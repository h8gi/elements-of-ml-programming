signature ELEMENT = sig
    type element
    val similar : element * element -> bool
end

signature BTREE = sig
    eqtype elt
    structure Element: ELEMENT
    sharing type elt = Element.element
    datatype btree = Empty | Node of elt * btree * btree
    val leaf : elt -> btree
    val build : elt * btree * btree -> btree
    val lookup : elt * btree -> bool
end

signature TREE = sig
    eqtype elt
    structure Element: ELEMENT
    sharing type elt = Element.element
    datatype tree = Tree of elt * tree list
    val build : elt * tree list -> tree
    val lookup : elt * tree -> bool
end

signature ALLTREES = sig
    structure Btree : BTREE
    structure Tree : TREE
    sharing Btree.Element = Tree.Element (* similar は別であり得る *)
    sharing type Btree.elt = Tree.elt (* これはredundant *)
end


structure S1 : ELEMENT =
struct
type element = string
fun similar (x, y) = x = y
end

structure SharedBTree : BTREE =
struct
type elt = string
structure Element = S1
datatype btree = Empty | Node of elt * btree * btree
fun leaf x = Node (x, Empty, Empty)
fun build (x, t1, t2) = Node(x, t1, t2)
fun lookup (_, Empty) = false
  | lookup (x, Node(y, left, right)) =
    x=y orelse
    (lookup (x, left)) orelse (lookup (x, right))
end

structure S2 : ELEMENT =
struct
type element = string
fun similar (x, y) = x = y
end

structure SharedTree : TREE =
struct
  type elt = string
  structure Element = S2
  datatype tree = Tree of elt * tree list
  fun build (x, children) = Tree (x, children)
  fun lookup (x, Tree(y, children)) =
      Element.similar (x, y) orelse
      let
          fun lookup_children [] = false
            | lookup_children (child::rest_children) =
               lookup (x, child) orelse
	       lookup_children rest_children
      in
          lookup_children children
      end
end

structure AllTrees : ALLTREES =
struct
structure Btree = SharedBTree
structure Tree = SharedTree
end
