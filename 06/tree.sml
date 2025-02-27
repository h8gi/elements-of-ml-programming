datatype 'label btree
  = Empty
  | Node of 'label * 'label btree * 'label btree;


datatype 'label evenTree
  = Empty
  | Enode of 'label * 'label oddTree * 'label oddTree
and 'label oddTree
    = Onode of 'label * 'label evenTree * 'label evenTree;

val t1 = Onode(1, Empty, Empty);
val t2 = Onode(2, Empty, Empty);
val t3 = Enode(3, t1, t2);

val t4 = Onode(4, t3, Empty);
val t5 = Enode(5, t4, t4);

(* ex 6.2.2 *)
datatype ('key, 'value) maptree
  = MEmpty
  | MNode of ('key * 'value) * ('key, 'value) maptree * ('key, 'value) maptree;

val p = MNode(("a", 1), MEmpty, MEmpty);

(* ex 6.2.5 *)
datatype suit = Heart | Spade | Diamond | Club;

datatype ulist = IntList of int list | RealList of real list;

datatype things =
	 Integer of int |
	 ThingsList of things list;
ThingsList [Integer 3, ThingsList [Integer 4, Integer 10]];

type 'a samePair = 'a * 'a;
val p = (1, 2) : int samePair;

(* ex 6.2.6 *)
(* 各pathの長さが mod 3 = 0, mod 3 = 1, mod 3 = 2 になるようなtree *)
datatype 'label zeroTree =
	 Empty
	 | ZeroNode of 'label * 'label twoTree * 'label twoTree
     and 'label oneTree = OneNode of 'label * 'label zeroTree * 'label zeroTree
     and 'label twoTree = TwoNode of 'label * 'label oneTree * 'label oneTree;

val t1 = ZeroNode(1, TwoNode(2, OneNode(3, Empty, Empty), OneNode(4, Empty, Empty)), TwoNode(5, OneNode(6, Empty, Empty), OneNode(7, Empty, Empty)));

(* ex 6.2.7 *)
(* a. write the type def *)
type 'node graph = ('node * 'node list) list;

exception NotANode;
fun succ (_, nil) = raise NotANode
  | succ (a, (x, y)::ys) = if a = x then y else succ(a, ys);

(* c. *)

fun member (a, nil) = false
  | member (a, x::xs) = a = x orelse member (a, xs);

(*
   search1 関数: 深さ優先探索を行う. chat GPT による添削
   ─────────────────────────────────────────────
   【問題点】
   - 引数 L（探索対象リスト）と R（訪問済みノードのリスト）の状態管理が
     各再帰呼び出し間で適切に共有されず、兄弟ノード間で最新の visited 状態が
     反映されない可能性がある。結果、同じノードを重複して探索するリスクがある。

   - 各探索結果のリストを foldr (op @) で結合しているが、
     リストの結合 (@) は結合するリストのサイズに依存してコストがかかるため、
     パフォーマンス面でのボトルネックになり得る。
*)
fun search1 (_, R, nil) = R
  | search1 (nil, R, _) = R
  | search1 (L : ''a list, R, G : ''a graph) =
    let
	val searchHelper = fn x => if member(x, R) then R
				   else search1(succ(x, G), x::R, G)
	val succListList = map searchHelper L
    in
	foldr (op @) nil succListList
    end;

fun search (a, G) = search1 ([a], [], G);

(* 改善後のコード。toVisitの更新がすっきりしている。 *)
fun dfs ([], visited, _) = visited
  | dfs (node::toVisit, visited, graph) =
    if member(node, visited) then
        dfs(toVisit, visited, graph)
    else
        let
            val neighbors = succ(node, graph)
        in
            dfs(neighbors @ toVisit, node::visited, graph)
        end;

fun search (start, graph) = dfs ([start], [], graph);

val l = [1];
val r = [];
val g = [
    (1, [2, 3]),
    (2, [3, 1]),
    (3, [4]),
    (4, [3])
];

(* ex 6.2.8. propositional logic *)

datatype plogic = And of plogic * plogic
		| Or of plogic * plogic
		| Not of plogic
		| Var of string;

fun eval (v as Var x, env) = member (v, env)
  | eval (And (e1, e2), env) = eval (e1, env) andalso eval (e2, env)
  | eval (Or (e1, e2), env) = eval (e1, env) orelse eval (e2, env)
  | eval (Not e, env) = not (eval (e, env));

(* (p ∨ q) ∧ ¬ p *)
val expr1 = And (Or (Var "p",Var "q"),Not (Var "p")) ;

eval (expr1, []);
eval (expr1, [Var "q"]);
eval (expr1, [Var "p"]);
eval (expr1, [Var "p", Var "q"]);
