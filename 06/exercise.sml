type ('d, 'r) mapping = ('d * 'r) list;

(* 6.1.4 *)
(* ex 6.1.1 *)
type 'a set_of_set = 'a list list;
type ('a, 'b) list_of_triples = ('a * 'a * 'b) list;

fun foo (x: ('a, 'b) list_of_triples) = x;

[(3.0, 3.4), (2.3, 4.5), (~3.0, 12.1)] : (real, real) mapping;
