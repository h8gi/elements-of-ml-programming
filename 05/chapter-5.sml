(* 5.2.6 Exercise for section 5.2 *)
(* ex 5.2.1 *)
exception OutOfRange
fun third (_::_::z::_) = z
  | third _ = raise OutOfRange;

(* 5.2.2 *)
exception Negative of int;
fun fact1 0 = 1
  | fact1 n = if n < 0 then raise Negative n
	      else n * fact1 (n-1);
fun fact n =
    fact1 n handle Negative neg =>
		   (print ("Negative number: " ^ Int.toString neg ^ "\n"); 0);

(* 5.2.3 *)
(* --- *)

fun myFavoriteException ("sally") = Div
  | myFavoriteException ("joe") = Match
  | myFavoriteException _ = raise Match;

(* generalization *)
fun identity x = x;
fun foony (x : 'a -> 'a) : ('a -> 'a) = x;

val pair = (identity, identity);

val pair2 = [identity, identity];

let
    val x = identity(identity);
in
    x(1)
end;

(* let *)
(*     val x = identity(identity) *)
(* in *)
(*     (x(1), x("a")) *)
(* end; *)

(identity(1), identity("a"));

(* 5.3.2 *)
fun hoge (x, y) = x > y;
fun huga (x, y) = x <> y;

fun rev1 L =
    if L = nil then nil
    else rev1(tl L) @ [hd L];

fun rev2 nil = nil
  | rev2 (x::xs) = rev2 xs @ [x];

(* 5.3.5 exercise *)
(* rev1 [(rev1: int list -> int list), rev1]; (* error, equality *) *)
(* rev2 [(rev1: int list -> int list), rev1]; *)
(* rev1 [rev1, rev1];		(* error, equality *) *)
(* rev2 [rev1, rev1];		(* non-generalizable *) *)
(* rev2 [rev2, rev2];		(* non-generaliable *) *)
(* rev1 [chr, chr];		(* error, equality *) *)
(* rev2 [chr, chr]; *)
(* rev1 [chr, ord];		(* list error *) *)
(* rev2 [chr, ord];		(* list error *) *)

val L = [(1,2), (3,4)];
val M = (1,2);
val N = (3,4);

fun myfoldr _ init nil = init
  | myfoldr f init (x::xs) = f(x, myfoldr f init xs);


fun myfoldl f init lst =
    let
	fun helper (nil, acc) = acc
	  | helper (x::xs, acc) = helper(xs, f(x, acc))
    in
	helper (lst, init)
    end;

(* 5.4.6 exercise for section 5.4 *)
(* 5.4.1 *)
fun listToN n =
    let
	fun helper (0, acc) = acc
	  | helper (n, acc) = helper (n-1, n-1::acc)
    in
	helper (n, nil)
    end




fun tabulate (a, delta, n, f) =
    let
	val xs = map (fn k => a+real(k)*delta) (listToN n)
	val fxs = map f xs
    in
	(
	  app (fn x => print( Real.toString x ^ " " )) xs;
	  print "\n";
	  app (fn fx => print( Real.toString fx ^ " " )) fxs
	)
    end;
