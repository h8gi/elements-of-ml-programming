fun reverse nil = nil
  | reverse (x::xs) = (reverse xs) @ [x]

fun comb(n, m) =
    if m <= 0 orelse n <= m then 1
    else comb(n-1, m) + comb(n-1, m-1);

fun factorial n =
    if n > 1 then n * factorial (n-1)
    else 1

fun cycle nil = nil
  | cycle (x::xs) = xs @ [x]

fun cycle_n n lst =
    if n > 0 then cycle_n (n-1) (cycle lst)
    else lst;

fun dupelem nil = nil
  | dupelem (x::xs) = x::x::dupelem xs

local
    fun inner_len n nil = n
      | inner_len n (x::xs) = inner_len (n+1) xs
in
fun len lst = inner_len 0 lst
end

fun power (x, i) =
    if i > 0 then x * power (x, i-1)
    else 1

fun foo nil = [3]
  | foo x = x;

(* use pattern matching *)
fun factorial 0 = 1
  | factorial n = n * factorial (n-1)

fun cycle_n 0 lst = lst
  | cycle_n n nil = nil
  | cycle_n n lst = cycle_n (n-1) (cycle lst)

fun dup nil = nil
  | dup (x::xs) = x::x::dup xs

fun power (x, 0) = 1
  | power (x, n) = x * power(x, n-1)

(* 3.2.1 *)
fun max_l nil = Real.negInf
  | max_l (x::nil) = x
  | max_l (x::y::zs) = if x > y then max_l (x::zs) else max_l (y::zs)

fun flipAlt nil = nil
  | flipAlt (x::nil) = [x]
  | flipAlt (x::y::zs) = y::x::(flipAlt zs)

fun delete_i i nil = nil
  | delete_i 0 (x::xs) = xs
  | delete_i n (x::xs) = x::delete_i (n-1) xs

(* val [(x,y), zs] = [((1,2), 3)] *)
val (x,y)::zs = [((1,2), 3)]

fun square 0 = 0
  | square n = square (n-1) + 2*n -1

fun order_pair_l nil = nil
  | order_pair_l ((xy as (x,y))::zs) =
    if x < y then xy :: order_pair_l zs
    else (y,x) :: order_pair_l zs

fun vowelStart (#"a"::_) = true
  | vowelStart (#"e"::_) = true
  | vowelStart (#"i"::_) = true
  | vowelStart (#"o"::_) = true
  | vowelStart (#"u"::_) = true
  | vowelStart (#"A"::_) = true
  | vowelStart (#"E"::_) = true
  | vowelStart (#"I"::_) = true
  | vowelStart (#"O"::_) = true
  | vowelStart (#"U"::_) = true
  | vowelStart _ = false;

local
    fun helper (nil, acc) = acc
      | helper (lst as x::xs, acc) =
	if vowelStart(lst) then lst @ acc
	else helper (xs, acc @ [x])
in
fun cycleToVowel lst = helper (lst, nil)
end;

(* 3.3.10 *)
(* pig latin *)
fun pigLatin str =
    let
	val lst = explode(str)
    in
	if vowelStart(lst) then str ^ "yay"
	else implode(cycleToVowel(lst)) ^ "ay"
    end

fun member (x, nil) = false
  | member (x, y::ys) = if x = y then true
			else member (x, ys)

fun delete (x, nil) = nil
  | delete (x, y::ys) = if x = y then ys
			else y :: delete (x, ys)

fun insert (x, lst) =
    if member (x, lst) then lst
    else x::lst

fun map_cons (x, nil) = nil
  | map_cons (x, y::ys) = (x::y)::map_cons(x, ys)

(* 3.3.13 power set *)
fun power_set nil = [[]]
  | power_set (x::xs) = let
      val n_1 = power_set xs
  in
      n_1 @ map_cons(x, n_1)
  end

(* 3.3.14 *)
(* Π(a-b_i) *)
fun sub_and_prod (a, lst) =
    let fun helper (nil, acc) = acc
	  | helper (x::xs, acc) = helper (xs, acc * (a-x))
    in
	helper(lst, 1.0)
    end

fun sub_and_prod_whole nil = 1.0
  | sub_and_prod_whole (x::xs) =
    sub_and_prod (x, xs) * sub_and_prod_whole xs;

(* 3.3.15 *)
fun is_nil nil = true
  | is_nil _ = false;

(* example 3.28 *)
fun split nil = ([], [])
  | split (x::nil) = ([x], [])
  | split (x::y::zs) =
    let val (xs, ys) = split zs
    in
	(x::xs, y::ys)
    end

local
    fun helper (M, N, nil) = (M, N)
      | helper (M, N, [x]) = (x::M, N)
      | helper (M, N, x::y::zs) = helper (x::M, y::N, zs)
in
fun split lst = helper (nil, nil, rev lst)
end

fun printIntList ints = app (fn i => print(Int.toString i ^ " ")) ints;
fun intListToString ints = "[" ^ (concat (map (fn i => Int.toString i ^ " ") ints)) ^ "]";

fun merge (nil, m) = m
  | merge (l, nil) = l
  | merge (l as x::xs, m as y::ys) =
    if x < y then x::merge(xs, m)
    else y::merge(l, ys);

fun mergeSort nil = nil
  | mergeSort ([a]) = [a]
  | mergeSort L =
    let
	val (M, N) = split(L);
	val M = mergeSort(M);
	val N = mergeSort(N)
	val str = intListToString M ^ "| " ^ intListToString N ^ "\n"
    in
	(* print(str); *)
	merge(M, N)
    end;

(* 3.4.1 *)
fun powerOf1000 (x: IntInf.int) =
    let
	val x = x * x * x * x;
	val x = x * x * x * x * x;
	val x = x * x * x * x * x;
	val x = x * x;
	val x = x * x * x * x * x;
    in
	x
    end;
(* 3.4.2 *)
(* 3.4.3 *)
(* 3.4.4 *)
fun max_l nil = Real.negInf
  | max_l (x::nil) = x
  | max_l (x::y::zs) = if x > y then max_l (x::zs) else max_l (y::zs)

fun max_list nil = Real.negInf
  | max_list (x::nil) = x
  | max_list (x::zs) = let
      val max = (max_list zs)
  in
      if x > max then x else max
  end;

(* 3.4.5 *)
(* (x^2)^i *)
fun power_of_2_of_i (x, 0) = x
  | power_of_2_of_i (x, i) =
    let
      val y = power_of_2_of_i(x,i-1)
    in
	y*y
    end;

(* 3.4.6 *)
fun sumPairs nil = (0, 0)
  | sumPairs [x] = x
  | sumPairs ((f, s)::xs) =
    let
	val (f_sum, s_sum) = sumPairs xs
    in
	(f+f_sum, s+s_sum)
    end;

(* 3.4.7 *)
fun sumEvenOdd nil = (0, 0)
  | sumEvenOdd [x] = (x, 0)
  | sumEvenOdd (even::odd::xs) =
    let
	val (even_sum, odd_sum) = sumEvenOdd xs
    in
	(even + even_sum, odd + odd_sum)
    end;

(* 3.5 case study: linear-time reverse *)
(* bad reverse *)
fun reverse (nil) = nil
  | reverse (x::xs) = reverse(xs) @ [x];

local
    fun helper (nil, acc) = acc
      | helper (x::xs, acc) = helper (xs, x::acc)
in
fun reverse2 lst = helper (lst, [])
end;


fun rev1 (nil, M) = M
  | rev1 (x::xs, ys) = rev1 (xs, x::ys);

fun reverse (L) = rev1(L, nil);

(* ex 3.5.1 *)
fun cat (L, M) = rev1(reverse(L), M);

(* ex 3.5.2 *)
(* fun cycle nil = nil *)
(*   | cycle (x::xs) = xs @ [x] *)

(* fun cycle_n n lst = *)
(*     if n > 0 then cycle_n (n-1) (cycle lst) *)
(*     else lst; *)

(* i should be smaller than the length of the list *)
exception list_exhasuted
fun split_at_i (L, 0) = ([], L)
  | split_at_i (nil, i) = raise list_exhasuted
  | split_at_i (x::xs, i) =
    let
	val (head, tail) = split_at_i (xs, i-1)
    in
	(x::head, tail)
    end

fun cycle (L, i) =
    let
	val i = i mod (length L)
	val (head, tail) = split_at_i (L, i)
    in
	tail @ head
    end;

(* padd(P, Q) produces the polynomial sum P+Q *)
fun padd (P, nil) = P
  | padd (nil, Q) = Q
  | padd ((p:real)::ps, q::qs) = (p+q) :: padd(ps, qs);

(* smult(P,q) multiplies polynomial P by scalar q *)
fun smult (nil, q) = nil
  | smult ((p:real)::ps, q) = (q*p) :: smult(ps, q);

(* pmult(P, Q) produces PQ *)
(* PQ = Pq + PSx *)
fun pmult (P, nil) = nil
  | pmult (P, q::qs) = padd(smult(P, q), 0.0::pmult(P, qs));

(* psub(P,Q) compute the difference of polynomials P-Q *)
fun psub (P, Q) = padd(P, smult(Q, ~1.0));

(* length(P) computes length (degree+1) of polynomial P *)
fun length (nil) = 0
  | length (p::ps) = 1+length(ps);

(* bestSplit(n,m) computes an appropriate size for the
   low-order "half" of polynomials of length n and m.
   It is the smaller of n and m should one be less than
   half the other. If they are approximately the
   same size, then it is half the larger. *)
fun bestSplit (n,m) =
    if 2*n <= m then n
    else if 2*m <=n then m
    else if n <= m then m div 2
    (* n/2 < m < n *)
    else n div 2;

(* shift(P, n) computes P times x^n, for polynomial P(x) *)
fun shift (P, 0) = P
  | shift (P, n) = 0.0::shift(P, n-1);

(* carve(P, n) returns a pair of polynomials. The first is
   the low-order n terms of P and theSecond is what remains
   of P, divided by x^n *)
fun carve (P, 0) = (nil, P)
  | carve (p::ps, n) =
    let
	val (qs, rs) = carve(ps, n-1)
    in
	(p::qs, rs)
    end;

(* komult(P,Q) computes the product of polynomials PQ using
   the Karatsuba-Ofman method that only calls itself three
   times rather than four on half-sized polynomials. *)
fun komult (P, nil) = nil
  | komult (nil, Q) = nil
  | komult (P, [q]) = smult(P, q)
  | komult ([p], Q) = smult(Q, p)
  | komult (P, Q) =
    let
	val n = length(P);
	val m = length(Q);
	val s = bestSplit(n,m);
	val (T, U) = carve(P, s);
	val (V, W) = carve(Q, s);
	val TV = komult(T, V);
	val UW = komult(U, W);
	val TUVW = komult(padd(T, U), padd(V, W));
	val middle = psub(psub(TUVW, TV), UW);
    in
	padd(padd(TV, shift(middle, s)), shift(UW, 2*s))
    end;

(* ex# 3.6.3 *)
fun eval (nil, a) = 0.0
  | eval (x::xs, a) = x + eval(smult(xs, a), a);

(* skip the rest.... *)
