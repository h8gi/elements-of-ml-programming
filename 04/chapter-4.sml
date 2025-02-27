fun testZero (0) = print("zero\n")
  | testZero (_) = print("not zero\n");

fun printList (nil) = ()
  | printList (x::xs) = (
      print(Int.toString(x));
      print("\n");
      printList(xs)
  );

(* 4.1.4 *)
(3;4;5);
let
in
    3;4;5
end;

(* 4.1.5 *)
(* ex 4.1.1 *)
(* errorはでない *)

(* ex 4.1.2 *)
fun comb(n, m) =
    if m <= 0 orelse n <= m then 1
    else comb(n-1, m) + comb(n-1, m-1);

fun comb_p(n, m) =
    (
      print("Select " ^ Int.toString(m) ^ " items from " ^ Int.toString(n) ^ " items.\n");
      print("The combination is " ^ Int.toString(comb(n, m)) ^ ".\n")
    );

(* 4.1.3 *)
fun build_2nx 0 s = s
  | build_2nx n s = build_2nx (n-1) (s ^ s)

fun print_2nx n = print ((build_2nx n "X") ^ "\n");
(* 4.1.4. skip... *)
(* Write a function that, given n, prints 2^n X's using only log2(N) recursive calls. *)

(* example 4.9 *)
local
    fun helper (infile, NONE) = nil
      | helper (infile, SOME c) =
	c :: helper (infile, TextIO.input1(infile))
in
fun makeList infile = helper(infile, TextIO.input1(infile));
end


(* 4.2.8 *)
(* ex 4.2.1 *)
(* a *)
val zap = TextIO.openIn("zap");
(* b *)
TextIO.closeIn zap;
(* c *)
val zap = TextIO.openIn "zap";
TextIO.inputN(zap, 5);
