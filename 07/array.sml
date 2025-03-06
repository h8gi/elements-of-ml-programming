fun member (_, nil) = false
  | member (x, y::ys) =
    x = y orelse member(x, ys)

(* memberAll(ch,L) checks that character ch and all
   following characters up to "z" are on list L *)
fun memberAll(ch, L) =
    if ch > #"z" then true
    else member(ch, L) andalso
	 memberAll(chr(1+ord ch), L);

memberAll (#"a", explode "abcdefgjhixjkalmnopqrstuvwxyz");

(* checkAll(A,i) checks that array A has only true
   elements with indexes 0 through i *)
fun checkAll A =
    let
	fun helper i =
	    i < 0 orelse
	    Array.sub(A, i) andalso helper (i-1)
    in
	helper (Array.length A - 1)
    end;

(* fillAndCheck(A,L) sets the element of array A to true
   for each letter appearing on list L, then checks
   that all elements are true *)
fun fillAndCheck (A, nil) = checkAll A
  | fillAndCheck (A, x::xs) = (
      Array.update(A, ord x - ord #"a", true);
      fillAndCheck(A, xs)
  );

fillAndCheck(Array.array(26, false), explode "qwertyuiopasdfghjklzxcvbnm");

(* 7.2.3 Exercise for Section 7.2 *)
(* Exercise 7.2.1 *)
(* a. *)
val a1 = Array.array(20, nil : real list);
val a2 = Array.array(100, 0.0);
val _ = Array.sub(a2, 29);
val _ = Array.sub(Array.array(20, 0), 9);
val _ = Array.update(a2, 9, 43.0);
val _ = Array.update(a1, 0, [1.0,2.0,3.0]);
(* Exercise 7.2.2 *)
(* bubble sort *)
fun swapIfUnsorted (arr, i) =
  let
      val i_elem = Array.sub(arr, i)
      val j_elem = Array.sub(arr, i+1)
  in
      if i_elem > j_elem then (Array.update(arr, i, j_elem);
			       Array.update(arr, i+1, i_elem); true)
      else false
  end

fun or(a, b) = a orelse b;

fun bubbleSortA A =
    let
	val n = Array.length A
	fun bubblePass unsorted i =
	    if i < n-1 then bubblePass (or(unsorted, swapIfUnsorted(A, i))) (i+1)
	    else unsorted
    in
	if bubblePass false 0 then bubbleSortA A
	else ()
    end;
(* bubble sort with list *)
fun bubbleSort [] = []
  | bubbleSort lst =
    let
      fun bubblePass [] = []
        | bubblePass [x] = [x]
        | bubblePass (x::y::rest) =
          if x > y then
            y :: bubblePass (x :: rest)
          else
            x :: bubblePass (y :: rest);

      val sortedList = bubblePass lst
    in
      if sortedList = lst then
        lst  (* Already sorted *)
      else
        bubbleSort sortedList
    end;

(* merge sort *)
val testArray3 = Array.fromList [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5];
fun split A =
    let
	val xs = 		(* TODO: *)
