(* exercise 8.3.2 *)
signature SIM = sig
    type element
    val similar : element * element -> bool
end

functor MakeSimSet(Sim: SIM) =
struct
open Sim
val create = [] : element list
fun findSim (_, []) = []
  | findSim (x, y::ys) =
    if similar(x,y) then y :: findSim(x, ys)
			 else findSim(x, ys)
fun insert (x, ys) = x::ys
end

structure Misspell : SIM =
struct
type element = string

fun diffCount ([], []) = 0
  | diffCount (rest, []) = length rest
  | diffCount ([], rest) = length rest
  | diffCount (x::xs, y::ys) =
    if x = y then diffCount(xs, ys) else 1+diffCount(xs, ys)

fun similar (x, y) = diffCount(explode x, explode y) < 2

end

structure MisspellSet = MakeSimSet(Misspell)
