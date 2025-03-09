structure Queue = struct
exception EmptyQueue

val create = []
fun enqueue (x, q) = q @ [x]
fun dequeue nil = raise EmptyQueue
  | dequeue (x::xs) = (x, xs)
fun isEmpty nil = true
  | isEmpty _ = false
end

signature PQUEUE = sig
    exception EmptyQueue
    val create : (string * int) list
    val enqueue : ((string * int) * (string * int) list) -> (string * int) list
    val dequeue : (string * int) list -> ((string * int) * (string * int) list)
    val isEmpty : (string * int) list -> bool
end

structure PairQueue : PQUEUE = Queue
