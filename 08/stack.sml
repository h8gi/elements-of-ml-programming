(* exercise 8.2.4 *)
structure Stack = struct

exception EmptyStack

(* return empty stack *)
val create = []

fun pop (nil:'a list) = raise EmptyStack
  | pop (_::xs) = xs

fun push (x, s:'a list) = x::s

fun top (nil:'a list) = raise EmptyStack
  | top (x::_) = x

fun isEmpty (nil: 'a list) = true
  | isEmpty _ = false

end

signature SSTACK = sig
    val create : string list
    val pop : string list -> string list
    val push : (string * string list) -> string list
    val isEmpty : string list -> bool
end

structure StringStack : SSTACK = Stack
