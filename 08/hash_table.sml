functor MakeHashTable(eqtype eltype
		      val b : int
		      val h : eltype -> int)
=
struct
open Array
fun create () = array(b, nil: eltype list)

local
    fun insertList (x, nil : eltype list) = [x]
      | insertList (x, y::ys) =
	if x=y then y::ys
	else y::insertList(x, ys)
in
fun insert (x, A) =
    let
	val bucket = h x
	val L = sub(A, bucket)
    in
	update(A, bucket, insertList(x,L))
    end
end

local
    fun deleteList (_, nil : eltype list) = nil
      | deleteList (x, y::ys) =
	if (x=y) then ys
	else y::deleteList(x, ys)
in
fun delete (x, A) =
    let
	val bucket = h x
	val L = sub(A, bucket)
    in
	update(A, bucket, deleteList(x,L))
    end
end

local
    fun lookupList (_, nil : eltype list) = false
      | lookupList (x, y::ys) = x=y orelse lookupList(x,ys)
in
fun lookup (x, A) = lookupList(x, sub(A, h x))
end

end

functor MakeHashFun(val b : int)
=
struct
type eltype = string
val b = b
local
    fun h1 nil = 0
      | h1 (x::xs) = (ord x + h1 xs) mod b
in
fun h x = h1(explode x)
end
end


structure HashFn100 = MakeHashFun(val b=100)
structure Hash100 = MakeHashTable(val b = HashFn100.b
				  type eltype = HashFn100.eltype
				  val h = HashFn100.h
				 )
