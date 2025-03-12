structure String: TOTALORDER =
struct
type element = string
fun lt (x, y) = (String.map Char.toLower x) < (String.map Char.toLower y)
end

structure StringBST = MakeBST(String)
