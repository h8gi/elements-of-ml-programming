structure Triple: TOTALORDER =
struct
type element = real * real * real
fun lt ((a,b,c):(real*real*real), (x,y,z)) =
    a < x orelse
    (not (x < a) andalso b < y) orelse
    (not (x < a) andalso not (b < y) andalso c < z)
end

structure TripleBST = MakeBST(Triple)
