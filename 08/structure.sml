structure Mapping = struct
exception NotFound

(* create the empty mapping *)
val create = nil

(* lookup(d,M) finds the range value r such that
   (d,r) is a pair in mapping M *)
fun lookup (_, nil) = raise NotFound
  | lookup (d, (e, r)::es) =
    if d = e then r
    else lookup(d, es)

(* insert(d,r,M) puts (d,r) in mapping M and removes
   any other pair (d,s) that was present in M *)
fun insert (d, r, nil) = [(d,r)]
  | insert (d, r, (e,s)::es) =
    if d = e then (d,r)::es
    else (e,s)::insert(d,r,es)
end
