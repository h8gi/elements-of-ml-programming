structure Mapping = struct
exception NotFound

(* type of key *)
type key = string

(* type of value *)
type value = int

(* create the empty mapping *)
val create : (key * value) list = nil

(* lookup(key, mapping) finds the range value r such that
   (key,r) is a pair in mapping *)
fun lookup (_, nil) = raise NotFound
  | lookup (key, (existing_key, value)::rest) =
    if key = existing_key then value
    else lookup (key, rest)

(* insert(key, value, mapping) puts (key, value) in mapping and removes
   any other pair (key,s) that was present in mapping *)
fun insert (key : key, value : value, nil) = [(key,value)]
  | insert (key : key, value : value, (existing_key,existing_value)::rest) =
    if key = existing_key then (key,value)::rest
    else (existing_key,existing_value)::insert(key,value,rest)

  (* map(f, mapping) applies f to all values in mapping, returning a new mapping *)
  fun map (_, nil) = nil
    | map (f, (key, value)::rest) = (key, f value) :: map (f, rest)

  (* fold(f, acc, mapping) folds the mapping with function f and initial value acc *)
  fun fold (_, acc, nil) = acc
    | fold (f, acc, (key, value)::rest) = fold (f, f(key, value, acc), rest)
end
