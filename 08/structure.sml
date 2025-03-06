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
    else lookup(key, rest)

(* insert(key, value, mapping) puts (key, value) in mapping and removes
   any other pair (key,s) that was present in mapping *)
fun insert (key : key, value : value, nil) = [(key,value)]
  | insert (key : key, value : value, (existing_key,existing_value)::rest) =
    case key = existing_key of
	true => (key,value)::rest
      | false => (existing_key,existing_value)::insert(key,value,rest)
end
