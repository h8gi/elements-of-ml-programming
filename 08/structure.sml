structure Mapping = struct
exception NotFound

val create = nil

(* lookup(key, mapping) finds the range value r such that
   (key,r) is a pair in mapping *)
fun lookup (_, nil) = raise NotFound
  | lookup (key, (existing_key, value)::rest) =
    if key = existing_key then value
    else lookup (key, rest)

(* insert(key, value, mapping) puts (key, value) in mapping and removes
   any other pair (key,s) that was present in mapping *)
fun insert (key, value, nil) = [(key,value)]
  | insert (key, value, (existing_key,existing_value)::rest) =
    if key = existing_key then (key,value)::rest
    else (existing_key,existing_value)::insert(key,value,rest)
end

signature SIMAPPING = sig
    exception NotFound
    val create : (string * int) list
    val lookup : string * (string * int) list -> int
    val insert : string * int * (string * int) list -> (string * int) list
end

structure SiMapping: SIMAPPING = Mapping
