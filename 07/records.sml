val Norm'sRecord = {
    ID=123,
    name="Norm dePlume",
    courses=["CS106X", "E40", "M43"]
};

(* Define a type alias for a student record *)
type student = {ID: int, name: string, courses: string list};

exception NotFound;
fun getID (_ : string, nil) = raise NotFound
  | getID (person : string, (x : student) :: xs) =
    (if #name x = person then
	#ID x
    else getID(person, xs))
(* Note: The `student` type does *not* include an ellipsis (`...`).
 * This means that a value of type `student` must *only* have the fields
 * `ID`, `name`, and `courses`.  It cannot have any other fields.
 * If you wanted to allow *other* fields in addition to these, you would
 * need to include the ellipsis in the type definition, like this:
 * `type student = {ID: int, name: string, courses: string list, ...}`
 *)

fun tuition {name=_, ID=_, courses=nil} = 1000
  | tuition {courses=[ _ ], ...} = 2000
  | tuition {ID=i, ...} =
    if i>=100000 then 5000
    else 4000;
