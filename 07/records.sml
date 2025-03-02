val Norm'sRecord = {
    ID=123,
    name="Norm dePlume",
    courses=["CS106X", "E40", "M43"]
};

(* Define a type alias for a student record *)
type student = {ID: int,
		name: string,
		courses: string list};

exception NotFound;
fun getID (_ : string, nil) = raise NotFound
  | getID (person : string, (x : student) :: xs) =
    (if #name x = person then
	#ID x
    else getID(person, xs))

fun tuition {name=_, ID=_, courses=nil} = 1000
  | tuition {courses=[ _ ], ...} = 2000
  | tuition {ID=i, ...} =
    if i>=100000 then 5000
    else 4000;
