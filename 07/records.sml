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

fun tuition {courses=nil, ...} = 1000
  | tuition ({courses=[ _ ], ...} : student) = 2000
  | tuition {ID, ...} =
    if ID>=100000 then 5000
    else 4000;

(* Test cases for tuition function *)
val student1 = {ID=12345, name="Alice", courses=nil};
val student2 = {ID=67890, name="Bob", courses=["CS101"]};
val student3 = {ID=100000, name="Charlie", courses=["EE200", "MA101"]};
val student4 = {ID=99999, name="David", courses=["ME101", "PH101"]};

val test1 = tuition student1 = 1000;  (* No courses *)
val test2 = tuition student2 = 2000;  (* One course *)
val test3 = tuition student3 = 5000;  (* ID >= 100000 *)
val test4 = tuition student4 = 4000;  (* ID < 100000 *)

(* 7.1.6 ex for 7.1 *)
(* 7.1.1 *)
(* weight in tons, height in feet *)
type deno = {name: string,
             weight: real,
             height: real};
val tyranno = {name="tyranno", weight=7.0, height=20.0};
