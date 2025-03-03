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
type dino = {name: string,
             weight: real,
             height: real};
val tyranno = {name="tyranno", weight=7.0, height=20.0};
val brachio = {name="brachio", weight=80.0, height=50.0};
#height tyranno;
#weight brachio;

(* 7.1.2 *)
fun tallestDino nil = raise NotFound
  | tallestDino [x:dino] = x
  | tallestDino ((x as {height,...})::xs) =
    let
	val t as {height=t_height,...} = tallestDino xs
    in
	if height > t_height then x
	else t
    end;

(* Test case for tallestDino *)
val dino_list = [tyranno, brachio, {name="stego", weight=3.0, height=12.0}];
val tallest = tallestDino dino_list;
val test_tallest = #name tallest = "brachio";

fun averageWeight (l : dino list) =
    let
	val sum = foldr (op +) 0.0 (map #weight l)
	val n = real(length l)
    in
	sum/n
    end;
(* 7.1.3 *)
(* a. find all records with name = n *)
fun findStudentsByName (_, nil : student list) = nil
  | findStudentsByName (n, (x as {name, ...})::xs) =
    let
	val rest = findStudentsByName (n, xs)
    in
	if name = n then x :: rest else rest
    end;

val student5 = {ID=55555, name="Alice", courses=["CS101"]};
val student6 = {ID=77777, name="Bob", courses=["EE200"]};
val student_list = [student1, student2, student3, student4, student5, student6];
val alice_list : student list = findStudentsByName ("Alice", student_list);

(* Test cases for findStudentsByName *)
val test_find1 = length alice_list = 2;
val test_find2 = #ID (hd alice_list) = 12345;

val student7 = {ID=88888, name="Carol", courses=["MA101"]};
val carol_list = findStudentsByName ("Carol", [student7]);
val test_find3 = #ID (hd carol_list) = 88888;
(* 7.1.3. b *)
fun findCoursesByID (_, nil : student list) = NONE
  | findCoursesByID (i, (x as {ID, ...})::xs) =
    if ID = i then SOME(#courses x) else findCoursesByID(i, xs);

findCoursesByID(99999, student_list);
(* 7.1.3. c *)
fun findEnrolleeByCourse (_, nil : student list) = nil
  | findEnrolleeByCourse (c, {name, courses, ...}::xs) =
    let
	val found = List.exists (fn elem => elem = c) courses
	val rest = findEnrolleeByCourse(c, xs)
    in
	if found then name :: rest else rest
    end;
