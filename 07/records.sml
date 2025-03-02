val Norm'sRecord = {
    ID=123,
    name="Norm dePlume",
    courses=["CS106X", "E40", "M43"]
};

exception NotFound;
fun getID (person : string, nil) = raise NotFound
  | getID (person : string, (x : {ID: int, name: string, courses: string list}) :: xs) =
    if #name(x) = person then
	#ID(x)
    else getID(person, xs);
