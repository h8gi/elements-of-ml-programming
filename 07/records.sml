val Norm'sRecord = {
    ID=123,
    name="Norm dePlume",
    courses=["CS106X", "E40", "M43"]
};

exception NotFound;
fun getID (person, nil) = raise NotFound
  | getID (person, (x as {name=p, ...}) :: xs) =
    if p = person then
	#ID(x)
    else getID(person, xs);
