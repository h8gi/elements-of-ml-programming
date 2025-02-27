exception Syntax;

fun digit(c) = (#"0" <= c andalso c <= #"9");

fun integer (IN, i) =
    case TextIO.lookahead IN of
	SOME c => if digit c then (
		      TextIO.input1 IN;	(* consume character c *)
		      integer(IN, 10*i + ord c - ord #"0")
		  ) else i		(* if c is not a digit, return i
					   without consuming input *)
      | NONE => i		(* ditto if end of file is reached *);

fun atom IN =
    case TextIO.lookahead IN of
	SOME #"(" => (
	 TextIO.input1 IN;	(* consume left paren *)
	 let
	     val e = expression IN
	 in
	     if TextIO.lookahead IN = SOME #")" then
		 (
		   TextIO.input1 IN; (* consume right paren *)
		   e		     (* return expression *)
		 )
	     else raise Syntax
	 end
     )
      | SOME c => if digit c then integer(IN, 0)
		  else raise Syntax
      | NONE => raise Syntax
and term IN = termTail(IN, atom IN)
and termTail (IN, i) =
    case TextIO.lookahead IN of
	SOME #"*" => (
	 TextIO.input1 IN;	(* consume * *)
	 termTail(IN, i*atom IN)
     )
      | SOME #"/" => (
	  TextIO.input1 IN;	(* consume / *)
	  termTail(IN, i div (atom IN))
      )
      | _ => i
and expression IN = expTail(IN, term IN)
and expTail(IN, i) =
    case TextIO.lookahead IN of
	SOME #"+" => (
	 TextIO.input1 IN;	(* consume + *)
	 expTail(IN, i+term IN)
     )
      | SOME #"-" => (
	  TextIO.input1 IN;	(* consume - *)
	  expTail(IN, i-term IN)
      )
      | _ => i

val infile = TextIO.openIn "arith_test.txt";
expression(infile)
