(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
(****--------------------------------****)
			       
(**** you can put all your code here ****);
(* Seperate into different line for easier read *);
val foldl = List.foldl;
val zip = ListPair.zip;

(* Problem 1 *)
val only_capitals =
    List.filter (fn (str) =>
		    Char.isUpper(String.sub(str, 0)))

(* foldl f init [x1, x2, ..., xn]
   returns f(xn,...,f(x2, f(x1, init))...)
   or 
   init if the list is empty. *);

(* Problem 2 *)
val longest_string1 = 
    foldl (fn (str1,str2) =>
	      if String.size str1 > String.size str2
	      then str1
	      else str2)
	  "";

(* Problem 3 *)
val longest_string2 =
    foldl (fn (str1,str2) =>
	      if String.size str1 >= String.size str2
	      then str1
	      else str2)
	  "";

(* Problem 4 *)
fun longest_string_helper f =
    foldl (fn (str1,str2) =>
	      if f(String.size str1,String.size str2)
	      then str1
	      else str2)
	  "";

val longest_string3 = longest_string_helper (op >);
val longest_string4 = longest_string_helper (op >=);

(* Problem 5 *)
val longest_capitalized = longest_string1 o only_capitals;

(* Problem 6 *)
val rev_string = String.implode o List.rev o String.explode;

(* Problem 7 *)
fun first_answer f list =
    case list of
	[]     => raise NoAnswer
      | hd::tl => case f(hd) of
		      NONE   => first_answer f tl
		    | SOME x => x;

(* Problem 8 *)
fun all_answers f list =
    let fun loop list result =
	    case list of
		[]      => SOME result
	     |  hd::tl  => (case f(hd) of
			        NONE   => NONE
			      | SOME x => loop tl (result @ x))
    in
	loop list []
    end;

(* Problem 9a *)
val count_wildcards = g (fn () => 1) (fn x => 0);

(* Problem 9b *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x);

(* Prblem 9c *)
fun count_some_var (x,p) =
    g (fn () => 0)
      (fn y => if x = y
	       then 1
	       else 0)
      p;

(* Problem 10 *)
fun check_pat p =
    let fun get_variable_list p =
	    case p of
		Wildcard           => []
	      | Variable s         => [s]
	      | UnitP              => []
	      | ConstP i           => []
	      | TupleP lst         => foldl (fn (x,tl) => get_variable_list(x) @ tl) [] lst
	      | ConstructorP (_,c) => get_variable_list(c)
	fun check_duplicate lst =
	    case lst of
		[]     => true
	      | hd::tl => not(List.exists (fn x => x = hd) tl) andalso check_duplicate(tl)
    in
	check_duplicate(get_variable_list p)
    end;

(* Problem 11 *)
fun match (v,p) =
    case (v,p) of
	(_,Wildcard)       => SOME []
      | (Const x,ConstP y) => if x = y
			      then SOME []
			      else NONE
      | (Unit,UnitP)       => SOME []
      | (x,Variable s)     => SOME [(s,x)]
      | (Tuple x,TupleP y) => if List.length x = List.length y
			      then all_answers (fn (x,y) => match(x,y)) (zip(x,y)) (* Unnecesary function wrapping *)
			      else NONE
      | (Constructor(s1,x),ConstructorP(s2,y)) => if s1 = s2
						  then match(x,y)
						  else NONE
      | _ => NONE;

(* Problem 12 *)
fun first_match v ps =
    ((SOME(first_answer (fn x => match(v,x)) ps))
	  handle NoAnswer => NONE)
	
