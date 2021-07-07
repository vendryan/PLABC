fun sum_list (x) =
    case x of
	[] => 0
      | x::xs => x + sum_list (xs)

fun minus_list (x) =
    case x of
	[] => 0
      | x::xs => x - minus_list(xs);

datatype list = empty
	      | cons of int * list;

val test_list = cons(5, cons(6, empty)); 

fun sum (x) =
    case x of
	empty => 0
      | cons(x,xs) => x + sum(xs);

val test = sum(test_list);
val x = {foo = 5,bar = 6}; 

fun testing(x,y) =
    case x of
	x::(y::z) => y;

val y = testing x;
