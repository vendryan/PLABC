use "hw22.sml";
(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string", "noob", "toom"]) = SOME ["noob", "toom"];

val test2 = get_substitutions1 ([["foo", "boo", "too"],["there", "foo"]], "foo") = ["boo", "too", "there"];

val test3 = get_substitutions2 ([["foo", "boo", "too"],["there", "foo"]], "foo") = ["boo", "too", "there"];
val test3_2 = get_substitutions2 ([["foo", "boo", "too"],["there", "foo"]], "foo");

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}];

val test5 = card_color (Clubs, Num 2) = Black;

val test6 = card_value (Clubs, Num 2) = 2;

val test7 = remove_card ([(Clubs, Num 4),(Hearts, Ace),(Clubs, Num 2)], (Hearts, Ace), IllegalMove)
									    
val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

		 (*
val strange = [(Hearts, Num 10), (Spades, Num 10), (Clubs, Num 10), (Diamonds, Num 10), (Spades, Ace)];
val testc = score_challenge (strange, 40);
val a = sum_cards(strange);
val b = score(strange, 40);
*)
