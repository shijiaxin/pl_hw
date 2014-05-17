(* Dan Grossman, Coursera PL, Homework2 Tests *)

use "5100379041_hw2.sml";

print "================================\n";
print "Begin testing\n";
print "problem 1\n";
(* These are tests for problem 1*)

all_except_option ("a",[]); (* return NONE *)

all_except_option ("a",["a"]); (* return SOME []*)

all_except_option ("",["a","abc","a"]); (* return NONE *)

all_except_option ("",["a","","a"]); (* return SOME ["a","a"] *)


get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred"); (* return ["Fredrick","Freddie","F"] *)

get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff");
(* return ["Jeffrey","Geoff","Jeffrey"] *)

get_substitutions1([[]],""); (* retrun [] *)

get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred"); (* return ["Fredrick","Freddie","F"] *)

get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff");
(* return ["Jeffrey","Geoff","Jeffrey"] *)

get_substitutions2([[]],""); (* retrun [] *)


similar_names([["Elizabeth","Betty"]], {first="Fred", middle="W", last="Smith"});
(* return [{first="Fred", last="Smith", middle="W"}]*)

similar_names([], {first="Fred", middle="W", last="Smith"});
(* return [{first="Fred", last="Smith", middle="W"}]*)

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"});
(* return [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"}, 
{first="F", last="Smith", middle="W"}] *)


(* tests for problem 2 *)

print "problem 2\n";
card_color(Diamonds, Jack); (* return Red *)

card_color(Hearts, Jack); (* return Red *)

card_color(Clubs, Num(1)); (* return Black *)

card_color(Spades, Num(0)); (* return Black *)

card_value(Diamonds, Jack); (* return 10 *)

card_value(Hearts, King); (* return 10 *)

card_value(Clubs, Num(1)); (* return 1 *)

card_value(Spades, Ace); (* return 11 *)

exception e;

remove_card([(Clubs, Jack), (Diamonds, Num(2)), (Clubs, Queen)], (Diamonds, Num(2)), e); (* return [(Clubs, Jack), (Clubs, Queen)] *)

(*
remove_card([(Clubs, Jack), (Diamonds, Num(2)), (Clubs, Queen)], (Hearts, King),e); (* raise exception e *)
*)

all_same_color([(Clubs, Jack), (Diamonds, Num(2)), (Clubs, Queen)]); (* return false *)

all_same_color([(Clubs, Jack), (Spades, Num(2)), (Clubs, Queen)]); (* return true *)

all_same_color([(Hearts, Jack), (Diamonds, Num(2)), (Diamonds, Queen)]); (* return true *)

sum_cards([(Clubs, Ace), (Diamonds, Num(2)), (Clubs, Queen)]); (* return 23 *)

sum_cards([(Clubs, Jack), (Spades, Num(2)), (Clubs, Queen)]); (* return 22 *)

sum_cards([]); (* return 0 *)

score([(Clubs, Jack), (Diamonds, Num(2)), (Clubs, Queen)], 21); (* return 3 *)

score([(Clubs, Jack), (Spades, Num(2)), (Clubs, Queen)], 5); (* return 25 *)

score([(Clubs, Jack), (Spades, Num(2)), (Hearts, Queen)], 32); (* return 10 *)

score([(Hearts, Jack), (Diamonds, Num(2)), (Diamonds, Ace)], 24); (* return 0 *)



val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Spades, Ace)];
val moves = [Draw,Draw,Draw,Draw,Draw];
officiate(cards,moves,42); (* return 3 *)

(*
val cards = [(Clubs,Jack),(Spades,Num(8))];
val moves = [Draw,Discard(Hearts,Jack)];
officiate(cards,moves,42); (* raise IllegalMove *)
*)


(* tests for problem 3 *)
print "problem 3\n";
score_challenge ([(Clubs, Ace)], 10); (* return 1 *)

score_challenge ([(Clubs, Ace)], 8); (* return 3 *)

score_challenge ([(Clubs, Ace),(Diamonds, Num(0))], 8);  (* return 7 *)

score_challenge ([(Clubs, Jack),(Diamonds, Num(0))], 9);  (* return 3 *)

score_challenge ([(Clubs, Jack),(Diamonds, Num(0))], 7);  (* return 9 *)

score_challenge ([(Clubs, Ace),(Diamonds, Ace)], 2);  (* return 0 *)

score_challenge ([(Clubs, Ace),(Diamonds, Ace)], 1);  (* return 3 *)

score_challenge ([(Clubs, Ace),(Diamonds, Ace)], 23);  (* return 1 *)


val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)];
val moves = [Draw,Draw,Draw,Draw,Draw];
officiate_challenge(cards,moves,42); (* return 3 *)

officiate_challenge(cards,moves,11); (* return 3 *)
officiate(cards,moves,11); (* returns 16 *)

officiate_challenge(cards, moves, 1); (* return 1*)
officiate(cards,moves,1); (* return 15 *)

careful_player(cards, 20);
(*[Draw]*)
val cards = [(Spades,Num(7)),(Hearts,King),(Clubs,Ace),(Diamonds,Num(2))];
careful_player(cards, 18);
(*[Draw,Draw,Discard (Hearts,King),Draw]*)
