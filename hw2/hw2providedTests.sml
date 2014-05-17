(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

;


all_except_option ("a", ["a","b","c"]);
all_except_option ("a", ["c","a","b"]);
all_except_option ("a", ["d","e"]);

get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred");
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff");

get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred");
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff");

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                   {first="Fred", middle="W", last="Smith"});

val c1=(Clubs,Num(2));
val c2=(Clubs,King);
val c3=(Diamonds,Ace);
all_same_color([]);
all_same_color([c1]);
all_same_color([c2]);
all_same_color([c1,c2]);
all_same_color([c1,c2,c3]);

(* 23 *)
sum_cards([c1,c2,c3]);
(* 6  *)
score([c1,c2],25);

fun my_test_officiate()=
    let
        val c1=(Clubs,Num(2))
        val c2=(Clubs,King)
        val c3=(Diamonds,Ace)
        val c4=(Spades,Num(5))
        val c5=(Hearts,Num(10))

        val cs1=[c1,c2,c3,c4,c5]
        val ms1=[Draw,Draw,Draw,Discard(c1)]
    in
        officiate(cs1,[Draw,Draw,Draw,Discard(c1)],24)::
        officiate(cs1,[Draw,Draw,Draw,Discard(c1)],13)::
        officiate(cs1,[Draw,Draw,Draw,Draw,Draw,Draw],70)::
        officiate(cs1,[],24)::
        []
    end;
(* [3,30,32,12] *)    
my_test_officiate();

fun my_test_officiate_challenge()=
    let
        val c1=(Clubs,Num(2))
        val c2=(Clubs,King)
        val c3=(Diamonds,Ace)
        val c4=(Spades,Ace)
        val c5=(Hearts,Ace)

        val cs1=[c1,c2,c3,c4,c5]
        val ms1=[Draw,Draw,Draw,Discard(c1)]
    in
        officiate_challenge(cs1,[Draw,Draw,Draw,Discard(c1)],24)::
        officiate_challenge(cs1,[Draw,Draw,Draw,Discard(c1)],13)::
        officiate_challenge(cs1,[Draw,Draw,Draw,Draw,Draw,Draw],70)::
        officiate_challenge(cs1,[],24)::
        []
    end;
(* [3,0,25,12] *)    
my_test_officiate_challenge();

fun my_test_careful_player()=
    let
        val c1=(Spades,Num(2))
        val c2=(Clubs,King)
        val c3=(Diamonds,Ace)
        val c4=(Spades,Ace)
        val c5=(Hearts,Ace)
    in
        careful_player([c1,c2,c3,c4,c5],15)::
        careful_player([c1,c2,c3,c4,c5],21)::
        []
    end;
my_test_careful_player();    





