(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, r) =
    case r of
        []      =>  NONE
    |   h::tl   =>  if(same_string(s,h)) 
                        then SOME tl
                    else case all_except_option(s,tl) of
                                NONE       =>  NONE
                            |   SOME lst   =>  SOME (h::lst);
                        

fun get_substitutions1(sll,s)=
    case sll of
        []     => []
    |   sl::tl =>  case all_except_option(s,sl) of
                        NONE       =>  get_substitutions1(tl,s)
                    |   SOME lst   =>  lst@get_substitutions1(tl,s);


fun get_substitutions2(sll,s)=
    let fun tail_rec(sll_rec,acc)=
        case sll_rec of
            []     =>  acc
        |   sl::tl_rec =>  case all_except_option(s,sl) of
                                NONE       =>tail_rec(tl_rec,acc)
                            |   SOME lst   =>tail_rec(tl_rec,acc@lst)
    in
        tail_rec(sll,[])
    end;


fun similar_names(sll,fullname)=
    let
        val {first=fname,middle=mname,last=lname} = fullname
        fun helper(sl)=
            case sl of
                []     =>[]
            |   s::tl  =>{first = s ,middle= mname , last= lname }::helper(tl)
    in
        fullname::helper(get_substitutions1(sll,fname))
    end;



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c)=
    let
        val (s,r)=c
    in
    case s of
        Clubs       =>Black
    |   Spades     =>Black
    |   Diamonds   =>Red
    |   Hearts    =>Red
    end;

fun card_value(c)=
    let
        val (s,r)=c
    in
    case r of
        Num N       => N
    |   Ace        => 11
    |   _          => 10
    end;

fun remove_card(cs, c, e)=
    case cs of
        []          =>  raise e
    |   ch::tl      =>  if(ch = c) then tl
                        else ch::remove_card(tl,c,e);

fun all_same_color(cs)=
    case cs of 
        ch1::(ch2::tl)     =>(card_color(ch1) = card_color(ch2)) andalso all_same_color(ch2::tl)
    |   _=>true;


fun sum_cards(cs)=
    let fun sum_cards_rec(clist,sum)=
        case clist of
            []      =>sum
        |   ch::tl =>sum_cards_rec(tl,card_value(ch)+sum)
    in
        sum_cards_rec(cs,0)
    end ;


fun score(cs:card list,goal:int)=
    let 
        val sum=sum_cards(cs)
        val preliminary=if(goal > sum) then (goal-sum)
                        else    3*(sum-goal)
    in
        if(all_same_color(cs)) then preliminary div 2
        else    preliminary
    end ;


fun officiate(cs:card list,ms:move list,goal:int)=
    let fun helper(clist,hlist,mlist,sum)=
        case mlist of
            []          =>  hlist
        |   Draw::m_tl1  => (   case clist of
                                    []          =>  hlist
                                |   ch::c_tl   =>  if(card_value(ch)+sum > goal) then ch::hlist
                                                else  helper(c_tl,ch::hlist,m_tl1,card_value(ch)+sum) )
        |   Discard(c)::m_tl2  =>helper(clist,remove_card(hlist,c,IllegalMove),m_tl2,sum-card_value(c))
    in
        score(helper(cs,[],ms,0),goal)
    end;



fun card_value_min(c:card)=
    let
        val (s,r)=c
    in
    case r of
        Num N       => N
    |   Ace        => 1
    |   _          => 10
    end;
fun sum_cards_min(cs:card list)=
    let fun sum_cards_rec(clist:card list,sum:int)=
        case clist of
            []      =>sum
        |   ch::tl =>sum_cards_rec(tl,card_value_min(ch)+sum)
    in
        sum_cards_rec(cs,0)
    end ;
fun score_challenge(cs:card list,goal:int)=
    let 
        val max_score=sum_cards(cs)
        val min_score=sum_cards_min(cs)
        val preliminary=if(goal > max_score) 
                            then (goal-max_score)
                        else if(goal<min_score)
                            then 3*(min_score-goal)
                        else 0
    in
        if(all_same_color(cs)) 
            then preliminary div 2
        else    preliminary
    end ;

fun officiate_challenge(cs:card list,ms:move list,goal:int)=
    let fun helper(clist,hlist,mlist,sum)=
        case mlist of
            []           =>  hlist
        |   Draw::m_tl1  => (   case clist of
                                    []         =>   hlist
                                |   ch::c_tl   =>   if(card_value_min(ch)+sum > goal) then ch::hlist
                                                    else  helper(c_tl,ch::hlist,m_tl1,card_value_min(ch)+sum) )
        |   Discard(c)::m_tl2  =>   helper(clist,remove_card(hlist,c,IllegalMove),m_tl2,sum-card_value_min(c))
    in
        score_challenge(helper(cs,[],ms,0),goal)
    end;

fun careful_player(cs:card list,goal:int)=    
    let fun exist_val(clist:card list, v:int)=
            case clist of
                []      =>  NONE
            |   c::tl   =>  if(card_value(c)=v) then SOME (c)
                            else exist_val(tl,v)
        fun careful_helper(cs:card list,hs:card list,goal:int)=
            case cs of
                    []      =>  []
                |   c::tl   =>  if(goal = 0)
                                    then []
                                else if(goal > 10)
                                    then Draw::careful_helper(tl,c::hs,goal-card_value(c))
                                else case exist_val(hs,card_value(c)-goal) of
                                        NONE            =>[]
                                    |   SOME discard_c  =>Discard(discard_c)::Draw::[]
    in
        careful_helper(cs,[],goal)
    end;






