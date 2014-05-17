
fun is_older(A : int*int*int , B : int*int*int) =
	if(#1 A< #1 B) 
		then true
	else if(#1 A = #1 B andalso #2 A < #2 B) 
		then true
	else if(#1 A = #1 B andalso #2 A = #2 B andalso #3 A < #3 B) 
		then true
	else false;

fun number_in_month(dlist : (int*int*int) list , m : int) =
	if null dlist 
		then 0
	else if (#2( hd dlist) = m ) 
		then 1+number_in_month(tl(dlist),m)
	else number_in_month(tl(dlist),m);

fun number_in_months(dlist : (int*int*int) list , mlist : int list) =
	if null mlist 
		then 0
	else number_in_month(dlist , hd(mlist)) + 
		number_in_months(dlist,tl(mlist));

fun dates_in_month(dlist : (int*int*int) list , m : int) =
	if null dlist 
		then []
	else if (#2(hd dlist) = m) 
		then (hd dlist)::dates_in_month((tl dlist),m)
	else dates_in_month((tl dlist),m);


fun dates_in_months(dlist : (int*int*int) list , mlist : int list) =
	if null mlist 
		then []
	else (dates_in_month(dlist,hd(mlist)))@dates_in_months(dlist,tl(mlist));

fun get_nth(slist:string list,n:int)=
	if(n=1)
		then hd(slist)
	else get_nth(tl(slist),n-1);

fun date_to_string(day:int*int*int)=
	get_nth(["January","February","March", "April", "May", "June", 
		"July","August","September","October","November","December"]  ,#2 day)
		^" "^Int.toString(#3 day)^", "^Int.toString(#1 day);

(* val t= date_to_string(2014,3,1);  *)

fun number_before_reaching_sum (sum : int ,mlist: int list)=
	if null mlist
		then 0
	else if(hd(mlist) >= sum)
		then 0
	else 1+number_before_reaching_sum(sum-hd(mlist),tl(mlist));

(*1<=day<=365*)
fun what_month(day : int)=
	1+number_before_reaching_sum(day,[31,28,31,30,31,30,31,31,30,31,30,31]);

fun month_range(day1 : int , day2 : int)=
	if (day1>day2)
		then []
	else what_month(day1)::month_range(day1+1,day2);	

fun oldest(dlist :(int*int*int) list)=
	if null dlist
		then NONE
	else
		let val tl_ans = oldest(tl(dlist))
		in
			if isSome tl_ans andalso is_older(valOf tl_ans,hd(dlist))
				then tl_ans
			else SOME(hd(dlist)) 
		end ;
fun remove_dup(mlist:int list)=
	let fun exist(d:int,int_list:int list)=
		if null int_list
			then false
		else if hd(int_list)=d
			then true
		else exist(d,tl(int_list))	
	in
		if null mlist
			then mlist
		else if exist(hd(mlist),tl(mlist))
			then remove_dup(tl(mlist))
		else hd(mlist)::remove_dup(tl(mlist))
	end ;

fun number_in_months_challenge(dlist : (int*int*int) list , mlist : int list) =
	number_in_months(dlist,remove_dup(mlist));

fun dates_in_months_challenge(dlist : (int*int*int) list , mlist : int list) =
	dates_in_months(dlist,remove_dup(mlist));

fun reasonable_date(day:int*int*int)=
	let 
		val is_leapyear=
			if (#1 day mod 4 = 0 andalso #1 day mod 100 <>0) 
				then true 
			else if (#1 day mod 400 = 0 )
				then true
			else false
		fun get_nint(int_list:int list,n:int)=
			if(n=1)
				then hd(int_list)
			else get_nint(tl(int_list),n-1)
	in
		if(#1 day<1 )
			then false
		else if (#2 day<1 orelse #2 day>12)
			then false
		else if (#3 day <1)
			then false
		else if (is_leapyear andalso #3 day > get_nint([31,29,31,30,31,30,31,31,30,31,30,31],#2 day))
			then false
		else if ((not is_leapyear) andalso #3 day > get_nint([31,28,31,30,31,30,31,31,30,31,30,31],#2 day))
			then false
		else true
	end ;
