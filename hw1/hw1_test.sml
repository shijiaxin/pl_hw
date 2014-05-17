(* test of is_older *)
val test_is_older=
    is_older((3,4,5),(3,4,4)) = false andalso
    is_older((3,4,5),(3,4,5)) = false andalso
    is_older((3,4,5),(3,4,6)) = true andalso
    is_older((3,4,5),(3,5,4)) = true andalso
    is_older((3,4,5),(5,4,3)) = true

val test_number_in_month=
    number_in_month([(1000,1,1),(1000,1,7),(1000,5,4)],1) = 2 andalso
    number_in_month([(1000,1,1),(1000,1,7),(1000,5,4)],5) = 1 andalso
    number_in_month([(1000,1,1),(1000,1,7),(1000,5,4)],2) = 0 

val test_number_in_months=
    number_in_months([(1000,1,1),(1000,1,7),(1000,5,4)],[1,5]) = 3 andalso
    number_in_months([(1000,1,1),(1000,1,7),(1000,5,4)],[4]) = 0 

val test_dates_in_month=
    dates_in_month([(1000,1,1),(1000,1,7),(1000,5,4)],1) = [(1000,1,1),(1000,1,7)] andalso
    dates_in_month([(1000,1,1),(1000,1,7),(1000,5,4)],3) = []

val test_dates_in_months=
    dates_in_months([(1000,1,1),(1000,1,7),(1000,5,4)],[1,5]) =[(1000,1,1),(1000,1,7),(1000,5,4)] andalso
    dates_in_months([(1000,1,1),(1000,1,7),(1000,5,4)],[3]) = []

val test_get_nth=
    get_nth(["a","b","c","d"],2)="b" andalso
    get_nth(["a","b","c","d"],4)="d" 

val test_date_to_string=
    date_to_string(2009,11,11)="November 11, 2009" andalso 
    date_to_string(2010,3,15) = "March 15, 2010"

val test_number_before_reaching_sum=
    number_before_reaching_sum(14,[1,2,3,4,5,6]) = 4 andalso 
    number_before_reaching_sum(15,[1,2,3,4,5,6]) = 4 andalso 
    number_before_reaching_sum(16,[1,2,3,4,5,6]) = 5 

val test_what_month=
    what_month(365)=12 andalso
    what_month(1)=1 andalso
    what_month(31)=1 andalso
    what_month(32)=2 andalso
    what_month(59)=2

val test_month_range=
    month_range(28,32)=[1,1,1,1,2] andalso
    month_range(47,47)=[2] andalso
    month_range(49,47)=[]

val test_oldest=
    oldest([])= NONE andalso
    oldest([(1000,1,1),(1000,1,7),(1000,5,4)])= SOME (1000,1,1)

val test_number_in_months_challenge=
    number_in_months_challenge([(1000,1,1),(1000,1,7),(1000,5,4)],[5,1,5,5,5]) = 3

val test_dates_in_months_challenge=
    dates_in_months_challenge([(1000,1,1),(1000,1,7),(1000,5,4)],[1,5,5]) =[(1000,1,1),(1000,1,7),(1000,5,4)]

val test_reasonable_date=
    reasonable_date(0-1,7,15) =false andalso
    reasonable_date(2001,2,28) =true andalso
    reasonable_date(2001,2,29) =false andalso
    reasonable_date(2001,3,32) =false andalso
    reasonable_date(2001,3,31) =true andalso
    reasonable_date(1996,2,29) =true andalso
    reasonable_date(2000,2,29) =true andalso
    reasonable_date(2100,2,29) =false  

   
