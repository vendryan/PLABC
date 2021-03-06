(*Homework week 2/homework 1*)

val MONTH = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
val DAY_EVERY_MONTH = [31, 28, 31, 30, 31, 30 ,31, 31, 30, 31, 30, 31];

(*Check if the date is valid*)
(*fun valid_date(date : int * int * int) =
    if #1 date >= 0 andalso
       (#2 date >= 1 andalso #2 date <= 12) andalso
       (#3 date >= 1 andalso #3 date <= 31)
    then true
    else false*)

(* 1 *)
fun is_older(d1 : int * int * int, d2 : int * int * int) =
    if #1 d1 > #1 d2
    then false
    else if #1 d1 < #1 d2
    then true
    else if #2 d1 > #2 d2
    then false
    else if #2 d1 < #2 d2 
    then true
    else if #3 d1 > #3 d2 
    then false
    else if #3 d1 < #3 d2
    then true      
    else false

(* 2 *)
fun number_in_month(ds : (int * int * int) list, m : int) =
    if null ds
    then 0
    else if #2 (hd ds) = m
    then 1 + number_in_month(tl ds, m)
    else number_in_month(tl ds, m)

(* 3 *)	     
fun number_in_months(ds : (int * int * int) list, ml :  int list) =
    if null ml
    then 0
    else number_in_month(ds, hd ml) + number_in_months(ds, tl ml)

(* 4 *)
fun dates_in_month(ds : (int * int * int) list, m : int) =
    if null ds
    then []
    else if #2 (hd ds) = m
    then hd ds :: dates_in_month(tl ds, m)
    else dates_in_month(tl ds, m)
		       
fun dates_in_months(ds : (int * int * int) list, ml : int list) =
    if null ml
    then []
    else dates_in_month(ds, hd ml) @ dates_in_months(ds, tl ml)

fun get_nth(string_list : string list, num : int) =
    if num = 1
    then hd string_list
    else get_nth(tl string_list, num - 1)	    

(*Int.toString 7 *)
fun date_to_string(date : int * int * int) =
    get_nth(MONTH, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)

fun number_before_reaching_sum(sum : int, int_list : int list) =
    let
	val first_item = hd int_list
	fun helper(x : int list, total : int, count : int) =
	    if total < sum
	    then helper(tl x, total + hd (tl x), count + 1)
	    else count
    in
	helper(int_list, first_item, 0)
    end

fun what_month(num : int) =
    number_before_reaching_sum(num, DAY_EVERY_MONTH) + 1

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(date_list : (int * int * int) list) =
    if null date_list
    then NONE
    else
	let fun max(date_list :(int * int * int) list, oldest : (int * int * int)) =
		if null (tl date_list)
		then oldest
		else if is_older(oldest, hd (tl date_list))
		then max(tl date_list, oldest)
		else max(tl date_list, hd (tl date_list))
	in
	    SOME (max(date_list, hd date_list))
	end

fun is_inside(num_list : int list, num : int) =
    if null num_list
    then false
    else (hd num_list = num) orelse is_inside(tl num_list, num)
	
fun number_in_months_challenge(date : (int * int * int) list, ml : int list) =
    let fun remove_duplicate(ml : int list) =
	    if null (tl ml)
	    then hd ml::[]
	    else let val tail = remove_duplicate(tl ml)
		 in if not(is_inside(tail, hd ml))
		    then hd ml::tail
		    else tail
		 end
	val list = remove_duplicate(ml)
    in  number_in_months(date, list)
    end

fun dates_in_months_challenge(date : (int * int * int) list, ml : int list) =
    let fun remove_duplicate(ml : int list) =
	    if null (tl ml)
	    then hd ml::[]
	    else let val tail = remove_duplicate(tl ml)
		 in if not(is_inside(tail, hd ml))
		    then hd ml::tail
		    else tail
		 end
	val list = remove_duplicate(ml)
    in  dates_in_months(date, list)
    end

fun reasonable_date(date : int * int * int) =
    let val year = #1 date
	val month = #2 date
	val day = #3 date
	val LEAP_YEAR_DATE = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val NO_LEAP_DATE = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	fun day_in_month(day_list : int list, m : int) =
	    if m = 1
	    then hd day_list
	    else day_in_month(tl day_list, m - 1)
    in if year <= 0 orelse not (month >= 1 andalso month <= 12)
       then false
       else if year mod 4 = 0 andalso (not (year mod 100 = 0) orelse year mod 400 = 0) (*Leap year*)
       then if day >= 1 andalso day <= (day_in_month(LEAP_YEAR_DATE, month)) then true else false
       else if day >= 1 andalso day <= (day_in_month(NO_LEAP_DATE, month)) then true else false
    end
