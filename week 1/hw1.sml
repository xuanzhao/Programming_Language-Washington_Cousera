(* int*int*int -> year, month, day *)
fun is_older (pr1 : int * int * int, pr2 : int * int * int) = 
	#1 pr1 < #1 pr2
	orelse (#1 pr1 = #1 pr2 andalso #2 pr1 < #2 pr2)
	orelse (#1 pr1 = #1 pr2 andalso #2 pr1 = #2 pr2 andalso #3 pr1 < #3 pr2)


(* (int*int*int) list, int -> int *)
fun number_in_month (xl : (int * int * int) list , month :int) = 
	if null xl
	then 0
	else 
		let 
		     val rest_dates = number_in_month( tl xl, month)
		 in 
		 	 if #2 (hd xl) = month
		 	 then 1 + rest_dates
		 	 else rest_dates
		 end

(* (int*int*int) list, int list -> int *)
fun number_in_months (xl : (int * int * int) list, months : int list) = 
	if null months
	then 0
	else number_in_month(xl, hd months) + number_in_months(xl, tl months)

(* (int*int*int) list, int -> (int*int*int) list *)
fun dates_in_month (xl: (int * int * int) list, month: int) = 
	if null xl
	then []
	else 
		if #2 (hd xl) = month 
		then hd xl :: dates_in_month (tl xl, month)
		else dates_in_month (tl xl, month)

fun dates_in_months (xl: (int * int * int) list, months : int list) = 
	if null months
	then []
	else 
		dates_in_month (xl, hd months) @ dates_in_months(xl, tl months)


(* string list, int -> string *)
fun get_nth (xl : string list, n : int) = 
	if n = 1
	then hd xl
	else get_nth (tl xl, n-1)

(* (int*int*int) -> string *)
fun date_to_string (pr: (int * int * int)) = 
	let 
		val months = ["January", "February", "March", "April", "May", "June", 
					  "July", "August", "September", "October", "November", "December"]
	in 
		get_nth(months, #2 pr) ^ " " ^ Int.toString(#3 pr ) ^ ", " ^ Int.toString(#1 pr) 
	end

(* int list, int -> int *)
fun number_before_reaching_sum (sum: int , int_list : int list) = 
	if hd int_list >= sum
	then 0
	else 1 + number_before_reaching_sum (sum - hd int_list, tl int_list)

(* int -> string *)
fun what_month (day : int) = 
	let 
		val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		1 + number_before_reaching_sum (day, days_in_month)
	end

(* int, int -> int list *)
fun month_range (day1: int, day2: int) = 

	if day1 > day2
	then []
	else what_month(day1) :: month_range(day1+1, day2)



fun oldest (xl : (int * int * int) list) = 
	if null xl
	then NONE
	else 
		let val rest_dates = oldest (tl xl)
		in 
			if isSome rest_dates andalso is_older(valOf rest_dates, hd xl)
			then rest_dates
			else SOME (hd xl)
		end

fun sort_list (xl: int list) = 
	let 
		fun merge (x: int, xl: int list) = 
			if null xl
			then x :: []
			else 
				if x < hd xl 
				then x :: xl 
				else hd xl :: merge(x, tl xl)
	in 
		if null xl
		then []
		else if null (tl xl) 
			 then xl
			 else merge(hd xl, sort_list(tl xl))
	end 

fun rm_dup (xl: int list) = 
	let val sorted_xl = sort_list(xl)
	in
		if null sorted_xl
		then []
		else 
			if null (tl sorted_xl)
			then sorted_xl
			else 
				let val rest_xl = rm_dup (tl sorted_xl)
				in 
					if hd sorted_xl = hd rest_xl 
					then rest_xl
					else hd sorted_xl :: rest_xl
				end
	end


fun number_in_months_challenge (xl: (int*int*int) list, months: int list) =
	number_in_months(xl, rm_dup(months)) 

fun dates_in_months_challenge(xl: (int*int*int) list, months: int list) =
	dates_in_months(xl, rm_dup(months)) 



fun reasonable_date (pr: (int * int * int)) =
	let 
		fun is_leap_year (year: int) = 
			(year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        val leap_day_limit = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val normal_day_limit = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun get_day (xl: int list, month: int) = 
        	if month = 1
        	then hd xl
        	else get_day(tl xl, month-1)
	in 
		if (#1 pr < 1) orelse ((#2 pr < 1) orelse (#2 pr > 12)) orelse
			(#3 pr < 1 orelse #3 pr > 31)
		then false
		else 
			if is_leap_year(#1 pr)
			then #3 pr = get_day(leap_day_limit, #2 pr)
			else #3 pr = get_day(normal_day_limit, #2 pr) 
	end













