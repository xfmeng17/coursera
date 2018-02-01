
fun is_older (date1 : int * int * int, date2 : int * int * int) =
  if #1 date1 < #1 date2
  then true
  else if #1 date1 > #1 date2
  then false
  else if #2 date1 < #2 date2
  then true
  else if #2 date1 > #2 date2
  then false
  else if #3 date1 < #3 date2
  then true
  else if #3 date1 > #3 date2
  then false
  else false

fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month (tl dates, month)
  else number_in_month (tl dates, month)

fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if #2 (hd dates) = month
  then hd dates :: dates_in_month (tl dates, month)
  else dates_in_month (tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)
		      
fun get_nth (strs : string list, n : int) =
  if n = 1
  then hd strs
  else get_nth (tl strs, n - 1)

fun date_to_string (date : int * int * int) =
  let val months = ["January", "February", "March", "April", "May", "June",
		    "July", "August", "September", "October", "November", "December"]
  in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum : int, xs : int list) =
  if sum <= hd xs
  then 0
  else 1 + number_before_reaching_sum (sum - (hd xs), tl xs)

fun what_month (day : int) =
  let val days_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in 1 + number_before_reaching_sum (day, days_of_month) 
  end

fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month (day1) :: month_range (day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
  if null dates
  then NONE
  else let fun oldest_nonempty (dates : (int * int * int) list) =
	     if null (tl dates)
	     then hd dates
	     else let val tl_ans = oldest_nonempty (tl dates)
		  in if is_older (hd dates, tl_ans)
		     then hd dates
		     else tl_ans
		  end
       in SOME (oldest_nonempty dates)
       end

fun is_exists (x : int, xs : int list) =
  if null xs
  then false
  else if x = hd xs
  then true
  else is_exists (x, tl xs)    

fun remove_duplicates (xs : int list) =
  if null xs
  then []
  else if is_exists (hd xs, tl xs)
  then remove_duplicates (tl xs)
  else hd xs :: remove_duplicates (tl xs)
				  
fun number_in_months_challenge (date : (int * int * int) list, months : int list) =
  number_in_months (date, (remove_duplicates months))  
fun reasonable_date (date : int * int * int) =
  let val year = #1 date
      val month = #2 date
      val day = #3 date
  in if year < 1
     then false
     else if month < 1 orelse month > 12
     then false
     else let val max_day =
		  let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		  in let fun get_days (days : int list, n : int) =
			   if n = 1
			   then hd days
			   else get_days (tl days, n - 1)
		     in let fun is_leap_year (year : int) =
			      if (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)
			      then true
			      else false
			in if month = 2 andalso is_leap_year (year)
			   then 1 + get_days (days, month)
			   else get_days (days, month)
			end
		     end
		  end		  
	  in if day < 1 orelse day > max_day
	     then false
	     else true
	  end
  end
