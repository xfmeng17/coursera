fun is_older (day1 : int*int*int, day2 : int*int*int) =
    if (#1 day1) < (#1 day2)
    then true
    else if (((#2 day1) < (#2 day2)) andalso ((#1 day1) = (#1 day2)))
    then true
    else if (((#3 day1) < (#3 day2)) andalso ((#2 day1) = (#2 day2)) andalso ((#1 day1) = (#1 day2)))
    then true
    else false

fun number_in_month  (day_list: (int*int*int) list, month : int) =
    if null day_list
    then 0
    else if #2(hd day_list) = month then 1 + number_in_month(tl day_list, month)
    else number_in_month(tl day_list, month)

fun number_in_months (day_list: (int*int*int) list, month_list: int list) =
    if null month_list
    then 0
    else number_in_month(day_list, hd month_list) + number_in_months(day_list, tl month_list)

fun dates_in_month  (day_list: (int*int*int) list, month : int) =
    if null day_list
    then []
    else if #2(hd day_list) = month then (hd day_list)::dates_in_month(tl day_list, month)
    else dates_in_month(tl day_list, month)
    
fun dates_in_months  (day_list: (int*int*int) list, month_list : int list) =
    if null month_list
    then []
    else dates_in_month(day_list, (hd month_list))@dates_in_months(day_list, (tl month_list))

fun get_nth (string_list: string list, n: int) =
    if n = 1 then (hd string_list)
    else if (tl string_list) = [] then ""
    else get_nth(tl string_list, n-1)
 
fun date_to_string(day: int*int*int) =
    let val month = ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]
    in
     get_nth(month, #3 day) ^ " " ^ Int.toString(#2 day)^ ", " ^ Int.toString(#1 day)
    end

 
fun number_before_reaching_sum (sum: int, num: int list) =
    let
     fun nth_sum(index: int, sum: int, num: int list) =
         if null num then index
         else if (sum - (hd num)) < 0 then index + 1
            else nth_sum(index +1, sum - (hd num), tl num)
    in
     nth_sum(0, sum, num)
    end

fun what_month(day: int) =
    let val day_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
     number_before_reaching_sum(day - 1, day_of_month)
    end


fun month_range(day: (int*int)) =
    if (#1 day) > (#2 day) then []
    else if (#1 day) = (#2 day) then [what_month(#1 day)]
    else what_month(#1 day):: month_range((#1 day) + 1, (#2 day))


fun oldest(day_list: (int*int*int) list)=
    if null day_list then NONE
    else let
     fun oldest_nonempty(day_list: (int*int*int) list) =
         if null (tl day_list) then hd day_list
         else let val older = oldest_nonempty(tl day_list)
          in
               if is_older(hd day_list, older) then hd day_list
                     else older
              end
        in
         SOME (oldest_nonempty day_list)
    end

fun is_older (day1 : int*int*int, day2 : int*int*int) =
    if (#1 day1) < (#1 day2)
    then true
    else if (#2 day1) < (#2 day2) andalso  (#1 day1) = (#1 day2)
    then true
    else if (#3 day1) < (#3 day2) andalso (#2 day1) = (#2 day2) andalso (#3 day1) = (#3 day2) 
    then true
    else false

fun number_in_month  (day_list: (int*int*int) list, month : int) =
    if null day_list
    then 0
    else if #2(hd day_list) = month then 1 + number_in_month(tl day_list, month)
    else number_in_month(tl day_list, month)

fun number_in_months (day_list: (int*int*int) list, month_list: int list) =
    if null month_list
    then 0
    else number_in_month(day_list, hd month_list) + number_in_months(day_list, tl month_list)

fun dates_in_month  (day_list: (int*int*int) list, month : int) =
    if null day_list
    then []
    else if #2(hd day_list) = month then (hd day_list)::dates_in_month(tl day_list, month)
    else dates_in_month(tl day_list, month)
    
fun dates_in_months  (day_list: (int*int*int) list, month_list : int list) =
    if null month_list
    then []
    else dates_in_month(day_list, (hd month_list))@dates_in_months(day_list, (tl month_list))

fun get_nth (string_list: string list, n: int) =
    if n = 1 then (hd string_list)
    else if (tl string_list) = [] then ""
    else get_nth(tl string_list, n-1)
 
fun date_to_string(day: int*int*int) =
    let val month = ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]
    in
     get_nth(month, #2 day) ^ " " ^ Int.toString(#3 day)^ ", " ^ Int.toString(#1 day)
    end
 
fun number_before_reaching_sum (sum: int, num: int list) =
    let
     fun nth_sum(index: int, sum: int, num: int list) =
         if null num then index
         else if (sum - (hd num)) <= 0 then index
            else nth_sum(index +1, sum - (hd num), tl num)
    in
     nth_sum(0, sum, num)
    end

fun what_month(day: int) =
    let val day_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
     number_before_reaching_sum(day, day_of_month) + 1
    end

fun month_range(day: (int*int)) =
    if (#1 day) > (#2 day) then []
    else if (#1 day) = (#2 day) then [what_month(#1 day)]
    else what_month(#1 day):: month_range((#1 day) + 1, (#2 day))

fun oldest(day_list: (int*int*int) list)=
    if null day_list then NONE
    else let
     fun oldest_nonempty(day_list: (int*int*int) list) =
         if null (tl day_list) then hd day_list
         else let val older = oldest_nonempty(tl day_list)
          in
               if is_older(hd day_list, older) then hd day_list
                     else older
              end
        in
         SOME (oldest_nonempty day_list)
    end
