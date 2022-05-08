(*date structure : (year, month, day)*)
datatype date = date of int * int * int


(*task 1*)
fun is_older (date(a1,b1,c1) : date, date(a2,b2,c2) : date) =
    if a1 < a2 then true
    else if a1 > a2 then false
    else if b1 < b2 then true
    else if b1 > b2 then false
    else if c1 < c2 then true
    else if c1 > c2 then false
    else false;

(*task 2*)
fun number_in_month([], _) = 0
  | number_in_month(date(a1, b1, c1)::dates_list, m) =
    if b1 = m then 1 + number_in_month(dates_list, m)
    else number_in_month(dates_list, m)

(*task 3*)
fun number_in_months(dates_list, []) = 0
  | number_in_months(dates_list, m1::ms) =
    number_in_month(dates_list, m1) + number_in_months(dates_list, ms)

(*task 4*)
fun dates_in_month([], _) = []
  | dates_in_month(date(a, b, c)::dates_list, m) : date list = 
    if b = m then date(a, b, c)::dates_in_month(dates_list, m)
    else dates_in_month(dates_list, m)

(*task 5*)
fun dates_in_months(dates_list, []) = []
  | dates_in_months(dates_list, m::ms) : date list = 
    dates_in_month(dates_list, m) @ dates_in_months(dates_list, ms)

(*task 6*)
fun get_nth([], i : int) = ""
  | get_nth(s::strings : string list, 0) = s
  | get_nth(s::strings : string list, i : int) = get_nth(strings, i-1)

(*task 7*)
fun date_to_string(date(y, m, d) : date) : string =
    let val months_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months_list, m-1) ^ ", " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
    end

(*task 8*)
fun number_before_reaching_sum(0, _) : int = 0
  | number_before_reaching_sum(_, []) : int = 0
  | number_before_reaching_sum(sum : int, first::l::last : int list) : int =
    if sum < 0 then 0
    else if sum < first then 0 
    else 1 + number_before_reaching_sum(sum - first, last)

(*task 9*)
fun what_month(day : int) : int =
    let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      1 + number_before_reaching_sum(day, months)
    end

(*task 10*)
fun month_range_util(day1 : int, day2 : int) : int list =
    let
      val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      val m1 = what_month(day1)
      val m2 = what_month(day2)
    in
      if m1 > m2 then []
      else if m1 = m2 then [m1]
      else [m1, m2]
    end


fun for_loop_util(m1 : int, m2 : int) : int list =
    if m1 > m2 then []
    else m1::for_loop_util(m1+1, m2)


fun month_range(day1 : int, day2 : int) = 
    let
      val range = month_range_util(day1, day2)
    in
      if range = [] then []
      else for_loop_util(List.nth(range, 0), List.nth(range, 1))
    end


(*task 11*)
fun oldest_date([]) = date(0, 0, 0)
  | oldest_date(date(a1, b1, c1)::dates : date list) =
    if is_older(oldest_date(dates), date(a1, b1, c1)) then date(a1, b1, c1)
    else oldest_date(dates)


(*==================ALL_TEST_HERE==================*)

(*test 1*)
val _ = print( Bool.toString( is_older (date(2000, 11, 1), date(2000, 12, 1)) ) ^ "\n" );

(*test 2*)
val test2_list = [date(2000, 11, 1), date(2000, 12, 1), date(2000, 11, 2)];
val _ = print( Int.toString(number_in_month (test2_list, 11)) ^ " monthes were found\n" );

(*test 3*)
val _ = print( Int.toString(number_in_months (test2_list, [11, 12])) ^ " monthes were found\n" );

(*test 4*)
val found_dates = dates_in_month (test2_list, 11)

(*test 5*)
val found_dates = dates_in_months (test2_list, [11, 12])

(*test 6*)
val nth_elem = get_nth(["1", "2", "3"], 1)

(*test 7*)
val stringified_date = date_to_string(date(2000, 2, 11))

(*test 8*)
val t8_list = [30, 30, 30, 30, 30, 30]
val t8_sum = 64
val t8_res = number_before_reaching_sum(t8_sum, t8_list)

(*test 9*)
val t9_res = what_month(33)
(*should return 2*)

(*test 10*)
val test10_res = month_range(30, 33)
(*should return [1, 2]*)
val test10_res1 = month_range(30, 67)
(*should return [1, 2, 3]*)

(*test 11*)
val test11_l = [date(2000, 11, 1), date(2000, 12, 1), date(2001, 1, 1), date(2000, 11, 2)]
val test11_res = oldest_date(test11_l)
(*should return date(2000, 11, 1)*)

