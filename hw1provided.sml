
fun is_older(date1 : int*int*int, date2 : int*int*int) =
    let
	fun count_days_in_months (months : int) =
	    if months > 2
	    then (31 + 28) + (months - 2) * 30 + (months div 2)
	    else
		if months = 1
		then 31
		else 31 + 28
    in
	let
	    val date1_reformated = (#1 date1 * 365) + (count_days_in_months (#2 date1)) + #3 date1;
	    val date2_reformated = (#1 date2 * 365) + (count_days_in_months (#2 date2)) + #3 date2;
	in
	    date1_reformated <= date2_reformated
	end 
    end
    
fun number_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then 0
    else
	let 
	    val ans1 = if #2 (hd dates) = month then 1 else 0
	    fun count_next (dates : (int*int*int) list, count : int) = 
		let 
		    val ans2 = if #2 (hd dates) = month then count + 1 else count + 0
		in
		    if null(tl dates)
		    then ans2
		    else count_next(tl dates, ans2)
		end
	in
	    if null(tl dates)
	    then ans1
	    else count_next(tl dates, ans1)
	end
    
fun number_in_months (dates : (int*int*int) list, months : int list) = 
    if null dates orelse null months
    then 0
    else
	let
	    val ans1 = number_in_month(dates, hd months);
            fun count_next(months1 : int list, count : int) = 
		let
		    val ans2 = number_in_month(dates, hd months1) + count;
		in 
		    if null(tl months1)
		    then ans2
		    else count_next(tl months1, ans2)
		end
	in
	    if null(tl months)
	    then ans1
	    else count_next(tl months, ans1)
	end

fun dates_in_month(dates : (int*int*int) list, month : int) = 
    if null dates orelse month = 0
    then []
    else
	let 
	    val ans1 = if (#2 (hd dates)) = month then [hd dates] else []
	    fun count_next (dates1 : (int*int*int) list, ready_dates : (int*int*int) list) = 
		let
		    val ans2 = if (#2 (hd dates1)) = month then (hd dates1)::ready_dates else ready_dates
		in
		    if null(tl dates1)
		    then ans2
		    else count_next(tl dates1, ans2)
		end
        in
	    if null(tl dates)
	    then ans1
	    else count_next(tl dates, ans1)
	end


fun dates_in_months(dates : (int*int*int) list, months : int list) = 
    if null dates orelse null months
    then []
    else
	let 
	    val ans1 = dates_in_month(dates, hd months);
	    fun count_next(months1 : int list, ready_dates : (int*int*int) list) = 
		let
		    val ans2 = ready_dates @ dates_in_month(dates, hd months1);
		in
		    if null(tl months1)
		    then ans2
		    else count_next(tl months1, ans2)
		end
	in
	    if null(tl dates)
	    then ans1
	    else count_next(tl months, ans1)
	end

fun get_nth(words : string list, index : int) = 
    if null words orelse index <= 0
    then ""
    else
	let 
	    val i = 1
	    fun get_next_word(not_checked_words : string list, i1 : int) = 
		if i1 < index
		then
		    if null(tl not_checked_words)
		    then ""
		    else get_next_word(tl not_checked_words, i1+1)
		else hd not_checked_words
	in
	    get_next_word(words, 1)
	end

fun date_to_string(date : int*int*int) = 
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
    end

fun number_before_reaching_sum(sum : int, numbers : int list) = 
    if sum = 0 orelse null numbers
    then 0
    else
	let
	    val ans1 = hd numbers
	    fun count_next(numbers1, sum1, reached_number) = 
		let
		    val ans = (hd numbers1) + sum1
		in
		    if ans >= sum
		    then reached_number
		    else count_next(tl numbers1, ans, reached_number + 1)
		end
	in
	    if ans1 >= sum
	    then 1
	    else count_next(tl numbers, hd numbers, 1)
	end

fun what_month(days : int) = 
    if days = 0
    then 0
    else
	let
	    val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
	    if days <= hd months
	    then 1
	    else number_before_reaching_sum(days, months) + 1
	end
	
fun list_reverse (list1 : int list) = 
    if null(tl list1)
    then []
    else
	let
	    fun test(not_reversed : int list, reversed : int list) = 
		let
		    val ans1 = (hd not_reversed) :: reversed
		in
		    if null(tl not_reversed)
		    then ans1
		    else test(tl not_reversed, ans1)
		end
	in
	    test(list1, [])
	end

fun month_range(day1 : int, day2 : int) = 
    if day2 < day1
    then []
    else
	let 
	    val ans1 = [what_month(day1)]
	    fun count_next(next_day : int, ready_days : int list) =
		let
		    val ans2 = what_month next_day :: ready_days
		in
		    if next_day = day2
		    then ans2
		    else count_next(next_day+1, ans2)
		end
	in
	    if day1 = day2
	    then ans1
	    else list_reverse(count_next(day1+1, ans1))
	end
		
fun oldest(dates : (int*int*int) list) = 
    if null dates
    then NONE
    else
	let
	    val first_date = hd dates
	    fun count_next(dates1 : (int*int*int) list, date : int*int*int) = 
		let
		    val older_date = if is_older(date, hd dates1) then hd dates1 else date
		in
		    if null(tl dates1)
		    then older_date
		    else count_next(tl dates1, older_date)
		end
	in
	    if null(tl dates)
	    then SOME first_date
	    else SOME(count_next(tl dates, first_date))
	end
