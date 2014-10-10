
fun count_days_in_months (months : int) =
    if months > 2
    then
        (31 + 28) + (months - 2) * 30 + (months div 2)
    else
	if months = 1
	then 31
	else 31 + 28

fun is_older(date1 : int*int*int, date2 : int*int*int) = 
    let
	val date1_reformated = (#1 date1 * 365) + (count_days_in_months (#2 date1)) + #3 date1;
	val date2_reformated = (#1 date2 * 365) + (count_days_in_months (#2 date2)) + #3 date2;
    in
	if date1_reformated >= date2_reformated
	then false
	else true
    end 
    
(*fun number_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then 0
    else 
	if #2 (hd dates) = month
	then 
*)
