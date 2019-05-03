(*extra practice problems*)

(* 1. 
   int list -> int
   returns a number that is the product of adding the numbers from the given list with alternating sign (+,-)
 *)

fun alternate (loi : int list) =
    let
	fun even (n : int) =
	    (n mod 2) = 0
	fun helper (loi0: int list, prod : int, acc : int) =
	    if null loi0
	    then prod
	    else if even acc
	    then helper (tl loi0,prod + hd loi0,acc + 1)
	    else helper (tl loi0,prod - hd loi0,acc + 1)
    in
	helper (loi,0,0)
    end
	
(* 2.	       
   int list -> int * int
   return a pair of numbers containing the smallest and the greatest numbers from the given list
 *)

fun min_max (loi : int list) =
    let
	fun min (loi0 : int list, acc0 : int) =
	    if null loi0
	    then acc0
	    else if acc0 > (hd loi0)
	    then min (tl loi0, hd loi0)
	    else min (tl loi0, acc0)

	fun max (loi1 : int list, acc1 : int) =
	    if null loi1
	    then acc1
	    else if acc1 < (hd loi1)
	    then max (tl loi1, hd loi1)
	    else max (tl loi1, acc1)
    in
	(min (tl loi, hd loi), max (tl loi, hd loi))
    end
	
(* 3.
   int list -> int list
   return a list of the partial sums of the numbers from the given list
 *)

(*
fun cumsum (loi : int list) =
    let
	fun count_and_add (loi0 : int list, counter :int) =
	    if counter = 0
	    then 0
	    else hd loi0 + count_and_add (tl loi0, counter - 1)
			   
	fun helper (loi0 : int list, counter : int, result : int list) =
	    if null loi0
	    then result
	    else helper (tl loi0, counter + 1, result @ [count_and_add (loi, counter)])
				 
    in
	helper(loi,1,[])
    end
*)

fun cumsum (loi : int list) =
    let
	fun get_nth (loi0 : int list, n : int) =
		    if null loi0
		    then 0
		    else if n = 1
		    then (hd loi0)
		    else get_nth (tl loi0,n-1)
				 
	fun helper (loi0 : int list, counter : int, sum_so_far : int, result : int list) =
	    if null loi0
	    then result
	    else helper (tl loi0, counter+1, sum_so_far + get_nth (loi, counter), result @ [sum_so_far])
    in
	helper (loi, 2, hd loi, [])
    end
	
    
(* val test1 = cumsum ([1,4,20]) *)	    

(* 4.
   string option -> string
   return "Hello there, ...!" replacing ... by the given string option: SOME "name", and "you" if given NONE 
 *)

fun greeting (str : string option) =  (* enter (SOME "name") *)
    if isSome str
    then "Hello there, " ^ valOf str ^"!"
    else "Hello there, you!"

(* 5. 
   return list repeating the numbers in the first list according to the numbers in the second list
 *)

fun repeat (loi1 : int list, loi2 : int list) =
    let
	fun partial_result (n1 : int, n2 : int, partial : int list) =
	    if n2 = 0
	    then partial
	    else  partial_result (n1, n2 - 1, partial) @ [n1]
	fun helper (loi10 : int list, loi20 : int list, result : int list) =
	if null loi10 orelse null loi20
	then result
	else helper (tl loi10, tl loi20, partial_result (hd loi10, hd loi20, result))
    in
	helper (loi1, loi2, [])
    end
	
(* 6.
   int option * int option -> int * option	
   given two "optional" integers, adds them if they are both present (returning SOME of their sum),
   or returns NONE if at least one of the two arguments is NONE
 *)

fun addOpt (n1 : int option, n2 : int option) =
    if isSome n1 andalso isSome n2
    then SOME ((valOf n1) + (valOf n2))
    else NONE
	     
(* 7.
   int option list -> int option
 *)

fun addAllOpt (loio : int option list) =
    if null loio
    then SOME 0
    else if isSome (hd loio)
    then SOME ((valOf (hd loio)) + (valOf (addAllOpt( tl loio))))
    else (addAllOpt (tl loio))
(* 8.
   bool list -> bool		   
 *)

fun any (lob : bool list) =
    if null lob
    then false
    else if (hd lob) = true
    then true
    else any (tl lob)
		      
(* 9.
   bool list -> bool
 *)

fun all (lob : bool list) =
    if null lob
    then true
    else
	if (hd lob) = true
	then all (tl lob)
	else false
		 
(* 10.
   int list * int list -> int * int
   given two lists of integers creates consecutive pairs, and stops when one of the lists is empty
 *)

fun zip (loi1 : int list, loi2 : int list) =
    let
	fun helper (loi10 : int list, loi20 : int list, result : (int * int) list) =
	    if null loi10 orelse null loi20
	    then result
	    else helper (tl loi10, tl loi20, result @ [(hd loi10, hd loi20)])
    in
	helper (loi1, loi2, [])
    end

(* 11.
    int list * int list -> int * int
    given two lists of integers creates consecutive pairs, 
    when one list is empty it starts recycling from its start until the other list completes
 *)

fun zipRecycle (loi1 : int list, loi2 : int list) =
    let
	fun helper1 (loi10 : int list, loi20 : int list, result : (int * int) list) =
	    if null loi10 andalso not (null loi20)
	    then helper1 (tl loi1, tl loi20, result @ [(hd loi1, hd loi20)])
	    else
		if null loi20
		then result
		else helper1 (tl loi10, tl loi20, result @ [(hd loi10, hd loi20)])

	fun helper2 (loi10 : int list, loi20 : int list, result : (int * int) list) =
	    if null loi20 andalso not (null loi10)
	    then helper2 (tl loi10, tl loi2, result @ [(hd loi10, hd loi2)])
	    else
		if null loi10
		then result
		else helper2 (tl loi10, tl loi20, result @ [(hd loi10, hd loi20)])
    in
	if (length loi1) > (length loi2)
	then helper2 (loi1, loi2, [])
	else helper1 (loi1, loi2, [])
    end
	
(* 12.
   int list * int list  -> (int * int) list ption
   return SOME of a list of pairs if the given lists have the same length, and NONE if the lengths differ
 *)

fun zipOpt (loi1 : int list, loi2 : int list) =
    if (length loi1) = (length loi2)
    then SOME (zip (loi1, loi2))
    else NONE
	     
(* 13.
   (strin * int) list * string -> int option
   if given string is in the list, then return SOME (corresponding i), else return NONE
 *)

fun lookup (pair_list : (string * int) list, str : string) =
    if null pair_list
    then NONE
    else if (#1 (hd pair_list)) = str
    then SOME (#2 (hd pair_list))
    else lookup (tl pair_list, str)
		
(* 14.
   int list -> int list * int list
   return two list with positive and negative numbers separated
 *)

fun splitup (loi : int list) =
    let
	fun helper (loi0 : int list, positive : int list,negative : int list) =
	    if null loi0
	    then (positive, negative)
	    else if (hd loi0) >= 0
	    then helper (tl loi0, positive @ [hd loi0],negative)
	    else helper ( tl loi0, positive, negative @ [hd loi0])
    in
	helper (loi,[],[])
    end
	
(* 15.
   int list * int -> int list * int list
   separate the numbers from the given list bigger and smaller than the given number
 *)

fun splitAt (loi : int list, n : int) =
    let
	fun helper (loi0 : int list, greater : int list, smaller : int list) =
	    if null loi0
	    then (smaller, greater)
	    else
		if (hd loi0) >= n
		then helper (tl loi0, greater @ [hd loi0], smaller)
		else helper (tl loi0, greater, smaller @ [hd loi0])
    in
	helper (loi,[],[])
    end
	
(* 16.
   int list -> boolean
   produce true if numbers in the given list are in increasing order
 *)

fun isSorted (loi : int list) =
    if null (tl loi)
    then true
    else
	if (hd loi) < (hd(tl loi))
	then isSorted (tl loi)
	else false
		 
(* 17.
   int list -> boolean
   produce true if the numbers from the given list are either in increasing or decreasing order
 *)

fun isAnySorted (loi : int list) =
    let
	fun helper (loi0 : int list) =
	     if null (tl loi0)
	     then true
	     else
		 if (hd loi0) > (hd(tl loi0))
		 then helper (tl loi0)
		 else false
    in
	if isSorted loi = true
	then true
	else if helper loi = true
	then true
	else false
    end
	
(* 18. 
   int list * int list -> int list
   given two lists of integers, both sorted in increasing order, return one merged list sorted in increasing order
 *)

fun sortedMerge (loi1 : int list, loi2 : int list) =
    let
	fun helper (loi10 : int list, loi20 : int list, result : int list) =
	    if null loi10
		then result @ loi20
		else
		    if null loi20
		    then result @ loi10
		    else
			if (hd loi10) < (hd loi20)
			then helper (tl loi10, loi20, result @ [hd loi10])
			else helper (loi10, tl loi20, result @ [hd loi20])	  			     
    in
	helper (loi1, loi2, [])
    end
	
(* 19.
   int list -> int list
   return the given list sorted in increasing order
 *)
fun qsort (loi : int list) =
    let
	fun insert (n : int, loi10 : int list) =
	    if null loi10
	    then [n]
	    else
		if n > (hd loi10)
		then (hd loi10) :: insert (n,tl loi10)
		else n :: loi10
	fun sortIt (loi0 : int list) =	  
	    if null loi0
	    then []
	    else insert (hd loi0, sortIt (tl loi0))

	val separated = splitAt (loi, hd loi)
    in
	(sortIt (#1 separated)) @ (sortIt (#2 separated))
    end
	
(* 20.
   int list -> int list * int list
   given a list of integers, produces two lists by alternating elements between the two list
 *)

fun divide (loi : int list) =
    let
	fun helper (loi0: int list, acc : int, loi1 : int list, loi2 : int list) =
	    if null loi0
	    then (loi1,loi2)
	    else if (acc mod 2) = 0
	    then helper (tl loi0, acc + 1, loi1 @ [hd loi0], loi2)
	    else helper (tl loi0, acc + 1, loi1, loi2 @ [hd loi0])
    in
	helper (loi,0,[],[])
    end
	
(* 21.
   int list -> int list
   given the initial list of integers, splits it in two lists using divide, 
   then recursively sorts those two lists, then merges them together with sortedMerge
 *)

fun slow_sort (loi : int list) =
    let
	val divided_list = divide loi

	fun insert (n : int, loi10 : int list) =
	    if null loi10
	    then [n]
	    else
		if n > (hd loi10)
		then (hd loi10) :: insert (n,tl loi10)
		else n :: loi10
	fun sortIt (loi0 : int list) =	  
	    if null loi0
	    then []
	    else insert (hd loi0, sortIt (tl loi0))
    in
	sortedMerge ((sortIt (#1 divided_list)), (sortIt (#2 divided_list)))
    end
	
(* 22.
   int * int -> int * int
   given two numbers, k and n, it attempts to evenly divide k into n as many times as possible
   and returns a pair (d, n2), where d is the number of times and n2 is the resulting n after all the divisions
 *)

fun fullDivide (k : int, n : int) =
    let
	fun helper(n0 : int,  times : int) =
	    if (n0 mod k) = 0
	    then helper (n0 div k, times + 1)
	    else (times, n0)
    in
	if k <> 1
	then
	    if k <> 0
	    then helper (n,0)
	    else (0,0)
	else (n,0)
    end
	
			   
