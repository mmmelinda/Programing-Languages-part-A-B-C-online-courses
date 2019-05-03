(* Extra Practice Problems *)

type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* 1. int option * int -> pass_fail *)

fun pass_or_fail (grd, i) =
    case grd of
	NONE => fail
      | SOME i => if i>= 75
		  then pass
		  else fail
			   
(* 2. final_grade -> bool *)			   
	
fun has_passed {grade : int option, id : 'a} =
    pass_or_fail (grade, id) = pass

(* 3. final_grade list -> int *)				   
			   
fun number_passed (log) =
    let
	fun sum (log, acc) =
	    case log of
		[] => acc
	      | g::log' => if has_passed g
			   then sum (log', 1+acc)
			   else sum (log', acc)
    in
	sum (log,0)
    end
	
(* 4. (pass_fail * final_grade) list -> int *)						   

fun number_misgraded (list)=
    let
	fun sum (list, acc) =
	    case list of
		[] => acc
	      | (pof,{grade=grd,id=d})::list' => if pof = pass_or_fail (grd,d)
						 then sum (list',acc)
						 else sum (list', 1 + acc)
    in
	sum(list,0)
    end
	
datatype 'a tree = leaf 
                 | node of { value : 'a,
			     left : 'a tree,
			     right : 'a tree }
			       
datatype flag = leave_me_alone | prune_me

(* 5. 'a tree -> int *)

fun tree_height (tree) =
    case tree of
	leaf => 0
      | node {value=vl,left=lft,right=rgt} =>
	let
	    val left_branch = tree_height lft

	    val right_branch = tree_height rgt

	in
	  1+ ( if left_branch > right_branch
	       then left_branch
	       else right_branch)
	end
	    
(* 6. int tree -> int*)	  

fun sum_tree (tree) =
    case tree of
	leaf => 0
      | node {value=vl,left=lft,right=rgt} =>
	let
	    val left_branch = sum_tree lft

	    val right_branch = sum_tree rgt
	in
	    vl + left_branch + right_branch
	end
	    
				     
