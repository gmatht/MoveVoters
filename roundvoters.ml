(*
Version of movevoters for circular voting positions case
ACTUALLY CHANGE FOR ROUND CASE

core of the program is get_lp that gets the Linear Program needed to solve the problem.



TODO: Round floats for better precision, as they should be floats after scaling
./MoveVoters/ocaml-glpk-0.1.6/examples/example.ml

Inputs
 Frequencies: [c0 c1 c2 ...] c0 is number of candidates in 0th position
 Scores: [s0 s1 s2 ...] a scoring rule such that a voter ranking 
    a candidate in position i adds sum_{j>i} s_j the score.
    E.g. [1 0 0 0] is First past the post
         [1 1 1 1] is Borda


Requires a patched version of OCaml-GLPK to use lpx_exact instead of lpx_simplex

Should not exist an NE to 
2 1 1 1 1 0; 2 1 3 but may if inexact arithematic
*)


(* PRELIMINARIES *)

(* Adds two vectors *)

exception Slippery_Slope

let vector_sum = List.map2 ( + ) ;;
let vector_subtractf = List.map2 ( -. ) ;;

let list_neg = List.map (fun x -> -. x);;
let list_div = (fun y -> List.map (fun x -> x /. y));;

let verbosity = ref 0;;

let pr1 x = if !verbosity >= 1 then Printf.printf x else Printf.ifprintf stdout x
let pr2 x = if !verbosity >= 2 then Printf.printf x else Printf.ifprintf stdout x
(*let pr2 = pr_ 2*)

(* Returns the sum of a list (int & float respectively *)
let sum_list   = List.fold_left  (+)  0 ;; 
let sum_listf  = List.fold_left  (+.) 0.;; 
let sum_arrayf = Array.fold_left (+.) 0.;; 

let epsilon = 0.01
let is_zerof f = (f < epsilon) && (f > -.epsilon)

let string_of_floatlist = fun fl -> String.concat "\t" (List.map string_of_float fl);; 
let string_of_intlist = fun l -> String.concat "\t" (List.map string_of_int l);; 
let string_of_floatarray = fun fl -> (string_of_floatlist (Array.to_list fl));; 

let println_sfl = fun s fl -> print_string (String.concat "\t" [s; string_of_floatlist fl;".\n"])
let println_sil = fun s l -> print_string (String.concat " " [s; string_of_intlist l;".\n"])

let rec list_tl_n l n = if n <= 0 then l else list_tl_n (List.tl l) (n - 1);; (* Remove first n elements from list *)
(* Splits list l into two lists a, b, returns (rev a, b) 
   Note that it reverses the list a!! *)
let rec split_list l n = if n <= 0 then ([],l) else 
	match  (split_list l (n-1)) with 
		(a,x::b) -> (x::a,b)
		| _ -> failwith "split_list_err";;

let split_listt l n = match (split_list l (n)) with
                (a,x::b) -> (a,x,b)
                | _ -> assert false ;;

let decrement_nth = fun l n -> 
	let a = Array.of_list l in
	let _ = a.(n) <- a.(n) - 1 in
	Array.to_list a;;


		

(* Let an Xvector be a vector such that if we take the product of the xvector with a vector of political positions, we the total score awarded to a candidate *)


(*let rec scoref frq scores = match frq with [] -> [] | head::tail -> if head = 0 then (scoref tail scores) else (List.append [head] (scoref tail (List.tl scores)));;
*)

(* This returns an Xvector v for scores, 
 *   i.e. moving the ith candidate forward e units will increase the score of
 *   the current candidate by v[i]*e
 *
 * Inputs: 
 *     frq - the frequencies of positions to the right of this candidate
 *     scores - a scoring function
 *
 * Let c1 c2 c3 ... be the positions of 1st candidate after this one, the 2nd
 * and so on. 
 * Voters to the left of the midpoint will vote for this candidate. Hence 
 * Moving the next candidate forward 2e units will increase the number of first
 * place votes by e. It will not affect the number of voters who rank the
 * candidate 2+ 
 * *)

let rec scoref frq scores = match frq with
	[] -> [] 
	| head::tail -> 
		if head = 0
			then 0.::(scoref tail scores) 
			else match scores with 
				[] -> assert false
				| score::scoreT -> 
				( match (scoref ((head-1)::tail) scoreT) with
					[] -> assert false
					| oh::ot -> ((oh+.(score /. 2.)))::ot);;


(* Returns an Xvector of the score a candidate at zero would get
   I THINK THIS IS BUGGY *)

(*
let score_at_zero = fun frq scores ->
	let sum_frq = sum_list frq in
	let (a,b) = split_list scores sum_frq in
	sum_listf b;; (* STUB *)
*)
(* The value a candidate loses from candidates to the right, 
 * by moving towards the right by one unit.
 *)
let score_gradient_ro = fun scores n -> let (a, b) = split_list scores n in
	let (sum_a, sum_b) = (sum_listf a, sum_listf b) in
	( (sum_a /. 2.) +. sum_b ) 

(* The total value a candidate *gains* my moving one unit to the right
 * Including votes to the right and left *)

let score_gradient = fun scores frq_before frq_after -> 
	let (a, b) = (sum_list frq_before, sum_list frq_after) in 
	(score_gradient_ro scores a) -. (score_gradient_ro scores b);;

(*let rec join_list_ a b = if n <= 0 then ([],l) else 
	match a with 
		x::l -> join_list x::l 
		| _ -> assert false ;;
*)
(*let scorerev = fun frq scores (List.rev 
*)
(*
let scoreBA = fun frq_before frq_after scores  -> 
	let scores_before = list_neg (scoref frq_before scores) in
	let scores_after = (scoref frq_after scores) in 
(*	(list_neg ( score_at_zero frq_after scores ), score_at_zero frq_after scores, score_gradient  );; *)
	(scores_before, scores_after, score_at_zero frq_after scores, score_gradient  );;
(* Should rewrite the following in terms of scoreBA or delete *)
let scoreA = fun frq scores n  -> 
	let (frq_before, frq_after) =  split_list frq n in
	let scores_before = List.rev (list_neg (scoref frq_before scores)) in
	let scores_after = (scoref frq_after scores) in 
(*	(list_neg ( score_at_zero frq_after scores ), score_at_zero frq_after scores, score_gradient  );; *)
	(scores_before, scores_after, score_at_zero frq_after scores, score_gradient  );;
*)

   (*(Array.to_list newscores);;*)
(* When n candidates shares a postition the effective scores match (smear_scores scores n) *)


(* OUT_OF_DATE smear_scores_ takes as input a scoring rule and outputs a scoring rule that gives
   the total score given to a group of n candidates with the same political position 

   smear_scores is as above, but gives the score given to one of the candidates *)

let rec smear_scores_ = fun scores n ->
     	if n <= 1 then (
		scores
	) else (
		match scores with
		a::b::sc -> (smear_scores_ ((a+.b)::sc) (n-1))
		| _ -> (assert false)
	)

let smear_scores scores n =
	let div = (float n) in
	List.map (fun x -> x/.div) (List.rev (smear_scores_ (List.rev scores) n))

let smear_scores scores n =
	let a = Array.of_list scores in
	let nf = (float_of_int n) in
	for j = 1 to (n-1) do  
		let i = j - 1 in
		a.(i) <- (a.(i)/.nf)*.(float_of_int j)
	done;	
	Array.to_list a;;
	
(*	 
let smear_scores scores n =
   let ascores= (Array.of_list scores) in
   let newscores = Array.make ( (Array.length ascores) - n + 1 ) 0.0 in
   let _ = for i = 0 to (Array.length newscores) - 1 
   do
	for j = 0 to n - 1
	do newscores.(i) <- newscores.(i) +. ascores.(i+j)
	done ;
	newscores.(i) <- newscores.(i) /. (float n) 
   done in 
   Array.to_list newscores;; 
   (*(Array.to_list newscores);;*)
*)
let smear_scores scores n =
   let siz = List.length scores in
   let ascores= Array.append (Array.of_list scores) (Array.make n 0.0) in
   let newscores = Array.make siz 0.0 in
   let _ = for i = 0 to siz -  1 
   do
	for j = 0 to n - 1
	do newscores.(i) <- newscores.(i) +. ascores.(i+j)
	done ;
	newscores.(i) <- newscores.(i) /. (float n) 
   done in 
   Array.to_list newscores;; 
   (*(Array.to_list newscores);;*)
let rec list_tl_n l n =
	if n = 0 
		then l
		else (list_tl_n (List.tl l) (n-1))

let score_move_self_ frq_before scores =
	let sum_before = sum_list frq_before in
	sum_listf (list_tl_n scores sum_before)

(* slope of score change if all candidates that share a position move to the right
 * Note that if this is an equilibrium, and frq(n)=1, then this slope must equal 0
   IN ROUND CASE THIS ALWAYS EQUALS 0 *)
let score_move_self = fun frq_before frq_after scores ->
	0.
(*	((score_move_self_ frq_before scores) -. (score_move_self_ frq_after scores)) /. 2.;;*)

(*#trace score_move_self;;*)
(*let score_move_self = fun frq_before frq_at frq_after scores ->
(*        let (frq_before, old_frq_at, frq_after) = split_listt frq n in
	let old_frq_at = move_adj + old_frq_at in
dd	let scores = list_tl_n oscores (frq_at) in 
*)
	(*Printf.printf "\nscore_move_self: %s \n frq %d\n  before: %s\n  after: %s\n" 
		(string_of_floatlist scores)
		frq_at (string_of_intlist frq_before) (string_of_intlist frq_after)*)
	let (sum_before, sum_after) = (sum_list frq_before, sum_list frq_after) in
	
	let diff_sum = sum_after - sum_before in
	let (abs_sum, sign_sum) = if diff_sum > 0 then (diff_sum, 1.0) else (-diff_sum, -1.0) in
        let (diff_scores, _) = split_list (List.rev scores) (abs_sum+frq_at) in 
(*
        let (diff_scores, _) = split_list (List.rev scores) abs_sum in 
	let _ = (println_sil "n" [n],println_sil "frq_before" frq_before, println_sil "frq_after" frq_after, println_sil "abs_sum" [abs_sum], println_sfl "sign_sum" [sign_sum], println_sfl "diff_scores" diff_scores, println_sfl "-----" [])  in *)
(*        let (scores_that_cancel, diff_scores) = split_list scores diff_sum in 
	( (sum_listf diff_scores) *. sign_sum ) *. 100. +. 10000.;;

*)
	let r = ( (sum_listf diff_scores) *. sign_sum ) /. (float frq_at) /. 2.0 in 
	(Printf.printf "\nscore_move_self: %f for %s \n frq %d\n  before: %s\n  after: %s\n diff_sum: %d\n " 
		r (string_of_floatlist scores)
		frq_at (string_of_intlist frq_before) (string_of_intlist frq_after)
		diff_sum);
	r
*)

(*let xvector_x__ = let scores_before = List.rev (list_neg (scoref frq_before scores)) in
        let scores_after = (scoref frq_after scores) in
        let score_at_z = if frq_after = [] then sum_listf scores /. frq_at else 0 in
        List.concat [ score_at_z, scores_before, scores_after ];;*)

(*let slopes_of_n = fun frq scores n ->*)


let add_to_first = fun b f l ->
	if b then
	match l with 
		h::t -> (h+.f)::t
		| _ -> failwith "Attempt to add to first element of empty list"
	else
		l

(* here is a function that will fail if we a candidate can gain by moving slightly to the left or right *)
let assert_no_slope frq scores =
	let rec f = fun frq_before frq_after ->
		match frq_after with 
			x::a -> let slope = score_move_self frq_before a scores in 
				(pr2 "%s (%d) %s -> %f\n" (string_of_intlist frq_before) x (string_of_intlist a)) slope ;
				if (slope = 0. || x != 1) (* OK if we are only using integer floats*)
					then (f (x::frq_before) a)
				else
					raise Slippery_Slope
			| _ -> () in
	f [] frq
					(*((Printf.printf "Candidate %d would slide to %s\n" *)
	
(*let slopes_of_n = fun frq scores n -> *) 
let xvector_adjacent_n = fun afterb frq scores n -> 
	let after=if afterb then 1 else 0 in 
	let (frq_before, frq_after) =  split_list frq (n + after) in
	let moveself = score_move_self frq_before frq_after scores in
	let myscores = scores in
	let beforeb = not afterb in
	let scores_before_ = list_neg (scoref (List.append frq_before (List.rev frq_after)) myscores) in
	let scores_before = add_to_first afterb  moveself scores_before_ in
	let scores_after_ = scoref (List.append frq_after (List.rev frq_before))  myscores in  
	let scores_after = add_to_first beforeb moveself scores_after_ in  
	let score_at_z = (
		sum_listf (list_tl_n scores (sum_list frq_after)) +.
		sum_listf (list_tl_n scores (sum_list frq_before)) 
	) /. 2. in
	let r = List.concat [ [score_at_z]; List.rev scores_before; scores_after ] in 
	let _ = pr2 "\n\nAfter: %d\n frq: %s\n  before: %s\n  after:%s \n scores: %s\n n: %d\n r:%s \n"
		after
		(string_of_intlist frq) (string_of_intlist frq_before) (string_of_intlist frq_after)
		(string_of_floatlist scores) n (string_of_floatlist r) in
	let _ = pr2 "  scores_after_ %s\n  scores_after %s\n moveself: %f\n"
		(string_of_floatlist scores_after_)
		(string_of_floatlist scores_after)
		moveself in
	r;;
(*
	(xvector__n n frq frq_before frq_at frq_after scores scores);;
	let scores_before = List.rev (list_neg (scoref frq_before scores)) in
	let scores_after = (scoref frq_after scores) in  
	let score_at_z = if frq_after = [] then sum_listf scores /. (float frq_at) else 0. in
	List.concat [ score_at_z, scores_before, scores_after ];;
*)

(*let xvector__n = fun n frq frq_before frq_at frq_after myscores scores ->
	let scores_before = List.rev (list_neg (scoref frq_before myscores)) in
	let scores_after = (scoref frq_after myscores) in  
	let score_at_z = if frq_after = [] then sum_listf scores /. (float frq_at) else 0. in
	(*list_div frq_at (List.concat [ [score_at_z]; scores_before; [(score_move_self frq_before frq_at frq_after scores n)]; scores_after ]);;*)
	List.concat [ [score_at_z]; scores_before; [(score_move_self frq_before frq_at frq_after scores)]; scores_after ];;


let xvector_at_n= fun move_adj frq scores n -> 
	let (frq_before, x, frq_after) =  split_listt frq n in
        (* If the candidate moves to pos n, there will be one more candidate *)
	let frq_at = x + move_adj in 
	let myscores = smear_scores scores frq_at in
	print_int n;
	print_int frq_at;
	println_sfl "\nmyscores" myscores;
	xvector__n n frq frq_before frq_at frq_after myscores scores;;*)

(*
In linear voting order incrementing positions before n move them closer decreasing the score and incrementing positions after increase the score.

TODO-r circular voting order should be the same but we need to concat frq_before and frq_after.

Inputs
 frq_before: freqencies before current position (in reverse order, so first element is closest to current position. 
 frq_after: Freqencies after current position, if forward order so first element is closest to current position.
 myscores has already been smeared so it should be as if there is only one candidate at the current position 
*)
let xvector__n frq_before frq_after myscores =
	let myscores_before = List.rev (list_neg (scoref (List.append frq_before (List.rev frq_after)) myscores)) in
	let myscores_after = (scoref (List.append frq_after (List.rev frq_before)) myscores) in  
	let score_at_z = (
		sum_listf (list_tl_n myscores (sum_list frq_after)) +.
		sum_listf (list_tl_n myscores (sum_list frq_before)) 
		) /. 2.  in (* Score at zero *)
	(*let score_at_z = if frq_after = [] then sum_listf myscores else 0. in*)
	(*list_div frq_at (List.concat [ [score_at_z]; scores_before; [(score_move_self frq_before frq_at frq_after scores n)]; scores_after ]);;*)
	(*List.concat [ [score_at_z];  myscores_before; [(score_move_self frq_before frq_after myscores)]; myscores_after ];;*)
	List.concat [ [score_at_z];  myscores_before; [0.]; myscores_after ];;

(* Vector of scores if we move to position n *)
let xvector_at_n move_adj frq scores n = 
	let (frq_before, x, frq_after) =  split_listt frq n in
        (* If the candidate moves to pos n, there will be one more candidate *)
	let frq_at = x + move_adj in 
	let myscores = smear_scores scores frq_at in
	if (!verbosity > 1 ) then (
		print_int n;
		print_endline "";
		print_int frq_at;
		print_endline "";
		println_sfl "\nmyscores" myscores
	);
	xvector__n frq_before frq_after myscores
(*List.concat [ [score_at_z];  myscores_before; [(score_move_self frq_before frq_at frq_after scores)]; myscores_after ];;*)

let decrement_nth = fun l n -> 
	let a = Array.of_list l in
	let _ = a.(n) <- a.(n) - 1 in
	Array.to_list a;;


(* All of these must be non-positive, or candidates starting at position n can boost their vote by moving *)
let xvector_diffs = fun oldfrq scores ->
	let move_f = [|xvector_at_n 1; xvector_adjacent_n false|] in
	let l = ref [] in
	let _ = pr1 "\n\n\t1\tc1\tc2\t...\n" in
	let m = (List.length oldfrq)-1 in
	let _ = for i = 0 to m 
		do 
			let frq = decrement_nth oldfrq i in
			let orig_xvect = xvector_at_n 1 frq scores i in 
			for j = 0 to m
			do
				for k = 0 to 1
				do 
					(*println_sil "mijk" [m;i;j;k];*)
					let new_xvect=move_f.(k) frq scores j in
					let diff =  vector_subtractf new_xvect orig_xvect in	
					
					let move_symb = [|"";"-";"+"|] in
					let name = Printf.sprintf "%d->%d%s" i j move_symb.(k) in 
					l.contents <- (name,diff)::!l;
					if (!verbosity > 1 ) then (
						ignore (print_endline name);
						println_sfl "old" orig_xvect;
						println_sfl "new" new_xvect;
						println_sfl "gain" diff;
						print_string "\n";
					)
				done 
			done
		done in
	l.contents ;;

(*	
	let diff = vector_subtractf orig_xvect new_xvect in	
*)	




	
(*let ( scores_before, scores_after, score_at_z, score_grad) = scoreA frq scores n in*)
	
(* Greatest common divisor (inefficent) *)
let rec gcd a b =
	if a > b 
		then (gcd (a - b) b)
	else 
		(if a < b then (gcd a (b - a))
		 else a)

let gcm a b = a * b / (gcd a b)

(*let scale_factor l = 2 * (List.fold_left ( fun a b -> if b=1 then a else (gcm (b+1) (gcm (b-1) (gcm b a)))) 1 l ) *)

let rec fold_gcm n = if n = 1 then 1 else gcm n (fold_gcm (n-1)) 
let scale_factor sc = 2 * fold_gcm (1+List.length sc) 
let scale_factor_frq l = 2 * (List.fold_left ( fun a b -> if b=1 then a else (gcm (b+1) (gcm (b-1) (gcm b a)))) 1 l ) 

let normalise_scores fr sc = let sf = scale_factor_frq fr in pr1 "Scale Factor: %d\n" sf ; (fr, List.map (fun e -> float_of_int (e*sf)) sc)




	
	

(* let (fr,sc) = ([1;1;2], [0.2;0.2;0.2;0.3]);; *)


(* let (fr,sc) = normalise_scores [2;1;1] [1;0;0;0];; *)
(* GAIN from move to right 1   0.5  0.25
   GAIN from move to left  0.5 0.5  0.5
   NET GAIN  move to right 0.5 0   -0.25

Xvector 0: ?  0.5  -0.5  0
Xvector 1: ? -0.5   0    0.5
Xvector 2: ?  0    -0.25 -0.25
*)

(* for i = 0 to 2 do
	(*ignore (smear_scores sc i);*)
	(xvector_at_n 0 fr sc i);
done;

print_string "Xvector 0: ?  0.5   0.5  0\n";
print_string "Xvector 1: ? -0.5   0    0.5\n";
print_string "Xvector 2: ?  0    -0.25 -0.25\n\n";

for i = 0 to 2 do
	Printf.printf "Xvect%d " i;
	println_sfl "" (xvector_at_n 0 fr sc i);
done;;
*)

(* Print out data given in first email to Slinko *) 
(*List.iter (println_sfl "c") (xvector_diffs fr sc);;*)

let dump_bound name m l =
	Printf.printf "%s:\t%s <= %d\n" name (string_of_floatlist (Array.to_list l)) m;;

let rec dump_bounds b =
	match b with 
		(name,m,l)::b_tl -> 
			(dump_bound name m l) ;
			(dump_bounds b_tl)
		| _ -> ();;

(* Bounds *)

let rec iter_freq_ total remaining l f =
	if remaining=0 then f (List.rev l)
	else for i = remaining downto 1 do
		iter_freq_ total (remaining-i) (i::l) f
	done

let iter_freq n f = iter_freq_ n n [] f 



(*
Enumerate possible frequencies
let _ = print_string "-------------\n"
let _ = iter_freq 6 (fun l -> println_sil "fr" l)
let _ = print_string "-------------\n"
*)

(*let rec split_string s = let t = String.trim s in
	let i = String.rindex
*)
let string_words s_ = let s = " " ^ s_ in
	let words=ref [] in
	let word_len=ref 0 in
	for i = (String.length s_) downto 0 do
		if s.[i] = ' ' then (
			if (!word_len > 0) then
				words := (String.sub s (i+1) !word_len)::!words;
				word_len:=0;
		) else (
			word_len := !word_len+1
		)
	done ;
	words;;
string_words " xxxx  YY x z";;
string_words "z";;
string_words "1 z";;




(*let bounds = List.map (fun x -> ((-.infinity,0),x)) bounds *)

(*print_string "sdfsfds\n";;
*)

(*scoreA [1;1;2] [0.2;0.2;0.2;0.3] 1;;
(*print_float (score_move_self [1;1;2] [0.2;0.2;0.2;0.3] 0);;*)
(*print_string "  ";;
print_float (score_move_self [1;1;2] [0.2;0.2;0.2;0.3] 1);;
print_string "  ";;
print_float (score_move_self [1;1;2] [0.2;0.2;0.2;0.3] 2);;
print_string "  ";;
print_float (score_move_self [1;1;2] [0.2;0.2;0.2;0.3] 3);;
print_string "  ";;
print_string "\n";;
for i = 0 to 3 
do println_sfl "xvect" (xvector_at_n 0 [1;1;2] [0.2;0.2;0.2;0.3] i)
done;;
*)
*)


(*
println_sfl "c" (List.hd (xvector_diffs fr sc));;
List.iter (println_sfl "c") (xvector_diffs fr sc);;
let (o1,o2,o3)=scoreA [1;1;2] [0.2;0.2;0.2;0.3] 1;;
print_string (String.concat " " (List.map string_of_float o1));;*)


(*let xvector_adjacent_n = fun afterb frq scores n -> 
  - after and before
test xvector_adjacent
at
non-zero score_z
in middle
at end
borda
6 1 1 0
LP finds solution

	assert score_move_self = fun frq_before frq_at frq_after scores ->
*)

(*let adj_assert a b c d eq =
	let result = xvector_adjacent_n a b c d in
*)	



(* Do actual problem *)
(*
#load "glpk.cmo";;
#load "glpk.cma";;
#load "glpk_stubs.o";; 
*)

let check_bounds = fun  bounds positions myepsilon print ->
	let error = ref false in
	List.iter ( fun (name,maxv,vector) -> (
		let v = sum_listf (List.map2 ( *. ) (Array.to_list vector) (Array.to_list positions)) in
		if (v +. myepsilon > (float_of_int maxv)) then ( error := true ; if print then Printf.printf "%f " v;  dump_bound name maxv vector )

	)) bounds;
	!error;;

open Glpk
open Str 

(* the following is technially incorrect, but oh well. *)
let round x = let r = floor ( x +. 0.5 ) in
	assert (abs_float (r -. x) < 0.0001);
	r;;  

let slack = ref 0.0 (* Increase this to allow almost solutions? *)

let get_lp fr_ sc_ myprim =
	let epsilon=0.000 in
	let (fr,sc) = normalise_scores fr_ sc_ in
	let _ = assert_no_slope fr sc in
	flush stdout;
	let siz = 1+List.length fr in
	let rec bound_inorder_ n = 
		if n == 1 then []
		else let a = Array.make siz 0.0 in
			a.(n-1) <- 1.0;
			a.(n) <- -1.0;
			(Printf.sprintf "p_%d < p_%d" (n-1) n,0,(Array.append [|1.0|] a))::(bound_inorder_ (n-1)) in
	let bound_inorder = bound_inorder_ ((List.length fr)) in (*Ensure monotonically increasing positions *)
(*let bound_nogain = list.map (fun x-> ((-.infinity,0),x) (xvector_diffs fr sc));; *)
	let bound_nogain = List.map (fun (name,x)-> (name,0,Array.of_list x)) (xvector_diffs fr sc) in
	let bound_0 = (Array.make siz 0.0) in
	    bound_0.(1) <- -1.0; (* Ensure the first position is atleast 0, and hence all other positions are aleast 0 *)
	let bound_1 = (Array.make siz 0.0) in
	    bound_1.(siz-1) <-  1.0 ; (* Ensure that last position is at most 1, and thus all are at most 1 *)
	let bounds_noslack0 = List.concat [bound_nogain; [(">=0",0,bound_0)]; [("<=1", 1,bound_1)]] in
	let bounds_noslack1 = List.map (fun (name,a,b) -> (name, a, Array.append [|!slack|] b)) bounds_noslack0 in
	let bounds = List.concat ([bounds_noslack1; bound_inorder]) in
	if (!verbosity > 0) then dump_bounds bounds;
	(*dump_bounds bounds;; *)
	let vectors=List.map (fun (name,a,b) -> (Array.map round b)) bounds in
	let ranges=List.map (fun (name,a,b) -> (-.infinity,epsilon+.float_of_int a))  bounds in
	let maximize=Array.append [|1.0|] (Array.make siz 0.0) in
	let xbounds = (Array.map (fun a->(-.infinity,infinity)) (List.hd vectors)) in
	let _ = xbounds.(1) <- (1.0,1.0) in
	if (List.length myprim > 0) then (
		print_endline "BEGIN Bounds Violated";
		check_bounds bounds (Array.of_list (0.::1.::myprim)) 0.0;
		print_endline "END Bounds Violated";
		exit 0
	);
  	let lp = make_problem Maximize
             maximize
             (Array.of_list vectors)
             (Array.of_list ranges) 
             xbounds in
	set_message_level lp (if !verbosity >= 2 then 2 else 0);
	set_message_level lp 0;
    	(*scale_problem lp;
	
	 print_endline "write_lp ...";
    	write_cplex lp "cplex.txt"; *)
    	use_presolver lp true;
    	simplex lp;
    	let prim = get_col_primals lp in
	let obj_val = get_obj_val lp in
      	pr1 "Z: %g  X: %s\n%!" (get_obj_val lp) (string_of_floatarray prim);
	let error = check_bounds bounds prim (-0.0001) false in
	if ((obj_val <= 0.0 || error) && !verbosity > 1) then 
		(Printf.printf ("\n____________Problematic bounds_______________\n");
		check_bounds bounds prim 0.0001 true;
		 pr1 "frq: %s\nscores: %s\n" (string_of_intlist fr) (string_of_floatlist sc);
		print_endline "");
	(error, lp);;

let sign x =
	(abs_float x)

let get_report fr sc =
	try 
		let (error,lp) = get_lp fr sc [] in
    		let prim = get_col_primals lp in
      		let var = (string_of_floatlist (List.tl (List.tl (Array.to_list prim)))) in
      		(*let var = (string_of_floatarray prim) in*)
		if error then 
			"Invalid Solution: " ^ var
		else (
			let o = (get_obj_val lp) in
			if o > 0.0 then "Exists NE: " 
			else (
				if o < 0.0 then 
					"No NE: (needs slack) "
				else (
					if !slack > 0.0
					then "May have no gap: "
					else "No NE: (no gap) "
				)
			)
		) ^ var
	with Slippery_Slope ->
		"No NE: Non Zero Slope"
	| No_primal_feasible_solution ->
		"No NE: No LP solution"
(* from: http://rosettacode.org/wiki/Repeat_a_string#OCaml *)
let string_repeat s n =
  let len = String.length s in
  let res = String.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len;
  done;
  (res)
;;

let try_all_freq sc =
	let len = List.length sc in
	let results = ref [] in
	let f fr = if (List.length fr) > 1 then (
		let fr_s = (string_of_intlist fr) ^ (string_repeat "\t" (len - List.length fr)) in
		let report = get_report fr sc in
		let line = (fr_s ^ "; " ^ report ^ "\n") in
		if (!verbosity > 0) then (
			print_string "\n------------------------------\n";
			(print_string ("frq: " ^ fr_s ^ "\n"));
		);
		print_string line;
		results := line::!results
	) in 
	let _ = iter_freq len f in
	let out_string = ("\n\n" ^ String.concat "" !results) in
	if (!verbosity > 0) then print_string out_string;
	out_string


	
(* let _ = try_all_freq [1; 0; 0; 0];; *)

(*
let _ = print_string (get_report [2; 1; 1] [1; 0; 0;0]) *)

let words = (Str.split (Str.regexp ",?  *"))
let intlist_of_string s = List.map int_of_string (words s)

(* transforms traditional scores into the cumulative scores used internally.
   E.g. we transform the Borda scores "4 3 2 1" into "1 1 1 1" *)
let transform_scores scores =
	let rec f sc n =
		match sc with 
			hd::tl -> (hd-n)::(f tl hd)
			| [] -> [] in
	let cumulative = List.rev (f (List.rev scores) 0) in
	pr1 "Transformed scores into cumulative form: \nOriginal:   %s \nCumulative: %s\n\n" (string_of_intlist scores) (string_of_intlist cumulative);
	cumulative

let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

let has_ne sc = let r = try_all_freq sc in Printf.printf "\n--------\n%s\n^^^^^^^^^\n" r ; contains r "Exist"
let has_ne sc = let r = try_all_freq sc in contains r "Exist"
let has_neT sc = has_ne (transform_scores sc)
	

let self_diag () = (
	slack := 0.0;
	verbosity := 2;
	print_string("Beginning self diagnostics\n");
	assert ((score_move_self [1] [2] [120.;120.;0.;0.]) = 60.);
	(* here moving oneself to the right does not affect total 1st place vote, but
	   increases second place vote by 1/2x as we lose 1/2x to the voters to the right
           but gain 1/2x from the left *)
	assert ( (xvector_adjacent_n true [1; 2] [120.; 120.; 0.; 0.] 0) = [0.; 0.; 120.]);
	(* This represents a four candidate election. We consider a candidate infansemially to
	to right of the leftmost candidate, and to the left of the two rightmost candidates
	- score_at_z (0) because when all candidates are at zero our candidate gets none of the 1st and
		2nd place vote.
	- p1 (0) when p1 moves to the right we lose [1/2x] of the first place vote as we gain 1/2x
		from the candidates to the right, but lose 1x to the candidate to the left as the candidate
		to the left follows us right.
		We also gain [1/2x] of the second place vote, gaining 1/2 of the second place
		vote from the right voters without losing any second place votes.
		120*(-1/2+1/2)  = 0
	- p2 (120) As the candidates to the right move to the right, we gain [1/2x] of the first and second place
		votes. 
		120* (1/2+1/2) = 120 *)
	assert ( (xvector_adjacent_n true [1; 2] [240.; 120.; 0.; 0.] 0) = [0.; -60.; 180.]);
	(* score_at_z (0) as above
	- p1: 120*((2*-1/2)+1/2) = 120(-1/2) = -60 
	- p2: 120*((2+1/2)+1/2) = 180 *)
	assert ( (xvector_at_n 0 [2; 2] [120.; 120.; 0.; 0.] 0) = [0.; 90.; 90.]);
	assert ( (xvector_at_n 1 [1; 2] [120.; 120.; 0.; 0.] 0) = [0.; 90.; 90.]);
	(* score_at_z (0) as above
	- p1: moving right loses half the 1st place vote for the right member of the first group, but 
		also gains half the 2nd place vote, canceling out
		the left candiate gains 1x the 1st and 1/2 of the second place vote for a total of 3/2x of 
		120. this averages to 120*3/4 or 90 
		//average is (-1+2)/2=(1/2)
		//or 1/2x to each candidate.
	  p2: Moving right gives 1/2x 1st and 2nd to candidate c0.1 (right of leftmost group) and 
	      gives 1/2x of the 2nd place vote to the candidate c0.0 (left of left group)
	- p2: as with p1*)
	assert ( (xvector_at_n 0 [2; 2] [120.; 120.; 0.; 0.] 1) = [180.; -90.; -90.]);
	(* Similar to above, but 2nd candidate. At zero c1.1  gains all votes for total of 240, and
	   c1.0 gains second place votes for total of 120 this averages to 180: 1x first place and 1x second place
	   for total of 2x. Thus 1x per candidate in group *)
	assert ( (xvector_at_n 0 [2; 1;  2] [120.; 120.; 0.; 0.; 0.] 1) = [0.; -120.; 0.; 120.]);
	assert ( (xvector_at_n 0 [1; 3] [6.*.120.; 120.; 120.; 0.] 1) = [440.; -160.; -160.]); 
	(* at_z c1.2 gets all of vote (6+1+1+0)=8
		c1.1 gets (1+1+0)=2
		c1.0 gets 1
		avg = (8+2+1)*120/3 = (11*40)= 440 
		p0: c1.0 loses 1/2x 1st place vote; no effect on 2nd+ etc. (6*60)
			c1.1 loses 1/2 2nd+ place (60)
			c1.2 loses 1/2 3rd+ place (60)
		average of (6+1+1)*20 = 8*20=160
		p1: affect on left wing vote the reverse ie +160 instead of -160
			however c1.2 has 1x less vote in 1st place to the right
				simply because there is 1x less to the right
			c1.2 has 1x less 2nd+ place etc.
			so -320 loss for total of (160-320 = -160)
	*)
	assert ( (xvector_at_n 0 [2; 2] [72.; 12.; 12.; 0.] 0) = [6.; 27.; 27.]);
	(* score_at_z: [6] group gets third place vote of 12, both candidates get 12/2=6
	   p1: moving right loses c0.1 1/2x 1st place (72) and gains 1/2x 2nd place (12) vote, (c0.1 gets all third place vote anyway)
		 (-72/2+12/2) = 2(-18+3)= 2(-15)  
	       moving right gets c0.0  1x 1st place vote and 1/2x 2nd+ and 3rd+
	 	 (72+12/2+12/2) = 2(36+6=42)
		Average is (42-15=27)
	   p2: similar to above 
	*)
	assert ( (xvector_adjacent_n false [1; 2] [72.; 12.; 12.; 0.] 0) = [0.; 84.; 12.]);
	(* score_at_z: by moving left of all other candidates, get 0 score at pos 0
	   p1: moving right gets 1x of the first place vote (72), 1/2 of the 2nd and third place vote
               thus (72+12/2+12/2) = 72+12 = 84 
 	   p2: moving right gives none of the first place vote but gives 1/2 of the 2nd and 3rd place vote
		i.e. (12/2+12/2) *)

	assert ( (xvector_adjacent_n false [1; 2] [72.; 12.; 12.; 0.] 1) = [12.; -36.; 48.]);
	(* score_at_z: [12] by moving left of rightmost other candidates, get all of 3rd place vote at pos 0
		
	   p1: moving right takes 1/2x of the first place vote (72) but none of the 2nd and third place vote
               thus (-72/2)=-36  
 	   p2: moving right increases the gap between the first candidate and gains 1/2 of first place vote,
		 but gains 1x of the 2nd and 3rd place vote from the rightmost candidates
		i.e. (72/2+12/2+12/2)=(36+12)=48 *)
	assert ( (xvector_adjacent_n true [1; 2] [72.; 12.; 12.; 0.] 1) = [96.; -6.; -90.]);
	(* score_at_z: [12] by moving right of rightmost other candidates, get all of the vote at pos 0
		72+12+12=86
		p1: Moving p1 forward reduces the 3rd place vote by 1/2, hence 12/2=6
		p2: Moving p2 forward reduces the 1st and 2nd place vote by 1x and 3rd place by 1/2
			hence -72-12-6=-90
	*)
	assert ( (xvector_at_n 0 [2; 2] [72.; 12.; 12.; 0.] 1) = [60.; -27.; -27.]);
	(* at_z: the rightmost candiate would get the full 72+12+12=96
		the second would get 12+12. Between them they get the average of 36+12+12=60
	   p2: moving p2 right reduces by a factor of 1/2x the amount 2nd votes for c1.0
		left member of the cluster gets but increases the number of 1st votes by 1/2x.
		c1.0 is always voted 3rd or better. 
		 (72/2-12/2=36-6=30) and likewise reduces the number of 2nd+ and 3rd+
		votes by 1/2x of c1.1 and also reduces 1st by 1x (-12/2-12/2-72=-84).
		We take the average (30-84)/2=(-54)/2=-27
	   p1: moving p1 right reduces the 1st and 2nd vote of the c1.0 by 1/2x, 
		(-72/2-12/2=-36-6=-42)
 		reduces second and third place vote of c1.1 by 1/2x 
		(-12/2 -12/2=-12)
		average of -27
		for left member increases 1st and 2nd place vote by 1/2x (72/2 +12/2)
		giving average of (-72/2 -12/2)/2 = (-36-6)/2=(-18-3)=-21
for the ... *)
	assert ( has_neT [6; 1; 1; 0] );
	assert ( not (has_ne [6; 1; 1; 0]) );
	assert ( not (has_neT [2; 1; 1; 1; 1; 0])  ); (* if this one fails it means you probably haven't patched ocaml-glpk to use lpx_exact in place of lpx_simplex *)
	assert ( has_neT [2; 2; 1; 1; 1; 0]  );
	assert ( has_neT [10; 10; 4; 3; 3; 0]  );
	assert ( has_neT [4; 3; 1; 1; 0; 0]  );
	print_string ("Self diagnostic successful\n"));
	exit 0;;

(*
#load "findlib.cma"		
#load "glpk.cma"
#load "Str.cma"
#trace smear_scores;;
#trace score_at_z;;
#trace xvector_at_n;;
#trace xvector_adjacent_n;;
let x = self_diag ();;
*)

let rec list_l_e length elem =
	if length = 0
	then []
	else elem::(list_l_e (length - 1) elem)
;;

(* enumerate class of [a, b, b, ..., 0] where a>2b *)
let enumerate_ab inf =
   for m = 2 to inf do
	for length = 2 to m do
		for a = 1 to m do
			for b = 1 to m  do
				if ( (a > (2*b)) && (length = m || a = m || b = m) )
				then let scores = ((a::(list_l_e (length - 2) b))@[0]) in
					let c_scores = transform_scores scores in
					print_string ("\n\n---------------\nTASK " ^ (string_of_intlist [length; a; b]) ^ "\n");
					try_all_freq c_scores; ()
					(*print_string ("\n\nFOOO" ^ (try_all_freq c_scores) ^ "ENDFOOOO"); ()*)
			done
		done
	done
   done

;;

let get_opt c s =
	try s.[String.index s c] <- ' '; true
	with Not_found -> false

let clear_equals = fun s -> 
	try 
		let p = String.index s '=' in
		 String.sub s (p+1) (String.length s - p - 1)
	with Not_found -> s
let rec fixstr_ s i j = if j >= String.length s then String.sub s 0 i else let (si,jj)=(match s.[j] with '%' -> (char_of_int (int_of_string ("0x"^(String.sub s (j+1) 2))) ,j+3) | '+' -> (' ',j+1) | _ -> (s.[j],j+1)) in s.[i]<-si ; fixstr_ s (i+1) jj 
let fixstr = fun s -> fixstr_ (clear_equals s) 0 0

let _ =
 	Printf.printf "Content-type: text/plain TESSSST\n\n";
	let qs_ = try 
		(Sys.getenv "QUERY_STRING")
		with Not_found -> (
			try ( let opt = Sys.argv.(1) in
				if ( opt = "ab" ) 
				then  enumerate_ab 9999 ; opt 
			) with _ ->
			self_diag (); "1 0") in
	flush stdout;
	let qs = fixstr qs_ in
	pr2 "qs: %s\n" qs;
	let cumulative_scores = (get_opt 'C' qs || get_opt 'c' qs) in
	if (get_opt 'S' qs) then slack := 1.0; 
	verbosity := if get_opt 'V' qs then 2 else 
		(if get_opt 'v' qs then 1 else 0);
	let args = Str.split (Str.regexp "; *") qs in
	let score_string = String.uppercase (List.hd args) in
(*	if (score_string.[0] = 'C') then (
		score_string.[0] <- ' '
	);*)
	let sc_ = intlist_of_string score_string in
	let sc = if cumulative_scores then sc_ else transform_scores sc_ in
	(*print_endline ("Cumulative Scores: " ^ (string_of_intlist sc));*)
	if ((List.length args) = 1) 
		then ignore (try_all_freq sc)
		else (
			let l = List.tl args in
			let fr = intlist_of_string (List.hd l) in
			if ((List.length l) = 1) 
			then ( 
				let report = get_report fr sc in
				print_endline ""; 
				print_endline report
			) else (
				let positions = List.map float_of_string (words (List.hd (List.tl l)) ) in
				ignore (get_lp fr sc positions)
			)
		)
