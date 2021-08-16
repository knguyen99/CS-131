
let rec subset a b = match a with
        | [] -> true
        | head::tail ->
                if List.mem head b then subset tail b else false
;;

let equal_sets a b = 
        subset a b && subset b a  
;;

let rec set_diff a b = match a with
	| [] -> []
	| head::tail ->
		if List.mem head b then set_diff tail b else head::(set_diff tail b)
;;	

let set_union a b = a@(set_diff b a);;

let rec set_intersection a b = match a with
        | [] -> []
        | head::tail ->
                if List.mem head b then head::(set_intersection tail b) else set_intersection tail b
;;

let rec computed_fixed_point eq f x = 
	if (eq (f x) x) then x
	else computed_fixed_point (eq) (f) (f (x))
;;

type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

(* let type_filter a = match a with 
	| (N symbol) -> true
	| (T symbol) -> false
;; *)

let get_lt (a,_) = a;;
let get_rt (_,a) = a;;

let get_start g = match g with 
	| (start_symbol,rules) -> (rules, [start_symbol]);;

let equal_second_elem_sets a b = equal_sets (get_rt a) (get_rt b);;

let rec nonterms right_hand_side = match right_hand_side with 
		| [] -> []
		| N head::tail -> head::(nonterms tail)
		| T head::tail ->  nonterms tail
;;

(*get reachable symbols*)
let rec get_reachable_symbols param = match param with 
	| (rules, reachable_symbols) -> match rules with 
		| [] -> (rules, reachable_symbols)
		| tmp_rule::rest_rules -> match tmp_rule with 
			| (tmp_symbol, right_hand_side) ->
				if List.mem tmp_symbol reachable_symbols then get_reachable_symbols (rest_rules, (set_union reachable_symbols (nonterms right_hand_side)))
				else get_reachable_symbols (rest_rules,reachable_symbols)
;;

let level_reachable param = (get_lt param, get_rt (get_reachable_symbols param));;


(*computed fixed point eq*)
let reachable_symbols x = get_rt (computed_fixed_point (equal_second_elem_sets) (level_reachable) (x));;

(*filter rules*)
let filter_rules rules symbols = List.filter (fun x -> List.mem (get_lt x) symbols) rules;;

(* filter rules by reachable symbols*)
let filter_reachable g = match g with 
	| (start_symbol, rules) -> (start_symbol, filter_rules rules (reachable_symbols (get_start g))) 
;;







