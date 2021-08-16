(* subset *)
let my_subset_test0 = subset [] []
let my_subset_test1 = not (subset [1;2] [2;3])

(* equal sets *)
let my_equal_sets_test0 = equal_sets [1;2;2;3] [1;3;2]
let my_equal_sets_test1 = not (equal_sets [1;2;2] [1;2;3])

(* set union *)
let my_set_union_test0 = equal_sets (set_union [1;2] [2;3]) [1;2;3]
let my_set_union_test1 = not (equal_sets (set_union [1;2] [2;3]) [1;2])

(* set intersection *)
let my_set_intersection_test0 = equal_sets (set_intersection [1;4] [4;4;3;2]) [4]
let my_set_intersection_test1 = not (equal_sets (set_intersection [1;2] [2;3]) [1;3])

(* set diff *)
let my_set_diff_test0 = equal_sets (set_diff [1;2;3;4] [2;3]) [1;4]
let my_set_diff_test1 = not (equal_sets (set_diff [1;2;3] [1;2;3]) [1])

(* computed fixed point *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x-> x*x) 1 = 1

type my_nonterminals = | A | B | C

let my_rules = 
	[A, [N B];
	A, [T "D"; T "E"];
	B, [T "F"];
	C, [T "G";T "s"]
	]
let my_grammar = A,my_rules

let my_filter_reachable_test0 = filter_reachable my_grammar = (A,[A, [N B];
	A, [T "D"; T "E"];
	B, [T "F"]])
