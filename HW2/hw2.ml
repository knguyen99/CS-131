type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* from ta hint code *)
let match_term path t = match t with 
	| [] -> Some path
	| x -> None
;;

let rec get_list rules value = match rules with
	| [] -> []
	| head::tail -> 
		if value = (fst head) then (snd head)::(get_list tail value)
		else get_list tail value
;;

let convert_grammar gram1 = match gram1 with 
	| (start_symbol, rules) -> (start_symbol, get_list rules)
;;

let parse_tree_leaves tree = 
	let rec parse_rec tree_list = match tree_list with
		| [] -> []
		| head::tail -> 
			let deeper h = match h with
				| Leaf value -> [value]
				| Node (a,b) -> parse_rec b 
		in (deeper head)@(parse_rec tail)
	in parse_rec [tree]
;;

let rec rec_rules rules_func rules accept frag = match rules with
	| [] -> None
	| head::tail -> match (check_rule (rules_func) head accept frag) with
		| None -> rec_rules (rules_func) tail accept frag
		| x -> x
and check_rule rules_func rule accept frag = match rule with
	| [] -> accept frag
	| head::tail -> match frag with
		| [] -> None
		| frag_head::frag_tail -> match head with
			| N s ->  rec_rules rules_func (rules_func s) (check_rule rules_func tail accept) frag
			| T s -> if s = frag_head then (check_rule (rules_func) (tail) (accept) (frag_tail))
						else None 
;;

let make_matcher gram = match gram with 
	| (start_symbol, rules_func) -> ( fun accept frag -> rec_rules (rules_func) (rules_func start_symbol) accept frag)
;;

let rec prec_rules rules_func sym rules accept path frag = match rules with
	| [] -> None
	| head::tail -> let pcheck_call = pcheck_rule (rules_func) head accept (path@[head]) frag in 
		match pcheck_call with
		| None -> prec_rules (rules_func) sym tail accept path frag
		| _ -> pcheck_call
and pcheck_rule rules_func rule accept path frag = match rule with
	| [] -> accept path frag
	| head::tail -> match frag with
		| [] -> None
		| frag_head::frag_tail -> match head with
			| N s ->  prec_rules rules_func s (rules_func s) (pcheck_rule rules_func tail accept) path frag
			| T s -> if s = frag_head then (pcheck_rule (rules_func) (tail) (accept) path (frag_tail))
						else None 
;;


let rec create_tree root path_func = match root with 
	| [] -> (path_func, [])
	| head::tail -> let deeper_call = deeper_tree head path_func in 
					let create_call = create_tree tail (fst deeper_call) in 
					((fst create_call), (snd deeper_call)::(snd create_call))
and deeper_tree root path_func = match root with 
	| T s -> (path_func, Leaf s)
	| N s -> match path_func with 
		| [] -> ([], Node (s,[]))
		| head::tail -> let create_call = create_tree head tail in 
		((fst create_call), Node (s, (snd create_call)))
;;


let make_parser gram = match gram with
	| (start_symbol, rules_func) -> fun frag ->  match (prec_rules (rules_func) (start_symbol) (rules_func start_symbol) match_term [] frag ) with
		| None -> None
		| Some [] -> None
		| Some x -> match snd (create_tree [N start_symbol] x) with
			| [] -> None
			| head::tail -> Some head
;;