type my_nonterminals = 
	| Sentence | Noun | Verb | Adj | NP | VP | Punc

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

let my_grammar = (Sentence, function
	| Sentence -> [[N NP; N VP]]
	| NP -> [[N Noun]; [N Adj; N Noun]; [N Noun; N Punc]]
	| VP -> [[ N Verb; N VP]; [N Verb;]]
	| Noun -> [[T "elephant"];[T "raccoon"];[T "dog"];]
	| Verb -> [[T "yells"];[T "jumps"];[T "cries"]]
	| Adj -> [[T "crazy"];[T "short"]]
	| Punc -> [[T "!"]]);;

let match_test = (make_matcher my_grammar accept_all ["short";"elephant";"jumps"] = Some [])

let parse_test = (match make_parser my_grammar ["short";"elephant";"jumps"] with
					| Some tree -> parse_tree_leaves tree = ["short";"elephant";"jumps"]
					| _ -> false)