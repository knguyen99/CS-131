%  predicate tower takes in 3 input
% N, a nonnegative integer specifying the size of the square grid
% T, a list of N lists
% C, a structure with function symbol counts and arity 4
% sets constraints first, then finds solution
tower(N, T, C) :-
	C = counts(Top, Bottom, Left, Right),
	% check lengths of T and C
	length(T,N),
	check_columns_length(T,N),
	length(Top, N),
	length(Bottom, N),
	length(Left, N),
	length(Right, N),
	% check correct numbers in T
	check_numbers(T,N),
	transpose(T, TransT),
	check_numbers(TransT,N),
	%create solution
	maplist(fd_labeling,T),
	%check solution
	check_height(T,Left,Right),
	check_height(TransT, Top, Bottom).

% check each column if correct length
check_columns_length([],_).
check_columns_length([H | T],N) :-
	length(H,N),
	check_columns_length(T,N).

check_numbers(T,N) :-
	limit_valid(T,N),
	maplist(fd_all_different,T).

% set domain of each row to 1 - N 
limit_valid([],_).
limit_valid([H | T], N) :-
	fd_domain(H, 1, N),
	limit_valid(T, N).

% This is SWI-prolog's old implementation
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
% also from TA Hint Code
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%checks height of row given direction
check_height([],[], []).
check_height([H | T],[HD | TD], [HR | TR]) :-
	count_height(H,HD,0,0),
	reverse(H,RevH),
	count_height(RevH,HR,0,0),
	check_height(T,TD,TR).

%counts the height of specified row, compares if correct height
count_height([],Count, Count, _).	
count_height([H | T], HD, Count, Highest):-
	H #> Highest, 
	NewCount is Count+1,
	count_height(T,HD, NewCount, H).

count_height([H | T], HD, Count, Highest):-
	H #< Highest,
	count_height(T,HD, Count, Highest).


%plain_tower, same as tower, doesnt use GNU Prolog finite domain solver
% set values to positions first, then check if they work
plain_tower(N, T, C) :-
	C = counts(Top, Bottom, Left, Right),
	% shapes T and C
	length(T,N),
	check_columns_length(T,N),
	length(Top, N),
	length(Bottom, N),
	length(Left, N),
	length(Right, N),
	%sets domain
	within_domain(N,Domain),
	transpose(T,TransT),
	%generates solutions & checks height at same time
	generate(T,N,Domain,Left,Right),
	generate(TransT,N,Domain,Top,Bottom).

%from TA Code
% http://www.gprolog.org/manual/html_node/gprolog033.html
% http://www.gprolog.org/manual/gprolog.html#hevea_default674
% Domain is all the enumerated answers of between(1, N, X)
within_domain(N,Domain) :-
	findall(X, between(1,N,X), Domain).
% also from TA Code, check height immediately
generate([],_,_,[],[]).
generate([H |T],N,Domain, [HD | TD], [HD2 | TD2 ]):-
	permutation(Domain,H),
	count_height2(H,HD,0,0),
	reverse(H, RevH),
	count_height(RevH,HD2,0,0),
	generate(T,N,Domain,TD,TD2).

% same as above, except doesn't use #
count_height2([],Count, Count, _).	
count_height2([H | T], HD, Count, Highest):-
	H > Highest, 
	NewCount is Count+1,
	count_height2(T,HD, NewCount, H).

count_height2([H | T], HD, Count, Highest):-
	H < Highest,
	count_height2(T,HD, Count, Highest).

speedup(X) :-
	statistics(cpu_time,[ST | _]),
	tower(5,_,counts([1,2,2,3,3],[5,2,2,1,3],[1,2,3,2,4],[5,2,1,2,2])),
	statistics(cpu_time,[ET | _]),
	plain_tower(5,_,counts([1,2,2,3,3],[5,2,2,1,3],[1,2,3,2,4],[5,2,1,2,2])),
	statistics(cpu_time,[ET2 | _]),
	TowTim is ET-ST,
	PTTim is ET2-ET,
	X is PTTim/TowTim.

ambiguous(N,C,T1,T2) :-
	tower(N,T1,C),
	tower(N,T2,C),
	T1 \= T2.

