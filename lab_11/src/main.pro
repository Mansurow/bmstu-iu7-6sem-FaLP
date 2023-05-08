DOMAINS
	list = integer*
	length, sum, filter = integer
PREDICATES
	length(list, length).
	rlength(list, length, length).
	
	sum(list, sum).
	rsum(list, sum, sum).
	
	sumOfOddPos(list, sum).
	rSumOfOddPos(list, sum, sum).
	
	sumOfEvenPos(list, sum).
	rSumOfEvenPos(list, sum, sum).
	
	filterMoreThen(list, filter, list).
	removeElem(list, filter, list).
	append(list, list, list).
CLAUSES
	% Length
	rlength([], Current, Current) :- !. 
	rlength([_|Tail], Result, Current) :- 
		Next = Current + 1,
		rlength(Tail, Result, Next). 
	
	length(List, Length) :-
		rlength(List, Length, 0).
	
	% Sum of list integer
	rsum([], Current, Current) :- !.
	rsum([Head|Tail], Result, Current) :-
		Next = Current + Head,
		rsum(Tail, Result, Next).
	
	sum(List, Result) :-
		rsum(List, Result, 0).	
	
	% Sum of list odd position integer
	rSumOfOddPos([], TempSum, TempSum) :- !.
	rSumOfOddPos([_], TempSum, TempSum) :- !.
	rSumOfOddPos([_, Head|Tail], Sum, TempSum) :-
		Next = TempSum + Head,
		rSumOfOddPos(Tail, Sum, Next).
	
	sumOfOddPos(List, Result) :- 
		rSumOfOddPos(List, Result, 0).
		
	% Sum of list even position integer
	rSumOfEvenPos([], TempSum, TempSum) :- !.
	rSumOfEvenPos([Head], Sum, TempSum) :- Sum = TempSum + Head, !.
	rSumOfEvenPos([Head, _|Tail], Sum, TempSum) :-
		Next = TempSum + Head,
		rSumOfEvenPos(Tail, Sum, Next).
	
	sumOfEvenPos(List, Sum) :- 
		rSumOfEvenPos(List, Sum, 0).
	
	% filter elem from list more then filter number
	filterMoreThen([], _, []).
	filterMoreThen([Head|Tail], Filter, [Head|ResultTail]) :-
		Head > Filter, filterMoreThen(Tail, Filter, ResultTail).
	filterMoreThen([Head|Tail], Filter, Result) :-
		Head <= Filter, filterMoreThen(Tail, Filter, Result). 
		
	% remove the specified item from the list delete
	removeElem([], _, []).
	removeElem([Elem|Tail], Elem, Tail) :- !.
	removeElem([Head|Tail], Elem, [Head|ResultTail]) :-
		removeElem(Tail, Elem, ResultTail).
		
	% join two list 
	append([], List, List) :- !.
	append([Head|Tail], List, [Head|ResultTail]) :- 
		append(Tail, List, ResultTail).
			 		
GOAL 
	% length([1, 2, 3], R).
	% sum([1, 2, 3], R).
	% sumOfOddPos([1, 2, 3, 4, 5], R).
	% sumOfEvenPos([1, 2, 3, 4, 5], R).
	% filterMoreThen([1, 5, 3, 1, 10, 6], 4, R).
	% removeElem([1, 2, 3, 4, 5], 10, R).
	append([1, 2, 3], [4, 5, 6], R).