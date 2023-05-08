DOMAINS 
	n = integer.
	
PREDICATES
	rectFactorial(n, n, n).
	factorial(n, n).
	
	rectFibonachi(n, n, n, n)
	fibonachi(n, n).
	
CLAUSES
	rectFactorial(N, TMP, RESULT) :- N <= 1, RESULT = TMP, !.
	rectFactorial(N, TMP, RESULT) :- 
		NextN = N - 1,
		NextTMP = TMP * N,
		rectFactorial(NextN, NextTMP, RESULT).

	factorial(N, RESULT) :- rectFactorial(N, 1, RESULT).
	
	rectFibonachi(0, PPValue, PPValue, _) :- !.
	rectFibonachi(1, PValue, _, PValue) :- !.
	rectFibonachi(-1, PValue, _, PValue) :- !.
		
	rectFibonachi(N, RESULT, PPValue, PValue) :-
		N > 0,
		NextN = N - 1, 
		Value = PValue + PPValue,
		rectFibonachi(NextN, Result, PValue, Value). 
	
	
	rectFibonachi(N, RESULT, PPValue, PValue) :-
		N < 0,
		NextN = N + 1, 
		Value = PValue + PPValue,
		rectFibonachi(NextN, Result, PValue, Value). 
	
	
	fibonachi(N, RESULT) :- N >= 0, rectFibonachi(N, RESULT, 0, 1).
	fibonachi(N, RESULT) :- N < 0, rectFibonachi(N, RESULT, 0, -1).
	%fibonachi(N, RESULT) :- N < 0,
	%			New = -N,
	%			rectFibonachi(New, NewRes, 0, 1),
	%			RESULT = -NewRes.
GOAL
	% factorial(6, RES).
	fibonachi(-10, RES).