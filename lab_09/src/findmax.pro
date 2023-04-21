DOMAINS
	number = integer.

PREDICATES
	max2(number, number, number).
	max2_cut(number, number, number).
	max3(number, number, number, number).
	max3_cut(number, number, number, number).

CLAUSES
	max2(N1, N2, N2) :- N2 >= N1.
	max2(N1, N2, N1) :- N1 >= N2.
	
	max2_cut(N1, N2, N2) :- N2 >= N1, !.
	max2_cut(N1, _, N1).
	
	max3(N1, N2, N3, N3) :- N3 >= N1, N3 >= N2.
  	max3(N1, N2, N3, N2) :- N2 >= N1, N2 >= N3.
  	max3(N1, N2, N3, N1) :- N1 >= N2, N1 >= N3.

  	max3_cut(N1, N2, N3, N3) :- N3 >= N2, N3 >= N1, !.
  	max3_cut(N1, N2, _, N2) :- N2 >= N1, !.
  	max3_cut(N1, _, _, N1).	
 
GOAL
	% max2(-10, 20, RES).
	% max2_cut(-10, 20, RES).
	
	% max2(20, -20, RES).
	% max2_cut(20, -20, RES). 
	
	% max3(1, 2, 3, RES).
	% max3(3, 2 ,1, RES).
	% max3(1, 3, 2, RES).
	
	% max3_cut(1, 2, 3, RES).
	% max3_cut(3, 2 ,1, RES).
	max3_cut(1, 3, 2, RES).