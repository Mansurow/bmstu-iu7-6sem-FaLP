% Constraint Logic Programming
:- use_module(library(dif)).	% Sound inequality
:- use_module(library(clpfd)).	% Finite domain constraints
:- use_module(library(clpb)).	% Boolean constraints
:- use_module(library(chr)).	% Constraint Handling Rules
:- use_module(library(when)).	% Coroutining
%:- use_module(library(clpq)).  % Constraints over rational numbers

helperFibonacci(0, PPValue, PPValue, _) :- !.
helperFibonacci(1, PValue, _, PValue) :- !.
helperFibonacci(Num, Result, PPValue, PValue) :-   
                                            NextNum #= Num - 1,
                                            Value #= PPValue + PValue,
                                            helperFibonacci(NextNum, Result, PValue, Value).
              
fibonacci(Num, Res) :-
                        Num #>= 0,
                        helperFibonacci(Num, Res, 0, 1).

fibonacci(Num, Res) :-
                        Num #< 0,
                        NewNum #= -Num,
                        NewRes #= -Res,
                        helperFibonacci(NewNum, NewRes, 0, 1).


