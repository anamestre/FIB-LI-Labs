% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%% CONSTRAINT LOGIC PROGRAMMING %%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Spring 2016
% Complete the following graph coloring program (see problem 2). 
% Do makeConstraints recursively, using #\= and the built-in predicate nth1(I,L,X) 
% (“the Ith element of the list L is X”).

numVertices(5).
edges([1-2, 1-3, 2-3, 2-4, 2-5, 3-5]).
numColors(3).

main:- 	numVertices(N), edges(Edges), 
	   	listOfNPrologVars(N,Vars), numVertices(K),
		Vars ins 1..K,
		makeConstraints(Edges,Vars),
    	label(Vars),
		write(Vars), nl.

makeConstraints([], _).
makeConstraints([X-Y|Edges], Vars):- nth1(X, Vars, ColorX),
    								 nth1(Y, Vars, ColorY),
    								 ColorX #\= ColorY,
    								 makeConstraints(Edges, Vars).
                
listOfNPrologVars(N, Vars):- length(Vars, N).




% Spring 2015
% Write a Prolog predicate shortest([I1,J1], [I2,J2]) that writes to the output the shortest path
% on a chess board a horse needs to go from square [I1,J2] to square[I2,J2]. 
% Coordinates I,J are in 1..8. The path is the list of intermediate board squares. 
% Your solution should be short, clean and simple, without any comments.


shortest([I1,J1], [I2,J2]):- path([I1, J1], [I2, J2], [[I1, J1]], Path),
    						 between(1, 64, N),
    						 length(Path, N), % Si només tenim 64 caselles, només pot passar per aquestes.
    						 write(Path), nl.

path(F, F, Path, Path):- !.
path(From, To, UntilNow, TotalPath):- oneStep(From, NextStep),
    								  \+member(NextStep, UntilNow),
    								  path(NextStep, To, [NextStep|UntilNow], TotalPath).

oneStep([FromX, FromY], [ToX, ToY]):- member([StepX, StepY], [[1, 2], [2, 1]]),
    								  member(SignX, [1, -1]),
    								  member(SingY, [1, -1]),
    								  ToX is SignX * StepX,
    								  ToY is SignY * StepY,
    								  between(1, 8, ToX),
    								  between(1, 8, ToY).
