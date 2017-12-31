
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%% CONSTRAINT LOGIC PROGRAMMING %%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).



% Fall 2016
% Write a program return(L,A) in swi prolog (using library(clpfd) or not, feel free) that 
% outputs (writes) the minimal number of coins needed for returning the amount A if (infinitely many) 
% coins of each value of the list L are available. Two examples:

% ?-return([1,5,6],10). 		  % writes 2 (since 2*5 = 10).
% ?-return([1,2,5,13,17,35,157],361).  % writes 5 (since 1*13 + 2*17 + 2*157 = 361).

return(L, A):- length(L, Size), length(Vars, Size),
                    Vars ins 0..A,
                    sum(Vars, NumCoins), % Quantitat de monedes
                    totalValueExp(Vars, L, Value),
                    Value #= A,
                    labeling([min(NumCoins)], Vars),
                    S is NumCoins, write(S), nl, !.

sum([], 0):- !.
sum([V|Vars], Num):- sum(Vars, Num1), Num is V + Num1.

totalValueExp([], [], 0):- !.
totalValueExp([V|Vs], [C|Cs], V*C + Expr):- totalValueExp(Vs, Cs, Expr), !.




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
makeConstraints([X-Y|Edges], Vars):- 
                nth1(X, Vars, ColorX),
                nth1(Y, Vars, ColorY),
                ColorX #\= ColorY,
                makeConstraints(Edges, Vars).
                
listOfNPrologVars(N, Vars):- length(Vars, N).




% Spring 2015
% Write a Prolog predicate shortest([I1,J1], [I2,J2]) that writes to the output the shortest path
% on a chess board a horse needs to go from square [I1,J2] to square[I2,J2]. 
% Coordinates I,J are in 1..8. The path is the list of intermediate board squares. 
% Your solution should be short, clean and simple, without any comments.


shortest([I1,J1], [I2,J2]):- 
                path([I1, J1], [I2, J2], [[I1, J1]], Path),
                between(1, 64, N),
                length(Path, N), % Si només tenim 64 caselles, només pot passar per aquestes.
                write(Path), nl.

path(F, F, Path, Path):- !.
path(From, To, UntilNow, TotalPath):- 
                oneStep(From, NextStep),
                \+member(NextStep, UntilNow),
                path(NextStep, To, [NextStep|UntilNow], TotalPath).

oneStep([FromX, FromY], [ToX, ToY]):- 
                member([StepX, StepY], [[1, 2], [2, 1]]),
                member(SignX, [1, -1]),
                member(SignY, [1, -1]),
                ToX is SignX * StepX + FromX,
                ToY is SignY * StepY + FromY,
                between(1, 8, ToX),
                between(1, 8, ToY).




% Fall 2014
% Consider two groups of 10 people each. In the first group, as expected, the percentage of people with lung 
% cancer among smokers is higher than among non-smokers. In the second group, the same is the case. 
% But if we consider the 20 people of the two groups together, then the situation is the opposite: 
% the proportion of people with lung cancer is higher among non-smokers than among smokers! 
% Can this be true? Write a little Prolog program to find it out.

num(X):- between(1, 7, X). 

p:- num(SC1), num(SNC1),	 % Group 1, Smoker with cancer, smoker without cancer
    num(NSC1), num(NSNC1),	 % Group 1, Non-smoker with cancer, non-smoker without cancer
    10 is SC1 + SNC1 + NSC1 + NSNC1,
    SC1 / (SC1 + SNC1) > NSC1 / (NSC1 + NSNC1),
    num(SC2), num(SNC2), 	% Group 2, Smoker with cancer, smoker without cancer
    num(NSC2), num(NSNC2),	% Group 2, Non-smoker with cancer, non-smoker without cancer
    10 is SC2 + SNC2 + NSC2 + NSNC2,
    SC2 / (SC2 + SNC2) > NSC2 / (NSC2 + NSNC2),
    (SC1 + SC2) / (SC1 + SC2 + SNC1 + SNC2) < (NSC1 + NSC2) / (NSC1 + NSC2 + NSNC1 + NSNC2),
    write([SC1, SNC1, NSC1, NSNC1, SC2, SNC2, NSC2, NSNC2]), nl, fail.

    
% Spring 2014
% Consider an n × n chessboard (for any natural number n > 3, not necessarily 8), where n defined by a Prolog clause boardSize(n). 
% (for example, boardSize(14) if if n = 14). Define a Prolog predicate horse(I1,J1,I2,J2) that writes the shortest possible sequence of 
% positions that a horse of chess traverses to go from initial position I1,J1 to final position I2,J2 on the board (positions are (row,column), 
% each one in 1..n). It must write the sequence in the right order, the first position being [I1,J1], and write “no solution” if no such a sequence exists.

boardSize(8).

horse(I1, J1, I2, J2):- 
                boardSize(N),
                getPath([I1, J1], [I2, J2], [[I1, J1]], VarsPath, N),
                N2 is N * N, 
                between(0, N2, NewN),
                length(VarsPath, NewN),
                write(VarsPath), !.
horse(_, _, _, _):- write('no solution'), !.


getPath(From, From, Final, Final, _).
getPath(From, To, ActualPath, FinalPath, N):-
                nextMove(From, Next, N),
                \+member(Next, ActualPath),
                getPath(Next, To, [Next|ActualPath], FinalPath, N).

                
nextMove([I1-J1], [I2-J2], N):-
                member([StepX, StepY], [[1, 2], [2, 1]]),
                member(SignX, [1, -1]),
                member(SignY, [1, -1]),
                I2 is SignX * StepX + I1,
                J2 is SignY * StepY + J1,
                between(1, N, I2),
                between(1, N, J2).
