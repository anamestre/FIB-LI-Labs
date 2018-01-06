
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%% CONSTRAINT LOGIC PROGRAMMING %%%%%%%%%%%%%%%%%%%%%
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

                
                
% Fall 2013
% We have three dice (a die in Spanish is “dado”, and the plural of die is dice). They are fair (each one of their six sides has the same probability of coming up) 
% and their sides have numbers between 1 and 9 (not between 1 and 6!). Now suppose we play a game (many times): I pick a die; after that, you pick another die, 
% we roll both dice, and the player who gets the highest number receives one Euro from the other player. Can you design the dice (putting the numbers on them) 
% in such a way that you can become rich, that is, so that you can always pick a die that is better than mine (here better means that it wins with probability p > 0.5)? 
% Write a Prolog program that checks whether this is possible or not. Include all non-predefined predicates you use. To make the problem easier, assume that die A 
% has number A1 on two of its sides, A2 on two sides and A3 on two sides, and similarly, die B has B1, B2, B3 and die C has C1, C2 and C3 (each number on two sides), 
% where all nine numbers A1,A2,A3, B1,B2,B3, C1,C2,C3 are different and between 1 and 9. Also note that die A is better than die B if A wins in at least five of the nine 
% possible outcomes (A1,B1),(A1,B2),...,(A3,B3), and that you have to make die A better than die B, die B better than C, and C better than A.


dice:- permutation([1, 2, 3, 4, 5, 6, 7, 8, 9], [A1, A2, A3,  B1, B2, B3,  C1, C2, C3]),
            better([A1, A2, A3], [B1, B2, B3]),
            better([B1, B2, B3], [C1, C2, C3]),
            better([C1, C2, C3], [A1, A2, A3]),
            write([A1, A2, A3] - [B1, B2, B3] -[C1, C2, C3]), nl.
            
better(A, B):- 
            findall(X-Y, (member(X, A), member(Y, B), X > Y), Lits),
            length(Lits, N),
            N >= 5.
            

            
% Spring 2013
% Suppose we describe undirected graphs in Prolog using predicates as in the following example:
/*
vertices([1,2,3,4]).
edge(1,2).
edge(1,4),
edge(2,3),
edge(3,4),
nonedge(1,3).
nonedge(2,4). 

Program in Prolog a new predicate tree(T) which means that T is a subset of the vertices that forms a tree: all vertices in T are connected but there are no cycles within T. 
Also give all auxiliary predicates.
*/

vertices([1,2,3,4]).
edge(1,2).
edge(1,4).
edge(2,3).
edge(3,4).
nonedge(1,3).
nonedge(2,4). 

program:- tree(T), write(T).

tree(T):- vertices(V), subset(V, T), 
              connected(T), \+hasCycle(T).
              
              
connected([]):- !.
connected([T|Tree]):- connexion([T], Tree).

connexion(_, []).
connexion(T, Tree):-
            takeOne(T1, Tree, TreeWithoutT1),
            member(T2, T),
            edges(T1, T2), !,
            connexion([T1|T], TreeWithoutT1).

hasCycle(Tree):-
            subset(Tree, SubTree),
            permutation(SubTree, PermSubTree),
            PermSubTree = [First | _],
            isCycle(First, PermSubTree).
           
           
isCycle(First, [Second, Third | Tree]):-
            edges(Second, Third),
            isCycle(First, [Third | Tree]), !.
isCycle(First, [Last]):- edges(First, Last), !.
            
edges(X, Y):- edge(X, Y).
edges(X, Y):- edge(Y, X).

takeOne(T1, Tree, RestTree):-
            append(Part1, [T1|Part2], Tree),
            append(Part1, Part2, RestTree).
            
            
/*            
Fall 2012
John, Paul and Ringo are rich. Here we will count their money in natural numbers that represent millions of Euros. 
They all have more than zero and at most 10. John has at least twice the amount that Ringo has. Paul has at least 3 more than John. Ringo has at least 3.

A: Write all solutions.
B: Write a Gnu Prolog program with finite domain constraint propagation to find all solutions.
C: What are the domains of each variable after fully propagating the constraints (before the labeling)?
*/

beatles:- Beat = [John, Paul, Ringo],
               Beat ins 1..10,
               John #>= 2*Ringo,
               Paul #>= John + 3,
               Ringo #>= 3,
               label(Beat),
               write(Beat).
               
       
/*
Fall 2011
Now we want to solve problem 2) in Prolog. Assume there are 500,000 users and 500,000 clauses like: friends(3454,[3,7,11,23,37854]). 
meaning that (all) the friends of user 3454 are 3,7,11,23 and 37854.

    a) Define a predicate list200(L) that can generate in L under backtracking all lists of 200 different users (200 numbers in 1 . . . 500,000).
    b) Define a predicate friendsforever that writes a list of 200 friends of each other, if it exists.
    c) This predicate is called friendsforever because it may run for a long time (almost forever). Modify your program to make it faster.

*/

 % %%%%%%%%%%% A %%%%%%%%%%%%%%%%

list200(L):- getList(500000, List), 
                  subset(200, List, L).


getList(0, []):- !.
getList(Num, [Num|List]):- 
                Num1 is Num - 1,
                getList(Num1, List).

subset(0, _, []):- !.
subset(Num, [L|List1], [L|Final]):- 
                Num1 is Num - 1,
                subset(Num1, List1, Final).
subset(Num, [_|List1], Final):-
                subset(Num, List1, Final).
                
                
% %%%%%%%%%%% B %%%%%%%%%%%%%%%%

friendsforever:- list200(L), allfriends(L, L), write(L).

allfriends([]).
allfriends([L|List], AllFriends):- 
                isFriend(L, AllFriends),
                allfriends(List).
                
isFriend(_, []):- !.
isFriend(L, [L1|List]):-
                friends(L, Lfriends),
                member(L1, Lfriends),
                isFriend(L, List).
                
                
% %%%%%%%%%%% C %%%%%%%%%%%%%%%%

friendsforever2:- 
                friends(P, FriendsP),
                ff([P], [P|FriendsP], List),
                write(List).
                
                
ff(List, _, List).
ff(List, I, L1):-
                member(X, I), \+member(X, List),
                friends(X, FriendsX), 
                isSubset(List, FriendsX), % és a dir, "si també em té ell com a amic".
                intersection(I, [X|FriendsX], I1),
                length(I1, K),
                K >= 200,
                ff([X|L], I1, L1).
                
                
intersection([], _, []).
intersection([X|L], L1, [X|R]):- member(X, L1), !, intersection(L, L1, R).
intersection([_|L], L1, R):- intersection(L, L1, R).


isSubset([], _).
isSubset([L|List], L2):- member(L, L2), isSubset(List, L2).