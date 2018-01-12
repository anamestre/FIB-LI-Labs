
% Given a directed graph with N nodes, we want to find a Hamiltonian
% cycle: a cycle visiting each node exactly once.

% 1. Complete the following prolog program to find a Hamiltonian cycle (listed from
% node 1 to node 1) that visits the given "urgent" node as early as possible.
% The example input below should produce output ending with (something like):
%   Optimal solution, with cost 4:
%      1 10 7 13 19 6 17 16 4 8 3 20 18 14 12 15 2 5 9 11 1
% Note: we count positions from 0 to N. Cost=4 because the urgent node (node 19) is at position 4.
% UpLoad via Racó a file named urgent.pl.   Warning: use exactly this (lowercase) name!

% 2. Instead of visiting the urgent node as early as possible, forget about the 
% urgent node and find the SHORTEST cycle, considering that
%  -trajects between nodes with the same parity have length 0 km
%  -trajects between nodes of different parity  have length 1 km
% For example, the following solution has cost 10:
%   1 10 4 8 3 20 18 14 12 11 15 2 5 9 17 16 7 13 19 6 1 
%    ^      ^ ^           ^     ^ ^      ^  ^       ^ ^
% Upload via Racó a file named shortest.pl.   Warning: use exactly this (lowercase) name!

% Upload two independent .pl files, no zip.

%%% input:
numNodes(20).
%urgentNode(19).
adjacency(1,[10]).
adjacency(2,[7,5,17]).  % Three directed edges from node 2:  2->7, 2->5 and 2->17.
adjacency(3,[6,20,11]).
adjacency(4,[14,15,1,8,5]).
adjacency(5,[16,4,9,10,11]).
adjacency(6,[1,13,17]).
adjacency(7,[16,9,13,11]).
adjacency(8,[7,3,10,6,17]).
adjacency(9,[6,17,11]).
adjacency(10,[7,11,5,6,4]).
adjacency(11,[20,8,15,4,1,16,3]).
adjacency(12,[15,11,3]).
adjacency(13,[5,2,19,3,6]).
adjacency(14,[10,12,9,7]).
adjacency(15,[20,1,14,18,12,2]).
adjacency(16,[7,6,4,8,2,10]).
adjacency(17,[20,5,16,3,8]).
adjacency(18,[15,16,7,14,3]).
adjacency(19,[13,6]).
adjacency(20,[12,6,18,7,16]).
%%% end input


%Helpful prolog predicates:
position(P):- numNodes(N), between(0,N,P).
node(I):-     adjacency(I,_).
legalPosition(P):- numNodes(N), N1 is N - 1, between(1, N1, P).

parity(N1, N2, 0):- 0 is N1 mod 2, 0 is N2 mod 2.
parity(N1, N2, 0):- 1 is N1 mod 2, 1 is N2 mod 2.
parity(N1, N2, 1):- 1 is N1 mod 2, 0 is N2 mod 2.
parity(N1, N2, 1):- 0 is N1 mod 2, 1 is N2 mod 2.

%MANDATORY: Use the SAT variable: visited-i-p meaning "node i is visited in position p"

symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

writeClauses(K):-
    init,
    eachNodeExactlyOne,
    noTrajectIfThereIsNoCorrespondingEdge,
    eachPositionAMO,
    %urgentNodeNotAfterPositionK(K),
    shortestPath(K),
    trajectIfThereIsCorrespondingEdge,
    true, !.

    
shortestPath(K):-
    findall(visited-N-P, (node(N), position(P)), Lits),
    sortList(Lits, Sorted),
    sumaParitat(Sorted, Suma),
    Suma =< K, fail.
shortestPath(_).
    
sumaParitat([visited-N1-_, visited-N2-_], Suma):-
    parity(N1, N2, S),
    Suma is S, !.
sumaParitat([visited-N-_, visited-N2-_|Lits], Suma):-
    sumaParitat([visited-N2-_|Lits], Suma2),
    parity(N, N2, S),
    Suma is Suma2 + S.
    
init:- numNodes(N),  
       writeClause([visited-1-0 ]),
       writeClause([visited-1-N]), fail.
init.

eachPositionAMO:-
    position(P),
    findall(visited-N-P, node(N), Lits),
    exactly(1, Lits), fail.
eachPositionAMO.

eachNodeExactlyOne:- 
    node(N),
    N > 1,
    findall(visited-N-P, legalPosition(P), Lits),
    exactly(1, Lits), fail.
eachNodeExactlyOne.

urgentNodeNotAfterPositionK(K):-
    urgentNode(U),
    findall(visited-U-P, position(P), Lits),
    member(visited-U-P, Lits),
    P =< K, fail.
urgentNodeNotAfterPositionK(_).

noTrajectIfThereIsNoCorrespondingEdge:-
    node(N),
    adjacency(N, List),
    node(N1),
    \+member(N1, List),
    position(ActualP),
    NextP is ActualP + 1,
    %legalPosition(NextP),
    position(NextP),
    writeClause([\+visited-N-ActualP, \+visited-N1-NextP]), fail.
noTrajectIfThereIsNoCorrespondingEdge.


trajectIfThereIsCorrespondingEdge:-
    node(N),
    position(ActualP),
    NextP is ActualP + 1,
    legalPosition(NextP),
    adjacency(N, Adj),
    findall(visited-N1-NextP, member(N1, Adj), Lits),
    %expressOr(visited-N-ActualP, Lits), fail.
    writeClause([\+visited-N-ActualP|Lits]), fail.
trajectIfThereIsCorrespondingEdge. 
    
  /*  
trajectIfThereIsCorrespondingEdge:-
    node(N),
    position(ActualP),
    PrevP is ActualP - 1,
    position(PrevP),
    node(N1),
    adjacency(N1, Adj),
    findall(visited-N1-PrevP, member(N, Adj), Lits),
    expressOr(visited-N-ActualP, Lits), fail.
trajectIfThereIsCorrespondingEdge.*/

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog predicates to be implemented:    

% Given a model M, Cost is its cost:
cost(M,Cost):- 
    %urgentNode(U),
    %member(visited-U-Cost, M), !.
    findall(visited-N-P, member(visited-N-P, M), Lits),
    sortList(Lits, Sorted),
    sumaParitat(Sorted, Cost).

%show the solution:
displaySol(M):- 
    findall(visited-I-P, member(visited-I-P, M), Lits),
    sortList(Lits, Sorted),
    printLits(Sorted), fail.
displaySol(_).
    
sortList([], _):- !.
sortList([visited-N-P|Lits], Sorted):-
    P1 is P + 1,
    nth1(P1, Sorted, N),
    sortList(Lits, Sorted).
    
printLits([]):- !.
printLits([X|Lits]):-
    write(X), write(' '),
    printLits(Lits).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN:

main:-  symbolicOutput(1), !, numNodes(N), writeClauses(N), halt.   % print the clauses in symbolic form and halt
main:- initClauseGeneration, numNodes(NumNodes),
    nl, write('Looking for initial solution with cost '), write(NumNodes), write('...'), nl,
    tell(clauses), writeClauses(NumNodes), told, % generate the (numeric) SAT clauses and call the solver
    tell(header),  writeHeader,  told,
%    numVars(N), numClauses(C), 
%    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,[]),!.

treatResult(20,[]       ):- write('No solution exists.'), nl, halt.
treatResult(20,BestModel):- nl,nl,cost(BestModel,K),
			    write('Optimal solution, with cost '), write(K), write(':'), nl, displaySol(BestModel), nl,nl, halt.
treatResult(10,_):- %   shell('cat model',_),	
    see(model), symbolicModel(M), seen,  
    cost(M,K),
    write('Solution found with cost '), write(K), write(':'), nl,
    K1 is K-1,
    displaySol(M),nl,
    write('Looking for solution with cost '), write(K1), write('...'), nl,
    initClauseGeneration,
    tell(clauses), writeClauses(K1), told,
    tell(header),  writeHeader,  told,
%    numVars(N),numClauses(C),nl,
%    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,M),!.
    
:-dynamic(varNumber/3).

initClauseGeneration:-  %initialize all info about variables and clauses:
    retractall(numClauses(   _)), 
    retractall(numVars(      _)), 
    retractall(varNumber(_,_,_)),
    assert(numClauses( 0 )), 
    assert(numVars(    0 )),     !.


writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w( Lit ):- symbolicOutput(1), write(Lit), write(' '),!.
w(\+Var):- var2num(Var,N), write(-), write(N), write(' '),!.
w(  Var):- var2num(Var,N),           write(N), write(' '),!.


% given the symbolic variable V, find its variable number N in the SAT solver:
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.
 
% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% Cardinality constraints on arbitrary sets of literals Lits:
% For example the following generates the clauses expressing that 
%     exactly K literals of the list Lits are true:
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
    negateAll(Lits,NLits), 
    K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
    length(Lits,N),
    K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate(\+Lit,  Lit):-!.
negate(  Lit,\+Lit):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits ):- negate(Var,NVar), member(Lit,Lits),  writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits ):- negateAll(Lits,NLits), writeClause([ Var | NLits ]),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
