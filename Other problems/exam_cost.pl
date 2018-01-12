

% The Swiss Telephone Company (STC) needs to place new 5G antenna
% stations to cover ALL cities of Switzerland.  STC has a long list
% of possible antenna stations. Each city has a (non-empty) list of
% locations covering it.

% Case 1. Complete the following program to compute the MINIMAL number of
% stations needed.
% The example input below should produce output ending with an ordered
% list of stations used, like this:
%    Optimal solution, with cost 7:
%    6 7 8 10 14 16 19 
% UpLoad via Racó a file named stations.pl.   Warning: use exactly this (lowercase) name!

% Case 2. Extend CASE 1 as follows: the following important cities 1,5,10,20 must be covered by
% at least two stations.
% UpLoad via Racó a file named important.pl.   Warning: use exactly this (lowercase) name!

% Case 3: Extend CASE 2 as follows: instead of minimizing the NUMBER of stations used, now
% minimize THE SUM OF COSTS of all stations used, considering that stations 1-5 cost 2
% million Euro each, stations 11-20 cost 1 million Euro each, and stations 6-10 are free (0 Euro).
% Hint: for expensive stations such as station 5, introduce two cost variables cost-5-1, cost-5-2.
% UpLoad via Racó a file named costs.pl.  Warning: use exactly this (lowercase) name!

important(1).
important(5).
important(10).
important(20).

costCity(1, 2).
costCity(2, 2).
costCity(3, 2).
costCity(4, 2).
costCity(5, 2).
    
costCity(6, 0).
costCity(7, 0).
costCity(8, 0).
costCity(9, 0).
costCity(10, 0).
    
costCity(11, 1).
costCity(12, 1).
costCity(13, 1).
costCity(14, 1).
costCity(15, 1).
costCity(16, 1).
costCity(17, 1).
costCity(18, 1).
costCity(19, 1).
costCity(20, 1).


%input
numStations(20).
city(1,[12,8,1,5,6,9]).
city(2,[6,14,17,10,2]).
city(3,[12,8,2]).
city(4,[5,20,16]).
city(5,[19,11,12,2,1]).
city(6,[16,10,3,5,13]).
city(7,[5,13,19]).
city(8,[17,14,15,8,5]).
city(9,[13,14,6,11,19]).
city(10,[5,10,19]).
city(11,[1,5,8]).
city(12,[12,20,18,7]).
city(13,[7,3,16]).
city(14,[15,3,10,17]).
city(15,[8,3,1]).
city(16,[18,1,7]).
city(17,[10,5,19]).
city(18,[7,19,8,1,4]).
city(19,[17,9,20,7,13]).
city(20,[14,10,19]).
city(21,[6,5,3,15,19,20]).
city(22,[16,20,15,17]).
city(23,[17,6,20]).
city(24,[17,16,8,13,12,19]).
city(25,[8,11,5,2,6]).
city(26,[12,8,19]).
city(27,[19,17,20,18,14]).
city(28,[14,5,9,11,18,20]).
city(29,[7,1,11,12]).
city(30,[5,6,14,11]).
city(31,[10,2,6,9,15,13,20]).
city(32,[19,7,11,16,12]).
city(33,[11,6,9,17]).
city(34,[4,19,5,3,1,17,12]).
city(35,[18,14,12,3,16,1]).
city(36,[14,19,4]).
city(37,[7,2,14,4]).
city(38,[4,7,5,3,14]).
city(39,[18,4,16,15]).
city(40,[10,8,9]).
city(41,[6,13,1,4]).
city(42,[11,19,2]).
city(43,[1,2,17,12,16]).
city(44,[13,4,5,8,1]).
city(45,[17,20,15,3,14,4]).
city(46,[18,10,2,16]).
city(47,[1,11,16,2,20]).
city(48,[6,1,16]).
city(49,[9,5,19,11]).
city(50,[10,19,4,6]).
% end input


:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

writeClauses(K):-
    eachCityAtLeastOneStation,
    %noMoreThanKstations(K), 
    minimizeCost(K),
    ifUsedThenCost,
    ifNotUsedThenCost,
    importantCities, !.

% Variable: cityStation-I-S
    
eachCityAtLeastOneStation:- 
    city(_, List),
    atLeast(1, List), fail.
eachCityAtLeastOneStation.

ifUsedThenCost:-
    numStations(N),
    between(1, N, X),
    costCity(X, Cost),
    Cost == 2,
    writeClause([\+X, cost-X-1]),
    writeClause([\+X, cost-X-2]), fail.
ifUsedThenCost:-
    numStations(N),
    between(1, N, X),
    costCity(X, Cost),
    Cost == 1,
    writeClause([\+X, cost-X-1]), fail.
ifUsedThenCost.

ifNotUsedThenCost:-
    numStations(N),
    between(1, N, X),
    costCity(X, Cost),
    Cost == 2,
    writeClause([X, \+cost-X-1]),
    writeClause([X, \+cost-X-2]), fail.
ifNotUsedThenCost:-
    numStations(N),
    between(1, N, X),
    costCity(X, Cost),
    Cost == 1,
    writeClause([X, \+cost-X-1]),fail.
ifNotUsedThenCost.

minimizeCost(K):-
    findall(X, costCity(X, 1), Cost1),
    findall(cost-X1-1, member(X1, Cost1), Lits1),
    
    findall(Y, costCity(Y, 2), Cost2),
    findall(cost-Y1-1, member(Y1, Cost2), Lits2a),
    findall(cost-Y2-2, member(Y2, Cost2), Lits2b),
    
    append(Lits2a, Lits2b, LitsFinal),
    append(LitsFinal, Lits1, AllCost),
    atMost(K, AllCost), fail.
minimizeCost(_).
    
noMoreThanKstations(K):- 
    numStations(Num),
    findall(X, between(1, Num, X), Lits),
    atMost(K, Lits), fail.
noMoreThanKstations(_).

importantCities:-
    important(C),
    city(C, Lits),
    atLeast(2, Lits), fail.
importantCities.


% Prolog predicates to be implemented:
%Given a model M, Cost is its cost:
cost(M,Cost):-
    findall(X, (member(X, M), city(X, _)), Lits),
    sumCost(Lits, Cost), true.
    
sumCost([], 0):- !.
sumCost([C|Cities], Cost):-
    costCity(C, C1),
    sumCost(Cities, Cost2),
    Cost is Cost2 + C1.
    
%Show the solution:	
displaySol(M):-
    findall(X, (member(cost-X-_, M), city(X,_)), Lits),
    sort(Lits, Lits2),
    print(Lits2),
    fail.
displaySol(_):- nl.

print([]):- !.
print([S|Stations]):-
    write(S), write(' '),
    print(Stations).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN:

main:-  symbolicOutput(1), !, numStations(N), writeClauses(N), halt.   % print the clauses in symbolic form and halt
main:- initClauseGeneration, numStations(NumNodes),
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
