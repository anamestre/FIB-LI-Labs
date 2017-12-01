%% The mafia has a lot gangsters for doing different tasks.
%% These tasks are planned every 3 days (72h), according to a forecast
%% of the tasks to be done every hour.
%% No gangster can do two different tasks during the same hour or on two consecutive hours.
%% Some gangsters are not available on certain hours.
%% We want to plan all tasks (which gangster does what task when) and
%% we want to find the minimal K such that no gangster works more than
%% K consecutive hours.


%% EXAMPLE OUTPUT:
%% 
%%                       10        20        30        40        50        60        70  
%%               123456789012345678901234567890123456789012345678901234567890123456789012
%% 
%% gangster g01: ------------------------------p-------p-----p----p----p--------------p--
%% gangster g02: ------p--p-p--------------p--ppp-----pp---pp-----p----p---p----p--------
%% gangster g03: -p----p--p-p-p--p-------p-----pp-pp-p-pp-pppp--pp--p-pp-pppp--p-------p-
%% gangster g04: -pp---p--p--pp----pp-p-pp-p-p-pppppp-cc-p--c----pppppp-p-c-p--ppp--p-p--
%% gangster g05: pppppppppppppp--p-pp-p-ppppp-c--cc-p-cc-c-c-p--ppppppp-c-c-pppppppppppp-
%% gangster g06: pp-c--c-c-cc-pp-p-pppppppppp-c--ccc-ccc-c-c-ppppp-cc-p-c-c-pppppp-cc-ppp
%% gangster g07: -c-c-cc-c-cccc--ppp-c-pp-c-cccccccc-ccccccccc--c--ccccccccc-c--cc-cccc-p
%% gangster g08: ccccccccc-cccc-p-c-cccc--c-ccccccccc-k-ccccccc-cccccccc-k-p-c-ccc-ccccc-
%% gangster g09: k--k-c-k-cc-k--cccc-k-ccccc-k-c-k-c-kk-k-c-k-cccc-kk-cc-kk--cccccccccccc
%% gangster g10: k--k-c-k--k-k-cc-kkkk-k--k-kkk-kk-k-kk-k-c-kkkk--kkk-kkkkk-c-kkkk--k--k-
%% gangster g11: k-kkk--k--kkkk-k-kkkk-k-kk-kkkkkkkk-kk-kkk-kkkkkkkkk-kkkkk--kkkkkk-k--kk
%% gangster g12: kkkkkkkkkkkkkkkkkk-kkkkkkkkkkkkkkkkkk-kkkkkkkkkkkkkkkkkk-kkkkkkkkkkkkkkk



%%%%%%%%%%%%%%%%%%%%% INPUT:

% example: 4 gangsters are needed for killing on hour 1, one gangster on hour 2, two gangsters on hour 3, etc.
gangstersNeeded( killing,       [4,1,2,4,2,1,1,4,1,1,3,2,4,2,1,2,1,3,2,3,4,1,3,1,2,3,1,3,4,3,2,3,4,2,3,1,4,4,1,4,2,2,1,4,3,3,3,2,2,3,4,4,1,3,3,3,4,4,1,1,2,3,3,3,3,2,1,3,1,1,3,2] ).
gangstersNeeded( countingMoney, [1,2,1,3,1,4,3,1,3,1,4,3,2,2,1,2,1,2,1,1,2,1,2,1,1,3,1,2,2,4,3,2,4,4,4,1,2,4,4,2,4,4,4,3,2,2,1,3,2,1,3,3,2,3,3,3,1,4,1,1,3,1,2,3,3,1,4,4,3,3,2,1] ).
gangstersNeeded( politics,      [2,4,2,1,1,1,4,1,1,4,1,3,2,4,1,1,4,1,4,3,1,3,2,4,4,2,4,2,1,1,4,3,1,2,2,2,1,1,3,1,1,1,2,2,4,1,1,3,4,4,2,3,2,4,3,1,1,1,3,4,2,2,4,4,3,1,1,2,1,4,3,2] ).

gangsters([g01,g02,g03,g04,g05,g06,g07,g08,g09,g10,g11,g12]).

notAvailable(g01,[6,13,14,16,21,35,37,41,59]).
notAvailable(g02,[14,34,40,45,48,52,58,65,70,72]).
notAvailable(g03,[8,11,13,27,30,38,50,51,70]).
notAvailable(g04,[4,12,16,17,26,30,42,45,48,55,71]).

age(g01,22).
age(g02,25).
age(g03,28).
age(g04,23).
age(g05,41).
age(g06,37).
age(g07,31).
age(g08,58).
age(g09,60).
age(g10,71).
age(g11,84).
age(g12,72).

ageRange(young, 0, 29).         % young       between 0  and 29  (inclusive)
ageRange(middle_aged, 30, 59).  % middle_aged between 30 and 59  (inclusive)
ageRange(old, 60, 100).         % old         between 60 and 100 (inclusive)

getAgeRange(G, Range) :- age(G, Age), ageRange(Range, Min, Max), between(Min, Max, Age), !.
ageRange(R) :- ageRange(R, _, _).

%%%%%%%%%%%%%%%%%%%%% END INPUT. %%%%%%%%%%%%%%%%%%%%%


:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

%%%%%% Some helpful definitions to make the code cleaner:

task(T):-        gangstersNeeded(T,_).
needed(T,H,N):-  gangstersNeeded(T,L), nth1(H,L,N).
gangster(G):-    gangsters(L), member(G,L).
hour(H):-        between(1,72,H).
blocked(G,H):-   notAvailable(G,L), member(H,L).
available(G,H):- hour(H), gangster(G), \+blocked(G,H).


% We use (at least) the following types of symbolic propositional variables:
%   1. does-G-T-H means:  "gangster G does task T at hour H"     (MANDATORY)
%   2. works-G-H means: "gangster G works in some task at hour H"
%   3. involved-R-T: "Range R performs task T"

writeClauses(K) :-
    initClauseGeneration,
    %connectDoesWithWorks, %If there were more tasks, maybe this could optimize the number of clauses. In this case, it's better with just the var type "does"
    eachHourEachGangsterAtMostOneTask,
    noDifferentConsecutiveTasks,
    allTasksDone, 
    limitHours(K),
    limitRanges, 
    fillInvolveds, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

limitRanges :-
    task(T),
    findall(involved-R-T, ageRange(R), Lits),
    atMost(2, Lits), fail.
limitRanges.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fillInvolveds :-
    available(G, H),
    task(T),
    getAgeRange(G, R),
    negate(does-G-T-H, Neg),
    writeClause([Neg, involved-R-T]), fail.
fillInvolveds.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connectDoesWithWorks :-
    available(G, H),
    findall(does-G-T-H, task(T), Lits), 
    expressOr(works-G-H, Lits), fail.
connectDoesWithWorks.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eachHourEachGangsterAtMostOneTask :- 
    available(G,H), 
    findall(does-G-T-H, task(T), Lits), 
    atMost(1, Lits), fail.
eachHourEachGangsterAtMostOneTask.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

noDifferentConsecutiveTasks :-
    available(G, H),
    H_next is H+1,
    hour(H_next),
    available(G, H_next),
    task(T),
    task(T_next),
    T \= T_next,
    Lits = [does-G-T-H, does-G-T_next-H_next],
    negateAll(Lits, NLits),
    writeClause(NLits), fail.
noDifferentConsecutiveTasks.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allTasksDone :-
    needed(T, H, N),
    findall(does-G-T-H, available(G, H), Lits),
    atLeast(N, Lits), fail.
allTasksDone.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

limitHours(K) :-
    gangster(G),
    findall(H, blocked(G, H), L),
    Aux = [0|L],
    append(Aux, [73], Intervals),
    nth0(Pos, Intervals, H_init),
    Next is Pos+1,
    nth0(Next, Intervals, H_end),
    H_end - H_init - 1 >= K,
    limitInterval(G, H_init, H_end, K), fail.
limitHours(_).

limitInterval(G, H_init, H_end, K) :- 
    task(T),
    H_first is H_init + 1,
    H_max is H_end - K - 1,
    between(H_first, H_max, H1),
    H2 is H1 + K,
    findall(does-G-T-H, between(H1, H2, H), Lits),
    negateAll(Lits, NLits),
    atLeast(1, NLits), fail.
limitInterval(_, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

limitHoursSimple(K) :-
    gangster(G),
    task(T),
    hour(H_init),
    H_end is H_init + K,
    hour(H_end),
    findall(does-G-T-H, between(H_init, H_end, H), Lits),
    negateAll(Lits, NLits),
    atLeast(1, NLits), fail.
limitHoursSimple(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxConsecutiveHours(M, K) :-
    between(0, 71, N),
    K is 72-N,
    gangster(G),
    findall(H, member(does-G-_-H, M), Hours),
    sort(Hours, SHours),
    sublist(SHours, L),
    length(L, K),
    [H_init|_] = L,
    last(L, H_end),
    H_end is H_init + K - 1, !.

sublist(L, R) :-
    append(_, S, L),
    append(R, _, S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DISPLAYSOL:

displaySol(M):- nl,nl,
    write('                      10        20        30        40        50        60        70  '), nl,
    write('              123456789012345678901234567890123456789012345678901234567890123456789012'), nl,
    gangster(G), nl, write('gangster '), write(G), write(': '), hour(H), writeIfBusy(G,H,M), fail.
displaySol(M):- nl, nl, printRangesTasks(M), nl, nl, !.

printRangesTasks(M) :-
    task(T),
    write("Task "), write(T), write(":"), printRangesFromTask(M, T), nl, fail.
printRangesTasks(_).

printRangesFromTask(M, T) :- findall(R, member(involved-R-T, M), Ranges), printRanges(Ranges).

printRanges([]).
printRanges([R|Ranges]) :- 
    write(" "),
    write(R),
    printRanges(Ranges).

writeIfBusy(G,H,M):- member(does-G-killing-H,M),       write('k'),!.
writeIfBusy(G,H,M):- member(does-G-countingMoney-H,M), write('c'),!.
writeIfBusy(G,H,M):- member(does-G-politics-H,M),      write('p'),!.
writeIfBusy(_,_,_):- write('-'),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN:

main:-  symbolicOutput(1), !, writeClauses(30), halt.   % print the clauses in symbolic form and halt
main:-
    write('Looking for initial plan allowing arbitrary consecutive hours (72h).'), nl,
    tell(clauses), writeClauses(72), told,     % generate the (numeric) SAT clauses and call the solver
    solve(Result),
    treatResult(Result,[]),!.

treatResult(20,[]):- write('No solution exists.'), nl, halt.
treatResult(20,BestModel):- nl,nl,write('Optimal solution: '),nl, displaySol(BestModel), halt.
treatResult(10,_):- %   shell('cat model',_),   
    see(model), symbolicModel(M), seen,  
    maxConsecutiveHours(M,K),
    write('planning found with at most '), write(K), write(' consecutive hours '),nl,nl,
    displaySol(M),
    K1 is K-1,
    tell(clauses), writeClauses(K1), told,
    solve(Result),
    treatResult(Result,M),!.
    
solve(Result):-
    tell(header),  writeHeader,  told,
    numVars(N),numClauses(C),
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result).  % if sat: Result=10; if unsat: Result=20.

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
    K1 is K+1, subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
    length(Lits,N),
    K1 is N-K+1, subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate(\+Lit,  Lit):-!.
negate(  Lit,\+Lit):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L], [X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L], S):- length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits ):- negate(Var,NVar), member(Lit,Lits),  writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits ):- negateAll(Lits,NLits), writeClause([ Var | NLits ]),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
