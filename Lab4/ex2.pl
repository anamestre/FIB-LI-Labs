:-dynamic(varNumber / 3).
symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%% We want to schedule the nurses working in a hospital.
%% For each hour, we know the minimum number (each day the same) of nurses needed.
%% Each nurse every day has the same schedule: start working XX:00, then work for 
%% three consecutive hours, rest for one or two hours, and then work three
%% hours again, for example: work 23:00-02:00, rest 2:00-3:00, work 3:00-6:00.
%% Also, each nurse has a range of hours (s)he cannot work.
%% Find working hours for each nurse given all constraints.


%%%% Input example: %%%%%%%%%%%%%%%%%%%%%%%%%%%

nurseIDandBlocking(nurse01,  6, 14). % cannot work between 06:00 and 14:00
nurseIDandBlocking(nurse02,  6, 14).
nurseIDandBlocking(nurse03,  6, 14).
nurseIDandBlocking(nurse04, 14, 22).
nurseIDandBlocking(nurse05, 14, 22).
nurseIDandBlocking(nurse06, 14, 22).
nurseIDandBlocking(nurse07, 22,  6). % cannot work between 22:00 and 06:00
nurseIDandBlocking(nurse08, 22,  6).
nurseIDandBlocking(nurse09, 22,  6).
nurseIDandBlocking(nurse10, 22,  6).
nurseIDandBlocking(nurse11,  6, 14).
nurseIDandBlocking(nurse12,  6, 14).
nurseIDandBlocking(nurse13,  6, 14).
nurseIDandBlocking(nurse14, 14, 22).
incompatible(nurse02,nurse03).
incompatible(nurse11,nurse12).

% each H-N means "N nurses needed during the hour starting at H".  E.g. 17-3: 3 nurses in 17:00-18:00.
needs([ 0-1, 1-2, 2-1, 3-1, 4-2, 5-2, 6-3, 7-3, 8-4, 9-4,10-4,11-3,
       12-3,13-3,14-3,15-4,16-3,17-3,18-3,19-4,20-3,21-2,22-2,23-1]).

%%%% End Input example. %%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%% We will be using the following SAT variables:
% startsNH-N-H         "nurse N starts at hour H"  (N is a nurseID and H is an hour (in 0..23).
% worksNH-N-H          "nurse N works  at hour H"
% nurseType-N-Type     "nurse N has type T"        (if Type=1: WWWRWWW, if Type=2: WWWRRWWW)

%%%%%%%%% Some helpful definitions to make the code cleaner:

nurse(N):- nurseIDandBlocking(N,_,_).
hour(H):- between(0,23,H).
type(T):- between(1,2,T).
needed(H,N):- needs(L), member(H-N,L).

% Em retorna a quina hora estara treballant cada infermer segons el seu tipus de torn i segons a quina hora comença.
workingHourForTypeAndStartH(1,StartH,H):- between(0,2,I), H is (StartH+I) mod 24. %Type=1: WWWRWWW
workingHourForTypeAndStartH(1,StartH,H):- between(4,6,I), H is (StartH+I) mod 24.
workingHourForTypeAndStartH(2,StartH,H):- between(0,2,I), H is (StartH+I) mod 24. %Type=2: WWWRRWWW
workingHourForTypeAndStartH(2,StartH,H):- between(5,7,I), H is (StartH+I) mod 24.

% Controla que sigui una hora correcta entre Start i End.
hourInRange(Start,End,H):- Start<End, End1 is End-1,    between(Start,End1,H).
hourInRange(Start,End,H):- Start>End, End1 is End+24-1, between(Start,End1,H1), H is H1 mod 24.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ------------ WRITE CLAUSES ------------ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeClauses:-
    eachHourAtLeastKnurses,
    nursesCantWorkWithinBlockedHours,
    ifStartsThenWorks,
    restingHours,
    eachNurseHasAtype,
    eachNurseStartsOnce,
    incompatibleNurses,
    true.
    
% A cada hora es necessita un número mínim d'infermers segons needs([...]).
eachHourAtLeastKnurses:- hour(H), needed(H, K), findall(worksNH-N-H, nurse(N), Lits), atLeast(K, Lits), fail.
eachHourAtLeastKnurses.


% Els infermers no poden treballar dins d'un rang d'hores.
nursesCantWorkWithinBlockedHours:- nurseIDandBlocking(N, Bstart, Bend),
                                   hourInRange(Bstart, Bend, H),
                                   negate(worksNH-N-H, NegLit), writeClause([NegLit]), fail.
nursesCantWorkWithinBlockedHours.


% Cada infermer té exactament un tipus d'horari.
eachNurseHasAtype:- nurse(N), findall(nurseType-N-T, type(T), Lits), exactly(1, Lits), fail.
eachNurseHasAtype.

% Cada infermer comença només un cop.
eachNurseStartsOnce:- nurse(N), findall(startsNH-N-H, hour(H), Lits), exactly(1, Lits), fail.
eachNurseStartsOnce.


% Cada infermer ha de seguir el seu tipus d'horari. (1 o 2 hores de descans)
restingHours:- nurse(N), hour(Hstart), type(T), hour(HnotWork), \+workingHourForTypeAndStartH(T, Hstart, HnotWork),
               writeClause([\+startsNH-N-Hstart, \+nurseType-N-T, \+worksNH-N-HnotWork]), fail.
restingHours.  

ifStartsThenWorks:- nurse(N), hour(Hstart), type(T), hour(H), 
                    workingHourForTypeAndStartH(T, Hstart, H),
                    writeClause([\+startsNH-N-Hstart, \+nurseType-N-T, worksNH-N-H]), fail.
ifStartsThenWorks.

incompatibleNurses:- nurse(N1), nurse(N2), incompatible(N1, N2),
                     hour(H), writeClause([\+worksNH-N1-H, \+worksNH-N2-H]), fail.
incompatibleNurses.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ------------ DISPLAY SOL ------------ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
displaySol(M):- nl,write('       00-01-02-03-04-05-06-07-08-09-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24'),nl,
                nurse(N), format('~n~s:',[N]), hour(H), writeHour(M,N,H), fail.
displaySol(M):- nl,write('Total:  '),hour(H),findall(N,member(worksNH-N-H,M),L),length(L,K),format(' ~d ',[K]),fail.
displaySol(_):- nl,write('Needed: '),hour(H),needed(H,N),                                   format(' ~d ',[N]),fail.
displaySol(_).

writeHour(M,N,H):- member(worksNH-N-H,M), write(' X '),!.
writeHour(_,_,_):-                               write('   '),!.









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

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


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
tell(header),  writeHeader,  told,
numVars(N), numClauses(C),
write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
shell('cat header clauses > infile.cnf',_),
write('Calling solver....'), nl,
shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
	treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.

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
