
:-include(entradaPacking2).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
3) Queremos extender la práctica de packing de manera que se pueda evitar
    que ciertas piezas sean vecinas unas de otras. Entenderemos que dos
    piezas son vecinas si existe alguna posición ocupada por una pieza tal
    que alguna de las 8 posiciones adyacentes es ocupada por la otra. Es
    decir, en la configuración siguiente las piezas 4 y 9 son vecinas, así
    como 9 y 6, 1 y 3, etc.
     4  9  1  1  1 
     4  8  6  3  3 
     4  8  6  3  3 
     4  8  6  7  7 
     4  8  5  5  5 
     2  8  5  5  5 
     2  8  5  5  5 
    Dichos pares de piezas incompatibles se especifican con un predicado
    forbidden(Pieza1,Pieza2). Es decir, en la entrada:
    width(5).
    height(7).
    rect(1,3,1).
    rect(2,1,2).
    rect(3,2,2).
    rect(4,1,5).
    rect(5,3,3).
    rect(6,1,3).
    rect(7,2,1).
    rect(8,1,6).
    rect(9,1,1).
    forbidden(4,8).
    forbidden(3,7).
    forbidden(2,6).
    los pares incompatibles son (4,8), (3,7) y (2,6).
    Guarda la entrada en un archivo entrada.pl, y modifica tu práctica
    para que lea esta entrada y solucione el problema descrito
    anteriormente. Entrega un único archivo de nombre vecinos.pl. */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%% Some helpful definitions to make the code cleaner:
rect(B):-rect(B,_,_).
xCoord(X) :- width(W),  between(1,W,X).
yCoord(Y) :- height(H), between(1,H,Y).
width(B,W):- rect(B,W,_).
height(B,H):- rect(B,_,H).
insideTable(X,Y):- width(W), height(H), between(1,W,X), between(1,H,Y).



%%%%%%  Variables: They might be useful
% starts-B-X-Y:   box B has its left-bottom cell with upper-right coordinates (X,Y)
%  fills-B-X-Y:   box B fills cell with upper-right coordinates (X,Y)


writeClauses:-
    eachCellAMOrectangle,
    ifStartsThenFills,
    eachRectALO,
    true.
    
    
eachCellAMOrectangle:- xCoord(X), yCoord(Y),
                                       findall(fills-B-X-Y,  rect(B), Lits), 
                                       exactly(1, Lits), fail.
eachCellAMOrectangle.

getFinalPosition(B, Xstart, Ystart, Xend, Yend):- xCoord(Xstart),  yCoord(Ystart),
                                                                            rect(B, SizeX, SizeY), 
                                                                             Xend is (Xstart + SizeX - 1),
                                                                             Yend is (Ystart + SizeY - 1).
                                                                             
                                                                             
fitsTable(B, Xstart, Ystart):- getFinalPosition(B, Xstart, Ystart, Xend, Yend),
                                            insideTable(Xend, Yend).
                                                                             
getBetween(Xstart, Ystart, Xend, Yend, Xb, Yb):- between(Xstart, Xend, Xb),
                                                                               between(Ystart, Yend, Yb).


ifStartsThenFills:- rect(B), xCoord(Xstart), yCoord(Ystart),
                            getFinalPosition(B, Xstart, Ystart, Xend, Yend),
                            insideTable(Xend, Yend),
                            getBetween(Xstart, Ystart, Xend, Yend, Xbetween, Ybetween),
                            writeClause([\+starts-B-Xstart-Ystart, fills-B-Xbetween-Ybetween]), fail.
ifStartsThenFills.

eachRectALO:- rect(B), findall(starts-B-X-Y, fitsTable(B, X, Y), Lits), atLeast(1, Lits), fail.
eachRectALO.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Show the solution. Here M contains the literals that are true in the model: %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

displaySol(M):- nl, yCoord(Y), nl, xCoord(X), writeOK(X, Y, M), fail. % El nl entre Y i X és perquè al canviar de fila, salti de linia.
displaySol(_).

writeOK(X, Y, M):- member(fills-B-X-Y, M), !, write2(B), fail.
writeOK(_, _, _):- write(' *'), fail.

write2(K):- K < 10, write('  '), write(K), fail.
write2(K):- K >= 10, write(' '), write(K), fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%

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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
