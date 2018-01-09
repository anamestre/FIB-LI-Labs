%% A factory produces banners using only a set of existing rectangular
%% pieces.  Our goal is to find out how to use the minimum set of
%% pieces that exactly match the design of the banner. Note that
%% pieces can be rotated if necessary. Also, note that each piece can
%% be used at most once.  That's why there can be several identical
%% pieces in the input.

%%%%%%%%%%%%%%%%%%%%% INPUT EXAMPLE:

%% banner( [                             the x's define the design of the banner
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,x,x,x,x,x,x,.],
%% 	       [.,x,x,x,x,x,x,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.]
%% 	   ]).
%% pieces([
%% 	    [1,3,8],   % piece 1 is a 3 x 8 rectangle
%% 	    [2,3,3],   % piece 2 is a 3 x 3 rectangle
%% 	    [3,9,2],   % ...
%% 	    [4,3,3],
%% 	    [5,3,8],
%% 	    [6,3,2],
%% 	    [7,3,3],
%% 	    [8,3,3],
%% 	    [9,2,3],
%% 	    [a,1,3]	    
%% 	  ]).

%% A possible solution using 6 pieces:
%% .444...888.
%% .444...888.
%% .444...888.
%% .333333333.
%% .333333333.
%% .a99...777.
%% .a99...777.
%% .a99...777.

%% An optimal solution using 3 pieces:
%% .555...111.
%% .555...111.
%% .555...111.
%% .555666111.
%% .555666111.
%% .555...111.
%% .555...111.
%% .555...111.
      	 

:-include(input7). % Load input
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

%%%%%% Some helpful definitions to make the code cleaner:
piece(P):-                  pieces(L), member([P,_,_],L).
pieceSize(P,W,H):-          pieces(L), member([P,W,H],L).
widthBanner(W):-            banner(B), member(L,B), length(L,W),!.
heightBanner(H):-           banner(B), length(B,H), !.
contentsCellBanner(X,Y,C):- cell(X,Y), banner(B), heightBanner(H), Y1 is H-Y+1, nth1(Y1,B,L), nth1(X,L,C).
cell(X,Y):-                 widthBanner(W), heightBanner(H), between(1,W,X), between(1,H,Y).


% You can use the following types of symbolic propositional variables:
%   1. pieceCell-P-X-Y means:   "piece P fills cell [X,Y]" (note: [1,1] is the bottom-left cell of the banner (Careful: Mandatory variable. Otherwise, displaySol will not work)
%   2. rotated-P means:         "piece P is rotated"
%   3. pieceStarts-P-X-Y means: "bottom-left cell of piece P is in cell [X,Y]"
%   4. used-P means:            "piece P is used"

% Should be completed
% Write clauses imposing also that at most K pieces can be used
writeClauses(K):-
    eachPieceAMO,
    eachCellExacO,
    noMoreThanKpieces(K),
    
    ifStartsThenFills,
    noIfStartsThenFills,
    
    %pieceCanBeRotated,
    ifRotatedThenFills,
    noIfRotatedThenFills,
    
    usedP,
    ifUsedThenNotFills,
    true,!.

    
eachPieceAMO:- 
    piece(P),
    findall(pieceStarts-P-X-Y, cell(X, Y), Lits),
    atMost(1, Lits), fail.
eachPieceAMO.


eachCellExacO:- 
    contentsCellBanner(X, Y, 'x'), 
    findall(pieceCell-P-X-Y, piece(P), Lits), 
    exactly(1, Lits), fail.
eachCellExacO.


pieceCanBeRotated:- 
    pieceSize(P, SizeX, SizeY), 
    findall(pieceStarts-P-X-Y, pieceCanStartHere(P, X, Y, SizeY, SizeX), Lits), 
    expressOr(rotated-P, Lits), fail.
pieceCanBeRotated.
                                   

ifStartsThenFills:- 
    pieceSize(P, SizeX, SizeY),  
    getCells(X, Y, X1, Y1, SizeX, SizeY), 
    negate(pieceStarts-P-X-Y, NegStart),
    writeClause([NegStart, rotated-P, pieceCell-P-X1-Y1]), fail.
ifStartsThenFills.


ifRotatedThenFills:- 
    pieceSize(P, SizeX, SizeY), 
    getCells(X, Y, X1, Y1, SizeY, SizeX), 
    negate(pieceStarts-P-X-Y, NegStart),
    negate(rotated-P, NegRotated),
    writeClause([NegStart, NegRotated, pieceCell-P-X1-Y1]), fail.
ifRotatedThenFills.


usedP:- 
    pieceSize(P, SizeX, SizeY), 
    findall(pieceStarts-P-X-Y, (pieceCanStartHere(P, X, Y, SizeX, SizeY);  pieceCanStartHere(P, X, Y, SizeY, SizeX)), Lits),
    expressOr(used-P, Lits), fail.
usedP.

ifUsedThenNotFills:-
    cell(X, Y),
    piece(P),
    negate(pieceCell-P-X-Y, NegCell),
    writeClause([used-P, NegCell]), fail.
ifUsedThenNotFills.


noIfStartsThenFills:- 
    pieceSize(P, SizeX, SizeY),  
    
    cell(X,Y),
    cell(X1, Y1),
    \+getCells(X, Y, X1, Y1, SizeX, SizeY), 
    
    negate(pieceStarts-P-X-Y, NegStart),
    writeClause([NegStart, rotated-P, \+pieceCell-P-X1-Y1]), fail.
noIfStartsThenFills.


noIfRotatedThenFills:- 
    pieceSize(P, SizeX, SizeY), 
    
    cell(X,Y),
    cell(X1, Y1),
    \+getCells(X, Y, X1, Y1, SizeY, SizeX), 
    
    negate(pieceStarts-P-X-Y, NegStart),
    negate(rotated-P, NegRotated),
    negate(pieceCell-P-X1-Y1, NegCell),
    
    writeClause([NegStart, NegRotated, NegCell]), fail.
noIfRotatedThenFills.

noMoreThanKpieces(K):-
    findall(used-P, piece(P), Lits),
    atMost(K, Lits),
    fail.
noMoreThanKpieces(_).

% %%%%%%%%%%  Aux predicates %%%%%%%%%%%

pieceCanStartHere(_,X, Y, SizeW, SizeH):-
    contentsCellBanner(X, Y, 'x'),
    widthBanner(W),
    heightBanner(H),
    MaxW is W - SizeW + 1,
    MaxH is H - SizeH + 1,
    between(1, MaxW, X),
    between(1, MaxH, Y),
    findall(X1-Y1, getCells(X, Y, X1, Y1, SizeW, SizeH), Lits),
    Size is SizeW * SizeH,
    length(Lits, Size).
    
getCells(Xstart, Ystart, X, Y, SizeW, SizeH):-
    cell(Xstart, Ystart),
    FinalX is Xstart + SizeW - 1,
    FinalY is Ystart + SizeH - 1, 
    contentsCellBanner(X, Y, 'x'),
    between(Xstart, FinalX, X),
    between(Ystart, FinalY, Y).


% SHOULD BE MODIFIED!!!!
% Given model M, computes K (number of pieces used in the model)
piecesUsed(M,K):-
    findall(P, member(used-P, M), Lits),
    length(Lits, K), !.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DISPLAYSOL:

displaySol(M):-
    widthBanner(W),
    heightBanner(H),
    between(1,H,YB),
    nl,
    Y is H-YB+1,
    between(1,W,X),
    writeCell(M,X,Y),
    fail.
displaySol(_):-nl.

writeCell(M,X,Y):- member(pieceCell-P-X-Y,M), !, write(P).
writeCell(_,_,_):- write('.').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN:

main:-  symbolicOutput(1), !, writeClauses(30), halt.   % print the clauses in symbolic form and halt
main:-
    pieces(P),length(P,Initial),
    write('Looking for initial solution with at most '), write(Initial), write( ' pieces'), nl,
    tell(clauses), initClauseGeneration, writeClauses(Initial), told,% generate the (numeric) SAT clauses and call the solver
    tell(header),  writeHeader,  told,
    numVars(N), numClauses(C), 
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,[]),!.

treatResult(20,[]       ):- write('No solution exists.'), nl, halt.
treatResult(20,BestModel):- nl,nl,piecesUsed(BestModel,K), write('Optimal solution (with '), write(K), write(' pieces):'),nl, displaySol(BestModel), halt.
treatResult(10,_):- %   shell('cat model',_),	
    see(model), symbolicModel(M), seen,  
    piecesUsed(M,K),
    write('Solution found using '), write(K), write(' pieces '),nl,
    K1 is K-1,
    displaySol(M),nl,
    write('********************************************************************'),nl,
    write('Looking for solution with at most '), write(K1), write( ' pieces'), nl,
    tell(clauses), initClauseGeneration, writeClauses(K1), told,
    tell(header),  writeHeader,  told,
    numVars(N),numClauses(C),nl,
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
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
