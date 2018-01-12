:- use_module(library(clpfd)).

%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,  3,[2,1,1,1,1,1]).
ejemplo(1,  4,[2,2,2,1,1,1,1]).
ejemplo(2,  5,[3,2,2,2,1,1,1,1]).
ejemplo(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
ejemplo(4,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(5,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).


% Es comproven els límits del quadrat gran.
insideBigSquare(0, _, [], []):-!.
insideBigSquare(N, Big, [Side|Sides], [CoordIni|Vars]):-
        Limit is Big - Side + 1, 
        CoordIni in 1..Limit, 
        N2 is N - 1, 
        insideBigSquare(N2, Big, Sides, Vars).

% Agafa cada quadrat i l'envia a comprovar que no té solapaments.        
nonoverlapping(_, [], [], []).        
nonoverlapping(_, [Side|Sides], [RowCoord|RowVars], [ColCoord|ColVars]):-
        neverOverlap(Side, Sides, RowCoord, RowVars, ColCoord, ColVars),
        nonoverlapping(_, Sides, RowVars, ColVars).

% Per un quadrat comprova si es solapa amb tots els altres.        
neverOverlap(_, [], _, [], _, []).
neverOverlap(Side1, [Side2|Sides], Row1, [Row2|RowVars], Col1, [Col2|ColVars]):-
        nonOverLapping2squares(Side1, Side2, Row1, Row2, Col1, Col2),
        neverOverlap(Side1, Sides, Row1, RowVars, Col1, ColVars).

% Per cada dos quadrats, comprova que no es solapin entre ells.        
nonOverLapping2squares(Side1, Side2, Row1, Row2, Col1, Col2):-
        Row1 + Side1 #=< Row2;
        Col1 + Side1 #=< Col2;
        Row2 + Side2 #=< Row1;
        Col2 + Side2 #=< Col1.

main:- 
    ejemplo(3,Big,Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
    length(Sides, N), 
    length(RowVars, N), % get list of N prolog vars: Row coordinates of each small square
    insideBigSquare(N, Big, Sides, RowVars),
    insideBigSquare(N, Big, Sides, ColVars),
    nonoverlapping(N, Sides, RowVars, ColVars),
    label(RowVars),
    label(ColVars),
    displaySol(N, Sides, RowVars, ColVars), halt.


displaySol(N,Sides,RowVars,ColVars):- 
    between(1,N,Row), nl, between(1,N,Col),
    nth1(K,Sides,S),    
    nth1(K,RowVars,RV),    RVS is RV+S-1,     between(RV,RVS,Row),
    nth1(K,ColVars,CV),    CVS is CV+S-1,     between(CV,CVS,Col),
    writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.

