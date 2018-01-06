
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% SESSIÓ 5 - 6 %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
1. Escribe un predicado prolog "flatten" que aplana listas:

?- flatten( [a,[b,c,[b,b],e], f], L).
L = [a,b,c,b,b,e,f]

Escribe otro que elimina las repeticiones:
?- flattenNoRepetitions([a,[b,c,[b,b],e], f], L).
L = [a,b,c,e,f] */

flatten([], []):- !.
flatten(X, [X]):- X\=[_|_].
flatten([L|List], FList):- flatten(L, L2), flatten(List, List2), append(L2, List2, FList).

flattenNoRepetitions(L, RL):- flatten(L, L2), noRepes(L2, RL).

noRepes([], []):- !.
noRepes([L|List], [L|Res]):- \+member(L, List), noRepes(List, Res).
noRepes([_|List], Res):- noRepes(List, Res).

/*
2. Tenemos una fila de cinco casas, con cinco vecinos con casas de colores diferentes, y cinco 
profesiones, animales, bebidas y nacionalidades diferentes, y sabiendo que:

    1 - El que vive en la casa roja es de Peru
    2 - Al frances le gusta el perro
    3 - El pintor es japones
    4 - Al chino le gusta el ron
    5 - El hungaro vive en la primera casa
    6 - Al de la casa verde le gusta el coñac
    7 - La casa verde esta a la izquierda de la blanca
    8 - El escultor cría caracoles
    9 - El de la casa amarilla es actor
   10 - El de la tercera casa bebe cava
   11 - El que vive al lado del actor tiene un caballo
   12 - El hungaro vive al lado de la casa azul
   13 - Al notario la gusta el whisky
   14 - El que vive al lado del medico tiene un ardilla,

Escribe un programa Prolog que averigue para cada persona todas sus 
caracteristicas de la forma [num_casa,color,profesion,animal,bebida,pais] 
averiguables. Ayuda: sigue el siguiente esquema: */

vecinoIzq(R1, R2):- R2 is R1 + 1.
vecinoDer(R1, R2):- R2 is R1 - 1.

vecino(R1, R2):- vecinoIzq(R1, R2).
vecino(R1, R2):- vecinoDer(R1, R2).

casas:- Sol = [[1, _, _, _, _, _],
               [2, _, _, _, _, _],
               [3, _, _, _, _, _],
               [4, _, _, _, _, _],
               [5, _, _, _, _, _]],
               member([_, roja, _, _, _, peru], Sol),
               member([_, _, _, perro, _, francia], Sol),
               member([_, _, pintor, _, _, japon], Sol),
               member([_, _, _, _, ron, china], Sol),
               member([1, _, _, _, _, hongria], Sol),
               member([_, verde, _, _, coñac, _], Sol),
               member([R1, verde, _, _, _, _], Sol),
               member([R2, blanca, _, _, _, _], Sol),
               vecinoIzq(R1, R2),
               member([_, _, escultor, caracoles, _, _], Sol),
               member([_, amarilla, actor, _, _, _], Sol),
               member([3, _, _, _, cava, _], Sol),
               member([R4, _, _, caballo, _, _], Sol),
               member([R5, _, actor, _, _, _], Sol),
               vecino(R4, R5),
               member([R6, _, _, _, _, hongria], Sol),
               member([R7, azul, _, _, _, _], Sol),
               vecino(R6, R7),
               member([_, _, notario, _, whisky, _], Sol),
               member([R8, _, _, ardilla, _, _], Sol),
               member([R9, _, medico, _, _, _], Sol),
               vecino(R8, R9),
               write(Sol), nl.


/*
3. Haz un programa prolog que escriba la manera de colocar sobre un tablero de
   ajedrez ocho reinas sin que éstas se ataquen entre sí.
   Por ejemplo, ésta sería una solucion:
         
      . . x . . . . .
      . . . . . x . .
      . . . x . . . .
      . x . . . . . .
      . . . . . . . x
      . . . . x . . .
      . . . . . . x . 
      x . . . . . . . */
      
      
queens :- permutation([1,2,3,4,5,6,7,8], PermutedList),
          allOK(PermutedList),
          printQueens(PermutedList).

% Se segur que no hi haura cap reina a la mateixa columna ni fila, 
% nomes he de comprovar les diagonals
allOK([]).
allOK([L|List]):- checkQueen(L, List, 1), allOK(List).

checkQueen(_, [], _).
checkQueen(L, [L2|List], Dist):- L - L2 =\= Dist,
                                 L2 - L =\= Dist,
                                 Dist2 is Dist + 1,
                                 checkQueen(L, List, Dist2).
          
% Printing code          
printQueens([]).
printQueens([L|List]):- printRow(L), nl, printQueens(List).

printRow(L):- printPart(L), write('X'), printPart2(L).

printPart(1):- !.
printPart(L):- write('.'), L2 is L - 1, printPart(L2).

printPart2(8):- !.
printPart2(L):- write('.'), L2 is L + 1, printPart2(L2).


