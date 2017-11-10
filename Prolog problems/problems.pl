
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SESSIÓ 3 - 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEORIA I INTRODUCCIÓ A PROLOG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 "is" serveix per evaluar expressions aritmètiques
 "L1 =\ L2" vol dir L1 és no unificable amb L2
                
 member(X, [a, 1, b, f(2,3), [c,d]]), write(X), nl, fail.                
 member(X, [X|_]).
 member(X, [_|L]):- member(X, L).
*/

 natural(0).
 natural(N):- natural(N1), N1 is N + 1.

 fact(0,1).
 fact(N, F):- N1 is N-1, fact(N1, F1), F is N*F1.

 
 % cifras([4, 8, 7, 100, 4, 9], 380).
 /*cifras(L, N):- subset(L, S),
                 permutation(S, P),subset([], []).
                 expresssion(P, E),subset([X|L], [X|S]):- subset(L, S).
                 N is E,subset([_|L], S):- subset(L,S).
                 write(E), nl, fail.
 permt([], []).
 permt([X|L], P):- permt(L, P1), concat(PA, PB, P1), concat(PA, [X|PB], P).
 
 expresion([X], X).
 expression(L, E1*E2):- concat(L1, L2, L), L1 \= [], L2 \= [], expression(L1, E1), expression(L2, E2).
 expression(L, E1+E2):- concat(L1, L2, L), L1 \= [], L2 \= [], expression(L1, E1), expression(L2, E2).
 expression(L, E1-E2):- concat(L1, L2, L), L1 \= [], L2 \= [], expression(L1, E1), expression(L2, E2).
 expression(L, E1/E2):- concat(L1, L2, L), L1 \= [], L2 \= [], expression(L1, E1), expression(L2, E2), R is E2, R \= 0. % \+(0 is E2)*/

% "append"
 concat([], L, L).
 concat([X|L1], L2, [X|L3]):- concat(L1, L2, L3).
% concat(L1, L2, [a,b,c]), write([L1, L2]), nl, fail.


% factores
factores(1, []):- !.  % S'ha de posar perquè quan fas factores(1) pot unificar amb aquesta clausula i amb la següent, 
                      % però no volem que entri a la següent perquè entraria en un bucle infinit.
factores(N, [F|L]):- natural(F), F > 1, 0 is N mod F,
                     N1 is N // F, factores(N1, L).     % // és la divisió d'enters
                     
% Com està implementat el not "\+" -> Sí X falla en temps finit doncs guai, però si la trucada a X no acaba.. estamoh jodioh :) 
not(X):- X, !, fail.
not(_).

% Fer servir el findall -> Donada una llista de números, com obtenir la llista dels elements parells.
% findall(X, cond(X), L)
% parells([1,2,3,4,5], L). -> L = [2, 4]
parells(L, LP):- findall(X, (member(X, L), 0 is X mod 2), LP).

% Famoso problema de los dados
dados:- permutation([1,2,3,4,5,6,7,8,9], [R1, R2, R3, A1, A2, A3, V1, V2, V3]),
        gana([R1, R2, R3], [A1, A2, A3]),
        gana([A1, A2, A3], [V1, V2, V3]),
        gana([V1, V2, V3], [R1, R2, R3]),
        write([R1, R2, R3]),
        write([A1, A2, A3]),
        write([V1, V2, V3]).
        
gana(D1, D2):- findall([X,Y], (member(X, D1), member(Y, D2), X > Y), L), length(L, N), N >= 5. % >=5 perquè he de guanyar més de la meitat dels cops



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROBLEMES PROLOG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% 2. prod(L, P)
     prod([], 1).
     prod([L1|List], P):- prod(List, P1), P is L1*P1.

% 3. pescalar(L1,L2,P)
     pescalar([], [], 0).
     pescalar([L1|List1], [L2|List2], R):- pescalar(List1, List2, R1), R is R1 + L1*L2.
     
% 4. interseccion(L1, L2, L).
%    union(L1, L2, L).

    %interseccion([X|L1], [X|L2], [X|L]):- interseccion(L1, L2, L).
    % acabar
    union([], L, L).
    union([X|L1], L2, L3):- member(X, L2), !, union(L1, L2, L3). % Aquí s'ha de posar el "!" perquè sinó també entrarà a la següent opció.
    union([X|L1], L2, [X|L3]):- union(L1, L2, L3).
    
    interseccion([], _, []).
    interseccion([X|L1], L2, [X|L3]):- member(X, L2), !, remove_element(X, L2, NewL2), interseccion(L1, NewL2, L3).
    interseccion([_|L1], L2, L3):- interseccion(L1, L2, L3).
    
    remove_element(X, L, NewL):- append(L1, [X|L2], L), append(L1, L2, NewL).
    

% 5. lastElement(L, X), reverseList(L, RL).
    lastElement(L, X):- append(_, [X], L).
    
    reverseList([], []).
    reverseList(L, [X|RL]):- lastElement(L, X), removeLast(L, NewL), reverseList(NewL, RL).
    
    removeLast(L, NewL):- append(NewL, [_], L).


% 6. fib(N, F) = F es el N-esimo numero de Fibonacci.
% fib(1) = 1, fib(2) = 1, y, si N > 2, como: fib(N) = fib(N − 1) + fib(N − 2).ç
% 1, 1, 2, 3, 5, 8, 13, 21...
    fib(1, 1):- !.
    fib(2, 1):- !.
    fib(N, F):- N1 is N - 1, N2 is N - 2, 
                fib(N1, F1), fib(N2, F2),
                F is F1 + F2.


% 7. dados(P, N, L) = la lista L expresa una manera de sumar P puntos lanzando N dados.
    dados(0, 0, []).
    dados(P, N, [X|L]):- N > 0,  member(X, [1,2,3,4,5,6]), N1 is N - 1, P1 is P - X, dados(P1, N1, L). 
                     
    sum([], 0).
    sum([L|L1], P):- sum(L1, P1), P is P1 + L.
    
% 8. suma_demas(L) = algun elemento de L = suma de los demas elementos
    suma_demas(L):- append(L1, [X|L2], L), append(L1, L2, NewL), sum(NewL, X).

    
% 9. suma_ants(L) = algun elemento de L = suma de los anteriores elementos.
    suma_ants(L):- append(L1, [X|_], L), sum(L1, X).


% 10. card(L) = escribe la lista que dice cuántas veces aparece cada elemento en L.
% card( [1,2,1,5,1,3,3,7] ) escribirá [[1,3],[2,1],[5,1],[3,2],[7,1]].
 card(List):- cards(List, Res), write(Res).
 
 cards([], []).
 cards([L|List], [[L, NumL]|Res]):- cards(List, ResList),
               add_card([L, NewNumL], ResList, Res), !,
               NumL is NewNumL+1.
cards([L|List], [[L, 1]|Res]):- cards(List, Res).

% FinalL = List ++ [L]
add_card(L, List, FinalL):- append(L1, [L|L2], FinalL),
    						append(L1, L2, List).
    
% 11.   
  esta_ordenada([]).
  esta_ordenada([_]).
  esta_ordenada([L1, L2|List]):- L1 < L2, esta_ordenada([L2|List]).
  
% 12. ordenacion con permutacion y esta_ordenada
  ordenacion(L, SL):- permutation(L, SL), esta_ordenada(SL).

% 18. palindromos -> El setof treu les repeticions.
  palindromos(L):- setof(L2, (permutation(L, L2), es_palindromo(L2)), Res), write(Res).
  
  es_palindromo([]).
  es_palindromo([L|List]):- lastElement(List, Last), Last = L, 
                            removeLast(List, NewList), es_palindromo(NewList).
