
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% SESSIÓ 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
 cifras(L, N):- subset(L, S),
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
 expression(L, E1/E2):- concat(L1, L2, L), L1 \= [], L2 \= [], expression(L1, E1), expression(L2, E2), R is E2, R \= 0. % \+(0 is E2)

% "append"
 concat([], L, L).
 concat([X|L1], L2, [X|L3]):- concat(L1, L2, L3).
% concat(L1, L2, [a,b,c]), write([L1, L2]), nl, fail.



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%% PROBLEMES PROLOG %%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% 2. prod(L, P)
     prod([], 1).
     prod([L1|List], P):- prod(List, P1), P is L1*P1.

% 3. pescalar(L1,L2,P)
     pescalar([], [], 0).
     pescalar([L1|List1], [L2|List2], R):- pescalar(List1, List2, R1), R is R1 + L1*L2.
     
% 4. interseccion(L1, L2, L).
%    union(L1, L2, L).

    interseccion([X|L1], [X|L2], [X|L]):- interseccion(L1, L2, L).
    % acabar
    
