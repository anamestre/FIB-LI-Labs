
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% SESSIÓ 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 "is" serveix per evaluar expressions aritmètiques
 
 cifras([4, 8, 7, 100, 4, 9], 380).
 cifras(L, N):- subset(L, S),
                permutation(S, P),
                expression(P, E),
                N is E,
                write(E), nl, fail.

                
 member(X, [a, 1, b, f(2,3), [c,d]]), write(X), nl, fail.                
 member(X, [X|_]).
 member(X, [_|L]):- member(X, L).
 
 
*/

 natural(0).
 natural(N):- natural(N1), N1 is N + 1.

 fact(0,1).
 fact(N, F):- N1 is N-1, fact(N1, F1), F is N*F1.

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
