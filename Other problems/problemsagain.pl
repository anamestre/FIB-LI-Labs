prod([], 1).
prod([L|List], P):- prod(List, P2), P is P2 * L.

card(L):- card(L, List), write(List).

card([], []).
card([L|Ls], [[L, Num]|List]):-  card(Ls, List2),
                                                takeCard([L, NumL], List2, List), !,
                                                Num is NumL + 1.
card([L|Ls], [[L, 1]|List]):- card(Ls, List).

takeCard(C, List2, List):- append(L1, [[C]|L2], List2),
                                        append(L1, L2, List).
                                        
% EX 21
% Cada canibal es representado con el entero -1.
% Cada misonero es representado con el entero 1.
% OI y OD representan las personas en la orilla izquierda y derecha respectivamente.
% Canoa -1 si esta en el lado izquierdo, 1 si en el derecho.
% ActualSteps contiene los pasos dados hasta el momento.
% Steps contiene los pasos finales.

pert_con_resto(X,L,Resto):- concat(L1,[X|L2], L), concat(L1, L2, Resto).

count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

buildState(I,D,Canoa, NumPosI-NumNegI*NumPosD-NumNegD*Canoa):-
    count(I,1,NumPosI),
    count(D,1,NumPosD),
    count(I,-1,NumNegI),
    count(D,-1,NumNegD).

checkOrilla(Orilla):-
    count(Orilla,1,NumPos),
    NumPos == 0, !.
    
checkOrilla(Orilla):-
    count(Orilla,1,NumPos),
    count(Orilla,-1,NumNeg),
    NumPos >= NumNeg.
    
cruzaRio(_,OD, _, [], _):-
    count(OD, 1, 3), !.

cruzaRio(OI, OD, -1,  [Step|Steps], States):-
    checkOrilla(OI),
    checkOrilla(OD),
    pert_con_resto(P, OI, NewOI),
    Step = [NewOI, [P|OD], 1],
    buildState(NewOI, [P|OD], 1, State),
    \+ member(State, States),
    cruzaRio(NewOI, [P|OD], 1, Steps, [State|States]).
    
cruzaRio(OI, OD, -1,  [Step|Steps], States):-
    checkOrilla(OI),
    checkOrilla (OD), 
    pert_con_resto (P1, OI, OI1), 
    pert_con_resto (P2, OI1, NewOI), 
    Step = [NewOI, [P1 | [P2 | OD]], 1], 
    buildState (NewOI, [P1 | [P2 | OD]], 1, Estado), 
    \ + miembro (Estado, Estados), 
    cruzaRio (NewOI, [P1 | [P2 | OD]], 1, Pasos, [Estados | Estados]). 
   
cruzaRio (OI, OD, 1, [Step | Steps], States): - 
    checkOrilla (OI), 
    checkOrilla (OD), 
    pert_con_resto (P, OD, NewOD), 
    Step = [[P | OI], NewOD, -1 ], 
    buildState ([P | OI], NewOD, -1, State), 
    \ + member (State, States), 
    cruzaRio ([P | OI], NewOD, -1, Steps, [State | States]). 

cruzaRio (OI, OD, 1, [Step | Steps], States): - 
    checkOrilla (OI), 
    checkOrilla (OD),
    pert_con_resto (P1, OD, OD1), 
    pert_con_resto (P2, OD1, NewOD), 
    Step = [[P1 | [P2 | OI]], NewOD, -1], 
    buildState ([P1 | [P2 | OI]], NewOD , -1, Estado), 
    \ + miembro (Estado, Estados), 
    cruzaRio ([P1 | [P2 | OI]], NewOD, -1, Steps, [State | States]). 

writeMisioneros (0). 
writeMisioneros (N): - write ("M"), N1 es N-1, writeMisioneros (N1). 

writeCanibals (0). 
writeCanibals (N): - write ("C"), N1 es N-1, writeCanibals (N1). 

writeOrilla (Orilla): - 
    count (Orilla, 1, NumPos), 
    count (Orilla, -1, NumNeg), 
    writeMisioneros (NumPos), 
    writeCanibals (NumNeg), 
    nl. 

writeAgua (-1): - 
    write ("Canoa ~~~~~~~~~~~~~~~"), nl,
    write ("~~~~~~~~~~~~~~~~~~~~"), nl, 
    write ("~~~~~~~~~~~~~~~~~~~ ~ "), nl. 
    
writeAgua (1): - 
    write ("~~~~~~~~~~~~~~~~~~~~~~~~"), nl, 
    write ("~~~~~~~~~~~~~ ~~~~~~~ "), nl, 
    escribir (" Canoa ~~~~~~~~~~~~~~~ "), nl. 
    
printPasos (_, []): - !. 

printPasos (N, [[OI, OD, Canoa] | Pasos]): - 
    write ("STEP"), write (N), write (""), nl, 
    write ("--------- --------- "), nl, 
    writeOrilla (OI), 
    writeAgua (Canoa), 
    writeOrilla (OD), 
    nl, write (" ================ === "), 
    nl, nl, N2 es N + 1, printPasos (N2, pasos). 


    
mision: - 
    Misioneros = [1,1,1], 
    Canibales = [-1, -1, -1],

    buildState(OrillaIzquierda,[], -1, State),
    cruzaRio(OrillaIzquierda, [], -1, Pasos, [State]),
    nl, write("==================="),nl,nl,
    printPasos(1,[[OrillaIzquierda,[],-1]|Pasos]).
