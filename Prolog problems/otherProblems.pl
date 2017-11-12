% Preparació pel primer examen de laboratori de Prolog.
/*
2) Construid un predicado *domino* que, dada una lista de fichas de
    dominó (cada ficha es un par [X,Y]), construya un posible
    encadenamiento de todas ellas, girando las fichas si así fuera
    necesario.  Bajo backtrack el predicado debe ser capaz de generar
    todos las posibles soluciones. Es decir, se espera el siguiente
    comportamiento:
    ?- domino([[6,5],[4,1],[6,1],[2,5],[3,6],[2,4]],P), write(P), nl, fail.
    [[6,5],[5,2],[2,4],[4,1],[1,6],[6,3]]
    [[6,1],[1,4],[4,2],[2,5],[5,6],[6,3]]
    [[3,6],[6,5],[5,2],[2,4],[4,1],[1,6]]
    [[3,6],[6,1],[1,4],[4,2],[2,5],[5,6]]
    false.
    Entrega un único archivo de nombre domino.pl. */
    
    takeDo(Do, Dominos, NewDos):- append(L1, [Do|L2], Dominos),
                                                        append(L1, L2, NewDos).
    
    domino([], []).
    domino(Dominos, P):- takeDo(Do, Dominos, NewDos),
                                        domino(NewDos, P2),
                                        putDo(Do, P2, P).
                                        
putDo([X, Y], [], [[X, Y]]).
putDo([X, Y], [], [[Y, X]]).
putDo([X, Y], [[Y, Z]|List], [[X, Y], [Y, Z] | List]).
putDo([Y, X], [[Y, Z]|List], [[X, Y], [Y, Z] | List]).

/*
 2) Dada una fórmula proposicional, donde las variables son enteros desde
    1 hasta N, la conjunción se expresa como * y la disyunción como +,
    queremos convertirla a CNF utilizando el procedimiento de Tseitin. La
    salida será en formato DIMACS (el utilizado para el SAT solver), pero
    no queremos escribir la cabecera.
    Os damos un trozo de código en Prolog que deberéis rellenar. Debido a
    que Tseitin necesita la introducción de variables adicionales, os
    proporcionamos un predicado newvar(X), que instancia X a una nueva
    variable. Similarmente, writec(C) escribe la cláusula C (lista de
    literales).
    El comportamiento esperado es el siguiente:
    ?- main.
    -4 1 0
    -4 -2 0
    -1 2 4 0
    -2 5 0
    -1 5 0
    -5 2 1 0
    -6 3 0
    -6 5 0
    -3 -5 6 0
    4 7 0
    -6 7 0
    -7 -4 6 0
    7 0
    Debes entregar un único archivo de nombre tseitin.pl.
    Puedes completar el código que adjuntamos a continuación:
    main:- 
        F = -(1 * -2) + 3 * (2+1),  
        assertz(nextvarNum(4)), % necesario para utilizar newvar
        tseitin(F,RootVar), writec([RootVar]), nl, !.
    tseitin(F*G,X):- ..., !.
    tseitin(F+G,X):- ..., !.
    tseitin(-F,-X):- tseitin(F,X), !.
    tseitin(P,P).
    writeClauses( Z=X*Y ):- writec([-Z,X]), ..., !.
    writeClauses( Z=X+Y ):- ..., !.
    writec( []    ):- write('0'), nl,!.
    writec( [X|C] ):- Lit is X, write(Lit), write(' '), writec(C),!.
    newvar(X):- retract(nextvarNum(X)), X1 is X+1, assertz(nextvarNum(X1)),!.
    ************************************************************************
    ************************************************************************ */
    
    main:- 
        F = -(1 * -2) + 3 * (2+1),  
        assertz(nextvarNum(4)), % necesario para utilizar newvar
        tseitin(F,RootVar), writec([RootVar]), nl, !.
   
    tseitin(F*G,X):- newvar(A), tseitin(-A + F, ALGO),
                                                tseitin(-A + G, ALGO2),
                                                append(ALGO, ALGO2, X).
    
    tseitin(F+G,X):- newvar(A), tseitin(A + -F, ALGO),
                                                tseitin(A + -G, ALGO2),
                                                append(ALGO, ALGO2, X).
    
    
    tseitin(-F,-X):- tseitin(F,X), !.
    tseitin(P,P).
    
    %writeClauses( Z=X*Y ):- writec([-Z,X]), ..., !.
   % writeClauses( Z=X+Y ):- ..., !.
    
    writec( []    ):- write('0'), nl,!.
    writec( [X|C] ):- Lit is X, write(Lit), write(' '), writec(C),!.
    newvar(X):- retract(nextvarNum(X)), X1 is X+1, assertz(nextvarNum(X1)),!.