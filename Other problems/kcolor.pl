nVertices(5).
edge(1,2).
edge(1,3).
edge(2,3).
edge(3,4).
edge(4,5).
edge(5,1).
edge(5,2).

color(K):- 
    nVertices(N),
    length(Vars, N),
    Vars ins 1..K,
    kColor(N, N, Vars),
    label(Vars), write(Vars), nl.

kColor(0, _, _).
kColor(K, N, Vars):-
    different(K, N, Vars),
    K1 is K - 1,
    kColor(K1, N, Vars).

different(_, 0, _).
different(X, Y, Vars):-
    edges(X, Y),
    nth1(X, Vars, ColorX),
    nth1(Y, Vars, ColorY),
    ColorX #\= ColorY,
    Y1 is Y - 1,
    different(X, Y1, Vars).
different(X, Y, Vars):-
    Y1 is Y - 1,
    different(X, Y1, Vars).
    
edges(X, Y):- edge(X, Y).
edges(X, Y):- edge(Y, X).
