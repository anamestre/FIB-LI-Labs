:-use_module(library(clpfd)).

nVertices(5).
edge(1,2).
edge(1,3).
edge(2,3).
edge(3,4).
edge(4,5).
edge(5,1).
edge(5,2).

displaySolCover(Vars):-
    nVertices(N),
    between(1, N, V),
    nth1(V, Vars, X), %% X is the V-th element of the list Vars (first element has index 1)
    write('Chosen('), write(V), write(') = '), write(X), nl, fail.
displaySolCover(_).

cover(K):-
    nVertices(N),
    length(Vars, N),
    Vars ins 0..1,
    coverEdges(Vars),         % each edge has at least one of its extreme points chosen (to be implemented)
    atMostKVertices(Vars, K), % we choose at most K vertices (to be implemented)
    label(Vars), !,
    displaySolCover(Vars).

coverEdges(Vars) :-
    findall(V1-V2, edge(V1, V2), Edges),
    coverEdges(Edges, Vars).

coverEdges([], _).
coverEdges([V1-V2|Edges], Vars) :-
    nth1(V1, Vars, Var1),
    nth1(V2, Vars, Var2),
    Var1 #= 1 #\/ Var2 #= 1,
    coverEdges(Edges, Vars).

atMostKVertices(Vars, K) :-
    expressSum(Vars, Expr),
    Expr #=< K.
    
expressSum([V], V) :- !.
expressSum([V|Vars], V + Expr) :-
	expressSum(Vars, Expr).
