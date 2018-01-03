% Constraint Logic Programming
:- use_module(library(dif)).	% Sound inequality
:- use_module(library(clpfd)).	% Finite domain constraints
:- use_module(library(clpb)).	% Boolean constraints
:- use_module(library(chr)).	% Constraint Handling Rules
:- use_module(library(when)).	% Coroutining
%:- use_module(library(clpq)).  % Constraints over rational numbers

% Your program goes here

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


coverEdges(Vars):-
    findall(X-Y, edge(X, Y), Lits),
    cover(Lits, Vars).

cover([], _).
cover([L1-L2|Lits], Vars):-
    nth1(L1, Vars, VarL1),
    nth1(L2, Vars, VarL2),
    makeConstraint(VarL1, VarL2),
    cover(Lits, Vars).

% Alternativa per la OR:
%  Var1 #= 1 #\/ Var2 #= 1
makeConstraint(V1, V2):-
    V1 #= 1;
    V2 #= 1.

atMostKVertices(Vars, K):-
    suma(Vars, SumVars),
    SumVars #=< K.

suma([], 0).
suma([V|Vars], K + V):-
    suma(Vars, K).
