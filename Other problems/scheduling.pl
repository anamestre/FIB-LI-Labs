:- use_module(library(clpfd)).

machines(5).
tasks([1,2,3,4,5,6,7,8,9,10,11,12,13,14]).
duration([8,6,7,5,2,3,8,6,2,6,1,2,6,4]).

main :- schedule, nl, halt.

schedule:- 
    machines(M), length(M, SizeM),
    tasks(T), length(T, SizeT),
    NumVars is SizeM * SizeT,
    length(Vars, NumVars),
    Vars ins 0..1,
    matrixByRows(Vars, SizeM, MatrixR),
    transpose(MatrixR, MatrixC),
    duration(D), sum(D, MaxD),
    makeConstraints(MatrixR, MatrixC, maxD),
    labeling([min(maxD)], Vars).


matrixByRows([], _, []).
matrixByRows(Vars, SizeM, [M|MatrixR]):-
    split(Vars, SizeM, M, Vrest),
    matrixByRows(Vrest, SizeM, MatrixR).

split(Rest, 0, [], Rest):- !.
split([V|Vars], SizeM, [V|M], Vrest):-
    SizeM1 is SizeM - 1,
    split(Vars, SizeM1, M, Vrest).

sum([], 0).
sum([L|List], L + Num):-
    sum(List, Num).

% makeConstraints(
