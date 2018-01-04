:- use_module(library(clpfd)).

% Tenemos 5 ordenadores idénticos que pueden ejecutar tareas de computación, una detrás de otra. 
% ¿Cuánto tiempo necesitaré como mínimo para ejecutar las siguientes 14 tareas (cada una en al menos un ordenador)? 
% task 1 takes 8 minutes 
% task 2 takes 6 minutes 
% task 3 takes 7 minutes 
% task 4 takes 5 minutes 
% task 5 takes 2 minutes 
% task 6 takes 3 minutes 
% task 7 takes 8 minutes 
% task 8 takes 6 minutes 
% task 9 takes 2 minutes 
% task 10 takes 6 minutes 
% task 11 takes 1 minutes 
% task 12 takes 2 minutes 
% task 13 takes 6 minutes 
% task 14 takes 4 minutes 


machines(5).
tasks([1,2,3,4,5,6,7,8,9,10,11,12,13,14]).
duration([8,6,7,5,2,3,8,6,2,6,1,2,6,4]).

main :- schedule, nl, halt.

schedule:- 
	machines(M),
    tasks(T), length(T, SizeT),
    SizeV is SizeT * M,
    length(Vars, SizeV),
    Vars ins 0..1,
    matrixByRows(Vars, M, Rows),
    write('Hola'),
    transpose(Rows, Cols),
    duration(D),
    sum(D, MaxDuration),
    write('Hola2'),
    Duration in 1..MaxDuration, % En realitat no pot estar en 1
    write('Hola3'),
    makeConstraints(Cols, Rows, Duration),
    write('Hola4'),
    label(Vars).


matrixByRows([], _, []):- !.
matrixByRows(Vars, SizeT, [Row|Rows]):-
    split(Vars, SizeT, Row, RowsLeft),
    matrixByRows(RowsLeft, SizeT, Rows).

split(Vars, 0, [], Vars):- !.
split([V|Vars], Size, [V|Row], Rows):-
    Size1 is Size - 1, 
    split(Vars, Size1, Row, Rows).

makeConstraints(Cols, Rows, Duration):-
    checkTasks(Rows),
    checkMachines(Cols, Duration).

checkTasks([]).
checkTasks([R|Rows]):-
    sum(R, #=, 1),
    checkTasks(Rows).

checkMachines([], _).
checkMachines([C|Cols], Duration):-
    duration(D),
    scalar_product(D, C, #=<, Duration),
    checkMachines(Cols, Duration).

sum([], 0).
sum([L|List], L + Sum):-
    sum(List, Sum).
