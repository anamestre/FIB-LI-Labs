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
    transpose(Rows, Cols),
    duration(D),
    sum_list(D, MaxDuration),
    Duration in 1..MaxDuration, % En realitat no pot estar en 1
    makeConstraints(Cols, Rows, Duration),
    labeling([min(Duration)], [Duration|Vars]),
    printOutput(Rows, Duration).

matrixByRows([], _, []).
matrixByRows(Vars, Size, [Row|Rows]):-
    split(Vars, Size, Row, RowsLeft),
    matrixByRows(RowsLeft, Size, Rows).

split(Vars, 0, [], Vars).
split([V|Vars], Size, [V|Row], RowsLeft):-
    Size1 is Size - 1,
    split(Vars, Size1, Row, RowsLeft).


makeConstraints(Cols, Rows, Dur):-
    checkMachines(Cols, Dur),
    checkTasks(Rows).

checkMachines([], _).
checkMachines([Ma|Machines], Dur):-
    duration(D),
    scalar_product(D, Ma, #=<, Dur),
    checkMachines(Machines, Dur).

checkTasks([]).
checkTasks([T|Tasks]):-
    sum(T, #=, 1),
    checkTasks(Tasks).

printOutput(Tasks, Dur):-
    printTasks(Tasks, 1),
    write(Dur).

printTasks([], _).
printTasks([T|Tasks], Num):-
    write('Task number '),
    write(Num),
    getMachine(T, M, 1),
    write(' is executed by Machine number '),
    write(M), nl,
    Num1 is Num + 1,
    printTasks(Tasks, Num1).

getMachine([1|_], M, M).
getMachine([_|Tasks], M, It):-
    It1 is It + 1,
    getMachine(Tasks, M, It1).
