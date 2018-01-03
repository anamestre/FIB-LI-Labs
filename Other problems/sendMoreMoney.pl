:- use_module(library(clpfd)).

% ¿Qué dígitos diferentes tenemos que asignar a las letras S,E,N,D,M,O,R,Y, 
% de manera que se cumpla la suma S E N D + M O R E = M O N E Y?
 

sendMoreMoney:- 
    Vars = [S, E, N, D, M, O, R, Y],
    Vars ins 0..9,
    all_different(Vars),
    S * 1000 + E * 100 + N * 10 + D +
    M * 1000 + O * 100 + R * 10 + E #=
    M * 10000 + O * 1000 + N * 100 + E * 10 + Y,
    label(Vars),
    write(Vars).
