% D'aquesta manera activem el mòdul de CLP.
:- use_module(library(clpfd)).

% Entrades:
ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).


main:- 
    ejemplo(0, Amount, Coins),
    length(Coins, NumCoins),
    length(Vars, NumCoins),
    Vars ins 0..Amount,
    restriccions(Vars, Coins, Amount, 0),
    suma(Vars, SumVars),
    labeling([min(SumVars)], Vars),
    write(Vars).
    
    
restriccions([], [], T, T).
restriccions([V|Vars], [C|Coins], Total, Actual):-
    V * C  + Actual #=< Total,
    Act2 #= V * C + Actual,
    restriccions(Vars, Coins, Total, Act2).
    
    
suma([], 0).
suma([V|Vars], Suma + V):-
    suma(Vars, Suma).
    