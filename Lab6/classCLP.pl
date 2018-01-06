
% Generate & Test problems

% SEND MORE MONEY problem
p:- % generate
    Digs = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    L = [S, E, N, D, M, O, R, Y],
    append(D1, [_|D2], Digs),
    append(D1, D2, Digs1),
    permute(L, Dist1),
    % test
    X is S*1000 + E*100 + N*10 + D +
         M*1000 + O*100 + R*10 + E,
    X is M*10000 + O*1000 + N*100 + E*10 + Y,
    write(L).
    
    
% Generate & Test entrelazados -> Generar un trocito de la solución
% y luego con el test comprobar que funciona.
% CLPFD: Constraint Logic Programming Finite Domains

:- use_module(library(clpfd)).

p2 :- L = [S, E, N, D, M, O, R, Y],
      L ins 1..9, % Domain
      S*1000 + E*100 + N*10 + D +
      M*1000 + O*100 + R*10 + E #=
      X is M*10000 + O*1000 + N*100 + E*10 + Y,
      labeling(L),
      write(L).
      

ejemplo :- L = [X1, X2, X3, X4],
           L ins 0..1,
           % -X1 v X2 x -X4
           (1 - X1) + X2 + (1 - X4) #>= 1,
           labeling(L),
           write(L).
           
ejemplo2 :- L = [X, Y, Z, U],
            L ins 1..6,
            X #> Y + 1,
            Z #= X + Y,
            Z #= 2 * U,
            labeling(L),
            write(L).
            
% El labeling también se puede usar para maximizar o minimizar.
% max(Expr), min(Expr)

% Se pueden usar constraints "globales": all_different
% Es mejor usar all_distinct porque tiene más potencia de propagación,
% ergo es más eficiente.

sudoku :- L = [X11, ..., X99], % Cada casilla
          L ins 1..9,
          % ahora tocaría meter los valores conocidos por el problema
          X37 #= 8, % Por ejemplo
          ... ,
          ... ,
          all_different([X11, X12, ... X19]), % Esto hay que hacerlo por cada combinación
                                              % En total 27 líneas así.
          labeling(L),
          write(L).
          
          
% Para problemas en que nos den una "N" y no podemos escribir todos los constraints a mano,
% o cuando es un problema demasiado grande y tedioso de escribir, lo mejor es picar una función
% recursiva "imponerConstraints" que te los escriba para cada uno. 
