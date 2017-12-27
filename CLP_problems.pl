% Spring 2015
% Write a Prolog predicate shortest([I1,J1], [I2,J2]) that writes to the output the shortest path
% on a chess board a horse needs to go from square [I1,J2] to square[I2,J2]. 
% Coordinates I,J are in 1..8. The path is the list of intermediate board squares. 
% Your solution should be short, clean and simple, without any comments.


shortest([I1,J1], [I2,J2]):- path([I1, J1], [I2, J2], [[I1, J1]], Path),
    						 between(1, 64, N),
    						 length(Path, N), % Si només tenim 64 caselles, només pot passar per aquestes.
    						 write(Path), nl.

path(F, F, Path, Path):- !.
path(From, To, UntilNow, TotalPath):- oneStep(From, NextStep),
    								  \+member(NextStep, UntilNow),
    								  path(NextStep, To, [NextStep|UntilNow], TotalPath).

oneStep([FromX, FromY], [ToX, ToY]):- member([StepX, StepY], [[1, 2], [2, 1]]),
    								  member(SignX, [1, -1]),
    								  member(SingY, [1, -1]),
    								  ToX is SignX * StepX,
    								  ToY is SignY * StepY,
    								  between(1, 8, ToX),
    								  between(1, 8, ToY).
