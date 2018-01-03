
:- use_module(library(clpfd)).

% Now you are flying back from China, and you should write such a program to compute how many
% units of each one of six products you should take in your suitcase with capacity 80Kg, if you want
% to maximize the total value, and the products have the following weights (Kg) and values (Euros):
%           p1  p2  p3  p4  p5  p6
%   ------------------------------
%   weight: 1   2   3   5   6   7
%   value:  1   4   7   11  14  15


china:- Weight = [1, 2, 3, 5, 6, 7],
    	Value = [1, 4, 7, 11, 14, 15],
    	Capacity is 80,
    	length(Vars, 6),
    	Vars ins 0..80,
    	scalar_product(Weight, Vars, #=<, Capacity),
    	scalar_product(Value, Vars, #=, Val),
    	labeling([max(Val)], Vars), write(Vars).
