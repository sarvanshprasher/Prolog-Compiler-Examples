% author - Sarvansh Prasher
% version 1.0
% date - 17th February 2020

% hanoi (X, a, c, b) defines relation where number of discs are X
% ,a is starting peg,c is auxillary peg,b is final peg.

% Base case when we have only one disc and we have to put disc from a to c
hanoi(1,LEFT,CENTER,_):-
    write('Move '),
    write(LEFT),
    write(' to '),
    write(CENTER),
    nl.

% Recursive case which handles the moving of discs from peg.
hanoi(X,LEFT,CENTER,RIGHT):-
    X>1,
    X1 is X-1,
    hanoi(X1,LEFT,RIGHT,CENTER),
    hanoi(1,LEFT,CENTER,_),
    hanoi(X1,RIGHT,CENTER,LEFT).
