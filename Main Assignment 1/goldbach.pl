% author - Sarvansh Prasher
% version 1.0
% date - 15th February 2020

% goldbach(N,L) :- N is even number and L is the list of the two
% prime numbers that when added sums up to N.

% Relation for finding whether a number is prime number
% (Submitted in Mini Assignment 2)
div(X, Y, Z) :- Z is X / Y.
greater(X, Y) :- X < Y.
divisible(X, Y) :- div(X, Y, Z), integer(Z).
notPrime(X, Y) :- Y > 1, divisible(X, Y).
notPrime(X, Y) :- greater(Y, X / 2), notPrime(X, Y+1).
notPrime(Z) :- Z > 2, notPrime(Z, 2).
prime(Z) :- not(notPrime(Z)).


% Relation for finding list of prime numbers.

% Condition for checking prime number combinations for given number N.
primes(N,N1) :- N1 is N + 2, prime(N1),!.
primes(N,N1) :- N2 is N + 2, primes(N2,N1).

% After one number has been found, checking whether N-SolutionFound(Z) is also
% a prime number and making sure it is greater than Z.
goldbach(N,[Z,Z1],Z) :- Z1 is N - Z, prime(Z1), Z<Z1.
goldbach(N,L,Z) :- Z < N, primes(Z,Z1), goldbach(N,L,Z1).

% Converting goldbach/2 to goldbach/3 by taking extra variable 3.
goldbach(N,L) :- N mod 2 =:= 0, N > 4, goldbach(N,L,3).
