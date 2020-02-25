% author - Sarvansh Prasher
% version 1.0
% date - 13th February 2020

% combination(N,T,L) defines relation where L will be the list formed by T by
% N combinations.

combination(0, _, []).
combination(N, [H|T], [H|L]) :- N1 is (N - 1),  combination(N1, T, L).
combination(N, [_|T], L) :- N > 0, combination(N, T, L).
