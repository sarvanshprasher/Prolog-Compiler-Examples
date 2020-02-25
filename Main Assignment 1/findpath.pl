% author - Sarvansh Prasher
% version 1.0
% date - 10th February 2020

% findpath(X,Y,Weight,Path) - this relation will represent
% the paths of going from X to Y and weight of those individual paths.

% path(node1,node2,distance) represents the relation of connection of one node to
% another node
path(a,b,1).
path(a,c,6).
path(b,e,1).
path(b,d,3).
path(b,c,4).
path(d,e,1).
path(c,d,1).

findpath(Z1,Z2,N) :- path(Z2,Z1,N).
findpath(Z1,Z2,N) :- path(Z1,Z2,N).

findpath(Z1,Z2,N,L) :- findpath(Z1,Z2,N,L,0).

findpath(Z1, Z2, N, [Z1,Z2],_) :- path(Z1, Z2, N).

findpath(Z1, Z2, N, [Z1|P], V) :- \+ member(Z1, V), path(Z1, Z, N1),
    findpath(Z, Z2, N2, P, [Z1|V]),
    N is N1 + N2.
