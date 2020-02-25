  % author - Sarvansh Prasher
  % version 1.0
  % date - 19th February 2020

  % color_map(L) defines the relation where L contains all the vertices with
  % colors associated with them.

  % Rules which represent the vertexes
  vertex(1).
  vertex(2).
  vertex(3).
  vertex(4).
  vertex(5).
  vertex(6).

  % Rule for colors
  color(red).
  color(green).
  color(yellow).
  color(blue).

  % Rules which represent edge between vertices.
  edge(2,1).
  edge(2,3).
  edge(2,5).
  edge(1,6).
  edge(1,4).
  edge(1,3).
  edge(5,3).
  edge(5,4).
  edge(4,6).
  edge(4,3).
  edge(3,6).

  % Predicate for connecting edges to two nodes
  adjacent(X,Y) :- edge(Y,X);edge(X,Y).

  % Rules for defining color predicate which will color the vertices
  colorVertex([]).
  colorVertex([colors(_,C)|Vertex]) :- color(C),colorVertex(Vertex).

  % Rules for defining two vertexes having different color
  linkedVertextColor([],_).
  linkedVertextColor([(Vertex1,Vertex2)|RemainingList],FinalList):- member(colors(Vertex1,C1),FinalList),
    member(colors(Vertex2,C2),FinalList),dif(C1,C2),
    linkedVertextColor(RemainingList,FinalList).

  % Rules for getting colored map after giving list and finding all objects in the given edges pair
  color_map(L) :-
    findall((Vertex1, Vertex2), edge(Vertex1, Vertex2), E),
    findall(Vertex1, vertex(Vertex1), Vertexes),
    findall(colors(Vertex2, _), member(Vertex1, Vertexes), L),
    linkedVertextColor(E,L),
    colorVertex(L).
