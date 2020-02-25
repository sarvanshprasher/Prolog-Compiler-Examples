% author - Sarvansh Prasher
% version 1.0
% date - 18th February 2020

% queens(N,Qs) gives the solution where N represents how many queens need to be there
% on board and Qs will give you the solution of where will it be kept on board.

:- use_module(library(clpfd)).

queens(N,Solution) :- generateRowList(N,Rows), nQueens(Rows,[],Solution).

% Predcicate generateRowList(N,Rows) is for generating a list
% of N elements which will help in filling rows.
generateRowList(N,Rows) :- generateRowList(1,N,Rows).
generateRowList(N,N,[N]) :-!.
generateRowList(Rows,N,[Rows|List]) :- N >Rows, N >1, R1 is Rows+1,
    generateRowList(R1,N,List).

% select(Element,List1,List2) predicate will be used for selecting the row
% from main rows list
select([L|L1],L1,L).
select([R|R1],[R|R2],L):-
	select(R1,R2,L).

% Predicate nQueens(Rows,ChessBoard,Solution) relation for solving problem of
% where to put queen on chessboard.
nQueens([],Solution,Solution).
nQueens(Rows,ChessBoard,Solution) :- select(Rows,R1,RemainingRows) ,
    			checkIfValid(ChessBoard,RemainingRows),
    			nQueens(R1,[RemainingRows|ChessBoard],Solution).

% checkIfValid(ChessBoard,RemainingRows) for checking row,column,diagonal wise
% whether it is a valid move.
checkIfValid(ChessBoard,RemainingRows):- checkIfValid(ChessBoard,RemainingRows,1).
checkIfValid([],_,_):-!.
checkIfValid([RemainingList|R], Rows, Distane0) :-
        Rows #\= RemainingList,
        abs(Rows - RemainingList) #\= Distane0,
        Distance1 #= Distane0 + 1,
        checkIfValid(R, Rows, Distance1).
