% @author : Sarvansh Prasher
% @version 1.0
% @created on 04-02-2020

% Predicates used :

% "expr" predicate will be the main function of any programming language which
% contains the program starting point

% "subt" predicate will be used for subtracting one expresion from another expresion.

% "mult" predicate will be used for multiplying one expresion from another expresion.

% "div" predicate will be used for dividing one expresion from another expresion.

% "paranthesis" predicate will be used for defining expressions around paranthesis.

% "term" predicate will be used for defining identifier and numbers.

% "term" predicate will be used for defining identifier and numbers.

% "identifier" predicate will be used for defining set of identifiers .

% "eval_expr" predicate will be used for evaluating an expresion
%  and giving out the output of expressions.

% "eval_expr" predicate will be used for evaluating an expresion
% and giving out the output of expressions.

% "E" predicate will be used as an environment variable which will be contstantly updated
% while program execution.

% "table" predicate will be used for eliminating left recursion.

:- table expr/3, mult/3, div/3, subt/3, paranthesis/3.

expr(t_expr(Z,Z1)) --> expr(Z),[+],subt(Z1).
expr(Z) --> subt(Z).

subt(t_subt(Z,Z1)) --> subt(Z),[-],mult(Z1).
subt(Z) --> mult(Z).

mult(t_mult(Z,Z1)) --> mult(Z),[*],div(Z1).
mult(Z) --> div(Z).

div(t_div(Z,Z1)) --> div(Z),[/],paranthesis(Z1).
div(Z) --> paranthesis(Z).

paranthesis(t_parant(Z)) --> ["("] , expr(Z) , [")"].
paranthesis(Z) -->term(Z).


term(t_term(Z)) --> [Z] ,{number(Z)}.

term(t_id(x)) --> [x].
term(t_id(y)) --> [y].
term(t_id(z)) --> [z].
term(t_id(u)) --> [u].
term(t_id(v)) --> [v].

% --------------------------------------*******---------------------------------

eval_expr(t_expr(Z,Z1),E,Val) :- eval_expr(Z,E,Val1),eval_expr(Z1,E,Val2),
    							Val is Val1+Val2.

eval_expr(t_subt(Z,Z1),E,Val) :- eval_expr(Z,E,Val1),eval_expr(Z1,E,Val2),
    							Val is Val1-Val2.

eval_expr(t_mult(Z,Z1),E,Val) :- eval_expr(Z,E,Val1),eval_expr(Z1,E,Val2),
    							Val is Val1*Val2.

eval_expr(t_div(Z,Z1),E,Val) :- eval_expr(Z,E,Val1),eval_expr(Z1,E,Val2),
    							Val is Val1/Val2.

eval_expr(t_parant(Z),E,Val) :- eval_expr(Z,E,Val).

eval_expr(t_term(Z),_,Z).
eval_expr(t_id(Z),E,Val) :- lookup(Z,E,Val).

lookup(Z, [(Z,Val)|_],Val).
lookup(Z, [_|T],Val) :- lookup(Z,T,Val).
