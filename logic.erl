% https://sites.google.com/site/prologsite/prolog-problems/3

-module(logic).

-compile([export_all]).

% 3.01 (**) Truth tables for logical expressions.
%	table(A,B,and(A,or(A,B))).
problem1() ->
	[io:format("~p\t~p\t\t~p~n", [X,Y,pred_and(X,pred_or(X,Y))]) || X <- [false, true], Y <- [false, true]],
	io:format("~n~n"),
	[io:format("~p\t~p\t\t~p~n", [X,Y,pred_and(X,pred_or(X,pred_not(Y)))]) || X <- [false, true], Y <- [false, true]],
	io:format("~n~n"),
	ok.

%%
%% Private fun
%%

pred_not(false) -> true;
pred_not(true) -> false.

pred_and(true, true) -> true;
pred_and(_, _) -> false.

pred_or(true, _) -> true;
pred_or(_, true) -> true;
pred_or(_, _) -> false.

pred_nand(A,B) -> pred_not(pred_and(A,B)).

pred_nor(A,B) -> pred_not(pred_or(A,B)).

pred_xor(true, true) -> false;
pred_xor(false, false) -> false;
pred_xor(_, _) -> true.

pred_impl(true, false) -> false;
pred_impl(_, _) -> true.

pred_equ(A, B) -> pred_not(pred_xor(A,B)).
