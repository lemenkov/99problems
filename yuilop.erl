% http://yuilop.com/intl/jobs/server-core-engineer-java-erlang/

-module(yuilop).

-compile([export_all]).

% Build a function that given a list of Integers in the range [-65000,65000],
% the function returns true if any subset of the list summed is equal to zero.
% False otherwise.

% Example:
% [0, 1, 2, -3] returns true. As 1+2+(-3)==0.
% [1, 2, 3, -8] returns false. As no subset summed is equal 0.

fun1(List) when is_list (List) ->
	fun1(List, 0).
fun1([], 0) -> true;
fun1([], _) -> false;
fun1([Head | Tail], Acc) when
	is_integer(Head),
	Head =< 65000,
	Head >= -65000 ->
	fun1(Tail, Acc + Head).

% Build a function that given a list of Integers pairs in the range
% [-65000,65000], that represents {X, Y} Coordinates in a Plane. The function
% returns the two closest pairs.

% Example:
% [{0,0}, {1,20}, {5, 2}] returns [{0,0}, {5, 2}]
% [{-10,10}, {1,5}, {4, 3}] returns [{1,5}, {4, 3}]

fun2(List) when is_list (List) ->
	[{_, X, Y} | _ ] = lists:sort(
		[ {len(X,Y), X, Y} || X <- List, Y <- List -- [X] ]
	),
	{X,Y}.

len({X0, Y0}, {X1, Y1}) when
	is_integer(X0),
	is_integer(Y0),
	is_integer(X1),
	is_integer(Y1),
	X0 =< 65000, X0 >= -65000,
	Y0 =< 65000, Y0 >= -65000,
	X1 =< 65000, X1 >= -65000,
	Y1 =< 65000, Y1 >= -65000 ->
		math:sqrt((X1 - X0)*(X1 - X0) + (Y1 - Y0)*(Y1 - Y0)).
