% http://yuilop.com/intl/jobs/server-core-engineer-java-erlang/

-module(yuilop).

-compile([export_all]).

% Build a function that given a list of Integers in the range [-65000,65000],
% the function returns true if any subset of the list summed is equal to zero.
% False otherwise.

% Example:
% [0, 1, 2, -3] returns true. As 1+2+(-3)==0.
% [1, 2, 3, -8] returns false. As no subset summed is equal 0.
% [1, 4, 5, 2, -3] returns true.

% To be honest if we consider more specific dataset we could try to play with
% different approaches (analyse the entire set for non-negative, check for
% magnitudes of negatives, check for zero element etc) thus removing some sets
% from further analysis.

% The straitforward solution is to try each combination of K elements
% from L = length(List), with increasing K for each next round

fun1(List) when is_list (List) ->
	Length = length(List),
	fun1(1, Length, List).
fun1(K, L, List) ->
	case lists:any(fun(X) -> 0 == lists:foldl(fun erlang:'+'/2, 0, X) end, perm(K, List)) of
		true -> true;
		false when K == L -> false;
		false -> fun1(K+1, L, List)
	end.

perm(1, List) ->
	[[X] || X <- List];
perm(K, List) ->
	[[X | Y] || X <- List, Y <- perm(K - 1, List -- [X])].

% Build a function that given a list of Integers pairs in the range
% [-65000,65000], that represents {X, Y} Coordinates in a Plane. The function
% returns the two closest pairs.

% Example:
% [{0,0}, {1,20}, {5, 2}] returns [{0,0}, {5, 2}]
% [{-10,10}, {1,5}, {4, 3}] returns [{1,5}, {4, 3}]

% Straightforward solution - just create a list from every possible pair...

% Consider the entire array of possible pairs as a matrix, e.g. if the initial
% set is [X1, X2, X3, X4] then the resulting matrix will be
%
% || {X1, X1}  {X1, X2}  {X1, X3}  {X1, X4} ||
% || {X2, X1}  {X2, X2}  {X2, X3}  {X2, X4} ||
% || {X3, X1}  {X3, X2}  {X3, X3}  {X3, X4} ||
% || {X4, X1}  {X4, X2}  {X4, X3}  {X4, X4} ||
%
% We need only pairs above the main diagonal, e.g.
%
% || --------  {X1, X2}  {X1, X3}  {X1, X4} ||
% || --------  --------  {X2, X3}  {X2, X4} ||
% || --------  --------  --------  {X3, X4} ||
% || --------  --------  --------  -------- ||
%
% because we don't care about the order in each pair (the distance between X1
% and X2 is equal to the distance between X2 and X1) and we don't case about
% main diagonal (it's pointless to calculate distance from X1 to X1)

% How to test:
% List = [{X,Y} || X <- lists:seq(1,40), Y <- lists:seq(1,40)].
% timer:tc(yuilop, fun2, [List]).

fun2(List) when is_list (List) ->
	fun2([], List).
fun2(Ret, [Head | Tail]) ->
	Ret0 = [{Head, Y} || Y <- Tail],
	fun2(Ret0 ++ Ret, Tail);
% ...then compute distance for each pair and find minimal one.
fun2(Ret, []) ->
	{_, X, Y} = lists:min([{len(X,Y), X, Y} || {X,Y} <- Ret]),
	{X,Y}.

len({X0, Y0}, {X1, Y1}) when
	is_integer(X0),
	is_integer(Y0),
	is_integer(X1),
	is_integer(Y1),
	-65000 =< X0, X0 =< 65000,
	-65000 =< Y0, Y0 =< 65000,
	-65000 =< X1, X1 =< 65000,
	-65000 =< Y1, Y1 =< 65000 ->
		math:sqrt((X1 - X0)*(X1 - X0) + (Y1 - Y0)*(Y1 - Y0)).
