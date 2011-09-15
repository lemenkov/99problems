% https://sites.google.com/site/prologsite/prolog-problems/1

-module(prolists).

-compile([export_all]).

% 1.01 (*) Find the last element of a list.
%    Example:
%        ?- my_last(X,[a,b,c,d]).
%    X = d
problem1([Last| []]) ->
	Last;
problem1([_|List]) ->
	problem1(List).

% 1.02 (*) Find the last but one element of a list. [1,2,3,4,5] -> 4.
problem2([Elem, _ | []]) ->
	Elem;
problem2([_, Next | Rest]) ->
	problem2([Next | Rest]).

% 1.03 (*) Find the K'th element of a list.
%    The first element in the list is number 1.
%    Example:
%        ?- element_at(X,[a,b,c,d,e],3).
%    X = c
problem3([Elem | _], 1) ->
	Elem;
problem3([_| Rest], Number) ->
	problem3(Rest, Number - 1).

% 1.04 (*) Find the number of elements of a list.
problem4(List) ->
	problem4(List, 0).
problem4([], Number) ->
	Number;
problem4([_ | Rest], Number) ->
	problem4(Rest, Number + 1).

% 1.05 (*) Reverse a list.
problem5(List) ->
	problem5(List, []).
problem5([], RevList) ->
	RevList;
problem5([Elem | Rest], RevList) ->
	problem5(Rest, [Elem | RevList]).

% 1.06 (*) Find out whether a list is a palindrome.
%    A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
problem6(List) ->
	problem6(List, List, []).
problem6([], List,  RevList) ->
	List == RevList;
problem6([Elem | Rest], List,  RevList) ->
	problem6(Rest, List, [Elem | RevList]).

% 1.07 (**) Flatten a nested list structure.
problem7(DeepList) ->
	problem7(DeepList, []).
problem7([], FlatList) ->
	FlatList;
problem7([ [Elem | Rest2] | Rest], FlatList) ->
	problem7([Elem, Rest2 | Rest], FlatList);
problem7([[] | Rest], FlatList) ->
	problem7(Rest, FlatList);
problem7([Elem | Rest], FlatList) ->
	problem7(Rest, FlatList ++ [Elem]).

% 1.08 (**) Eliminate consecutive duplicates of list elements.
problem8(List) ->
	problem8(List, []).
problem8([], Result) ->
	Result;
problem8([Elem, Elem | Rest], Result) ->
	problem8([Elem | Rest], Result);
problem8([Elem | Rest], Result) ->
	problem8(Rest, Result ++ [Elem]).

% 1.09 (**) Pack consecutive duplicates of list elements into sublists.
problem9([Elem | Rest]) ->
	problem9(Rest, [Elem], []).
problem9([], Last, Result) ->
	Result ++ [Last];
problem9([Elem | Rest], [Elem | Something], Result) ->
	problem9(Rest, [Elem | Something] ++ [Elem], Result);
problem9([Elem | Rest], Something, Result) ->
	problem9(Rest, [Elem], Result ++ [Something]).

% 1.10 (*) Run-length encoding of a list.
problem10(List) ->
	problem10(problem9(List), []).
problem10([], Result) ->
	Result;
problem10([ [Elem | _] = ElemList | Rest], Result) ->
	problem10(Rest, Result ++ [[length(ElemList), Elem]]).

% 1.11 (*) Modified run-length encoding.
problem11(List) ->
	problem11(problem9(List), []).
problem11([], Result) ->
	Result;
problem11([ [Elem] | Rest], Result) ->
	problem11(Rest, Result ++ [Elem]);
problem11([ [Elem | _] = ElemList | Rest], Result) ->
	problem11(Rest, Result ++ [[length(ElemList), Elem]]).

% 1.12 (**) Decode a run-length encoded list.
problem12(List) ->
	problem12(List, []).
problem12([], Result) ->
	Result;
problem12([[0, _] | List], Result) ->
	problem12(List, Result);
problem12([[Num, Elem] | List], Result) ->
	problem12([[Num-1, Elem] | List], Result ++ [Elem]);
problem12([Elem | List], Result) ->
	problem12(List, Result ++ [Elem]).

% 1.13 (**) Run-length encoding of a list (direct solution).
problem13([Elem | Rest]) ->
	problem13(Rest, [Elem], []).
problem13([], [Last | []], Result) ->
	Result ++ [Last];
problem13([], [Last | _] = List, Result) ->
	Result ++ [[length(List), Last]];
problem13([Elem | Rest], [Elem | Something], Result) ->
	problem13(Rest, [Elem | Something] ++ [Elem], Result);
problem13([Elem | Rest], [Something | []], Result) ->
	problem13(Rest, [Elem], Result ++ [Something]);
problem13([Elem | Rest], [Something | _] = SomeList, Result) ->
	problem13(Rest, [Elem], Result ++ [[length(SomeList), Something]]).

% 1.14 (*) Duplicate the elements of a list.
problem14(List) ->
	problem14(List, []).
problem14([], Result) ->
	Result;
problem14([Elem | List], Result) ->
	problem14(List, Result ++ [Elem, Elem]).

% 1.15 (**) Duplicate the elements of a list a given number of times.
problem15(List, Multiply) ->
	problem15(List, Multiply, Multiply, []).
problem15([], _, _, Result) ->
	Result;
problem15([_ | List], Multiply, 0, Result) ->
	problem15(List, Multiply, Multiply, Result);
problem15([Elem | List], Multiply, Count, Result) ->
	problem15([Elem | List], Multiply, Count - 1, Result ++ [Elem]).

% 1.16 (**) Drop every N'th element from a list.
problem16(List, Number) ->
	problem16(List, Number, Number, []).
problem16([], _, _, Result) ->
	Result;
% Off-by-one?
problem16([_ | Rest], Number, 1, Result) ->
	problem16(Rest, Number, Number, Result);
problem16([Elem | Rest], Number, Count, Result) ->
	problem16(Rest, Number, Count - 1, Result ++ [Elem]).

% 1.17 (*) Split a list into two parts; the length of the first part is given.
problem17(List, Size) ->
	problem17(List, Size, []).
problem17(List, 0, Rest) ->
	{Rest, List};
problem17([Elem | Rest], Number, Result) ->
	problem17(Rest, Number - 1, Result ++ [Elem]).

% 1.18 (**) Extract a slice from a list.
problem18(List, Begin, End) ->
	% Off-by-one?
	problem18(List, Begin - 1, End - Begin + 1, []).
problem18(_, 0, 0, Result) ->
	Result;
problem18([Elem | Rest], 0, End, Result) ->
	problem18(Rest, 0, End - 1, Result ++ [Elem]);
problem18([_ | Rest], Start, End, []) ->
	problem18(Rest, Start - 1, End, []).

% 1.19 (**) Rotate a list N places to the left.
problem19(List, Shift) ->
	problem19(List, Shift, []).
problem19(Rest, 0, Ret) ->
	Rest ++ Ret;
problem19([S | Rest], Shift, Ret) ->
	problem19(Rest, Shift - 1, Ret ++ [S]).

% 1.20 (*) Remove the K'th element from a list.
problem20(List, Pos) ->
	problem20(List, Pos, []).
	% off-by-one
problem20([Prev | Rest], 1, Head) ->
	{Prev, Head ++ Rest};
problem20([Prev | Rest], Pos, Head) ->
	problem20(Rest, Pos - 1, Head ++ [Prev]).

% 1.21 (*) Insert an element at a given position into a list.
problem21(List, Pos, Elem) ->
	problem21(List, Pos, Elem, []).
% off-by-one
problem21(Rest, 1, Elem, Head) ->
	Head ++ [Elem] ++ Rest;
problem21([Prev | Rest], Pos, Elem, Head) ->
	problem21(Rest, Pos - 1, Elem, Head ++ [Prev]).

% 1.22 (*) Create a list containing all integers within a given range.
% lists:seq(1,10).
problem22(Begin, End) ->
	problem22(Begin, End, []).
problem22(End, End, Ret) ->
	Ret ++ [End];
problem22(Begin, End, Ret) ->
	problem22(Begin + 1, End, Ret ++ [Begin]).

% 1.23 (**) Extract a given number of randomly selected elements from a list.
problem23(List, Number) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	problem23(List, Number, []).
problem23(_List, 0, Ret) ->
	Ret;
problem23(List, Number, Ret) ->
	Num = random:uniform(length(List)),
	{Elem, Rest} = problem20(List, Num),
	problem23(Rest, Number - 1, Ret ++ [Elem]).

% 1.24 (*) Lotto: Draw N different random numbers from the set 1..M.
problem24(Begin, End, Number) ->
	problem23(problem22(Begin, End), Number).

% 1.25 (*) Generate a random permutation of the elements of a list.
problem25(List) ->
	problem23(List, length(List)).

% 1.26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
problem26(1, List) ->
	[ [X] || X <- List];
problem26(K, List) ->
	[ [X | Y] || X <- List, Y <- problem26(K - 1, List -- [X])].

% 1.27 (**) Group the elements of a set into disjoint subsets.
problem27(List, [K0, K1, _]) ->
	[[X, Y, List -- (X ++ Y)] || X <- problem26(K0, List), Y <- problem26(K1, List -- X)].

% 1.28 (**) Sorting a list of lists according to length of sublists

% a) We suppose that a list (InList) contains elements that are lists
% themselves. The objective is to sort the elements of InList according to their
% length. E.g. short lists first, longer lists later, or vice versa.

problem28a(ListOfLists) ->
	[ Y || {_, Y} <- qsort([{length(X), X} || X <- ListOfLists]) ].

qsort([]) ->
	[];
qsort([Anything]) ->
	[Anything];
qsort(OrigList) ->
	{Pivot, List} = problem20(OrigList, problem4(OrigList)),
	qsort(Pivot, List, [], []).

qsort(Pivot, [], Lesser, Greater) ->
	qsort(Lesser) ++ [Pivot] ++ qsort(Greater);
qsort(Pivot, [Head | Tail], Lesser, Greater) ->
	case bigger(Head, Pivot) of
		true ->
			qsort(Pivot, Tail, Lesser, Greater ++ [Head]);
		_ ->
			qsort(Pivot, Tail, Lesser ++ [Head], Greater)
	end.

bigger(_, []) ->
	true;
bigger({Size0, _}, {Size1, _}) when Size0 > Size1 ->
	true;
bigger({Size0, _}, {Size1, _}) when Size0 < Size1 ->
	false;
bigger({_, List0}, {_, List1}) when List0 > List1 ->
	true;
bigger(_, _) ->
	false.
