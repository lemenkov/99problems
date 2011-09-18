% https://sites.google.com/site/prologsite/prolog-problems/2

-module(arithmetic).

-compile([export_all]).

% 2.01 (**) Determine whether a given integer number is prime.
% http://www.f1gmat.com/data-sufficiency/how-to-identify-a-prime-number
problem1(IsPrime) when is_integer(IsPrime), IsPrime < 2 ->
	false;
problem1(2) ->
	true;
problem1(3) ->
	true;
problem1(5) ->
	true;
problem1(IsPrime) when is_integer(IsPrime) ->
	case IsPrime rem 10 of
		0 -> false;
		2 -> false;
		4 -> false;
		5 -> false;
		6 -> false;
		8 -> false;
		_ ->
			check_prime(IsPrime, round(math:sqrt(IsPrime)), 3)
	end.

check_prime(IsPrime, Sqrt, Sqrt) ->
	case Sqrt * Sqrt of
		IsPrime -> false;
		_ -> true
	end;
check_prime(IsPrime, Sqrt, Start) ->
	case IsPrime rem Start of
		0 -> false;
		_ -> check_prime(IsPrime, Sqrt, Start + 1)
	end.

% 2.02 (**) Determine the prime factors of a given positive integer.
problem2(1) ->
	[1];
problem2(Num) when is_integer(Num), Num > 1 ->
	problem2(Num, 2, []).

problem2(1, _, List) ->
	List;
problem2(Num, Divider, List) ->
	case problem1(Divider) of
		true ->
			case Num rem Divider of
				0 ->
					problem2(round(Num / Divider), 2, List ++ [Divider]);
				_ ->
					problem2(Num, Divider + 1, List)
			end;
		false ->
			problem2(Num, Divider + 1, List)
	end.

% 2.03 (**) Determine the prime factors of a given positive integer (2).
problem3(Num) when is_integer(Num), Num > 0 ->
	swap(prolists:problem10(problem2(Num)), []).

swap([], Processed) ->
	Processed;
swap([[Freq, Val] | Rest], Processed) ->
	swap(Rest, Processed ++ [[Val, Freq]]).

% 2.04 (*) A list of prime numbers.
problem4(Begin, End) when is_integer(Begin), is_integer(End), Begin =< End ->
	[X || X <- prolists:problem22(Begin, End), problem1(X) == true].

% 2.05 (**) Goldbach's conjecture.
problem5(Number) when is_integer(Number), Number > 2, Number rem 2 == 0 ->
	problem5(Number, 2).

problem5(Number, IsPrime) ->
	case {problem1(IsPrime), problem1(Number - IsPrime)} of
		{true, true} -> [IsPrime, Number - IsPrime];
		_ -> problem5(Number, IsPrime + 1)
	end.

% 2.06 (**) A list of Goldbach compositions.
problem6(Begin, End) when is_integer(Begin), is_integer(End), Begin =< End ->
	problem6(Begin, End, 0).

problem6(Begin, End, Limit) when is_integer(Begin), is_integer(End), is_integer(Limit), Begin =< End ->
	[ {X, {Y, X - Y}} || X <- prolists:problem22(Begin, End), X rem 2 == 0, Y <- prolists:problem22(3, X), problem1(Y) == true, problem1(X - Y) == true, Y >= Limit].

% 2.07 (**) Determine the greatest common divisor of two positive integer numbers.
problem7(A,B) when is_integer(A), is_integer(B), A > 0, B > 0, A < B ->
	problem7(B,A);
problem7(A,B) when is_integer(A), is_integer(B), A > 0, B > 0 ->
	case A rem B of
		0 -> B;
		_ ->
			problem7(B, A rem B)
	end.

% 2.08 (*) Determine whether two positive integer numbers are coprime.
problem8(A,B) when is_integer(A), is_integer(B), A > 0, B > 0 ->
	case problem7(A,B) of
		1 -> true;
		_ -> false
	end.
