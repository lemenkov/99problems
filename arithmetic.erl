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
