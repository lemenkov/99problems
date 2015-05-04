% http://imranontech.com/2007/01/24/using-fizzbuzz-to-find-developers-who-grok-coding/

-module(fizzbuzz).

-compile([export_all]).

fizzbuzz() ->
	[ case {X rem 3, X rem 5} of {0, 0} -> fizzbuzz; {0, _} -> fizz; {_, 0} -> buzz; _ -> X end || X <- lists:seq(1,100)].
