% https://sites.google.com/site/prologsite/prolog-problems/4

-module(btrees).

-compile([export_all]).

% 4.01 (*) Check whether a given term represents a binary tree
problem1(nil) -> true;
problem1({Atom, nil, nil}) when is_atom(Atom) -> true;
problem1({Atom, Anything, nil}) when is_atom(Atom) -> problem1(Anything);
problem1({Atom, nil, Anything}) when is_atom(Atom) -> problem1(Anything);
problem1({Atom, A1, A2}) when is_atom(Atom) -> problem1(A1) and problem1(A2);
problem1(_) -> false.


