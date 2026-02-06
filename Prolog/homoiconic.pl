:- use_module(library(clpfd)).

/* === HELPER PREDICATES === */
sq(N, N2) :- N2 is N * N.
even(N) :- N mod 2 =:= 0.
my_times(X, Y, Z) :- Z is X * Y.

/* === FUNCTIONAL PROGRAMMING PATTERNS === */

% filter: Keep elements satisfying predicate
my_filter(_, [], []).
my_filter(P, [H|T], Result) :-
    (   call(P, H)
    ->  Result = [H|T2]
    ;   Result = T2
    ),
    my_filter(P, T, T2).

% map: Transform each element
my_map(_, [], []).
my_map(P, [H|T], [H2|T2]) :-
    call(P, H, H2),
    my_map(P, T, T2).

% take_while: Keep elements until predicate fails
my_take_while(_, [], []).
my_take_while(P, [H|T], [H|T2]) :-
    call(P, H),
    my_take_while(P, T, T2).
my_take_while(P, [H|_], []) :-
    \+ call(P, H).

% fold/reduce: Accumulate with binary operation
my_fold(Acc, _, [], Acc).
my_fold(Acc, P, [H|T], Result) :-
    call(P, Acc, H, NewAcc),
    my_fold(NewAcc, P, T, Result).

/* === TERM REWRITING === */

% rewrite(Term, Old, New, Result)
% Replace all occurrences of Old with New in Term
rewrite(T, X, Y, Y) :- T == X, !.
rewrite(T, _, _, T) :- var(T), !.
rewrite(T, X, Y, R) :-
    T =.. [F|Args],
    rewrite_list(Args, X, Y, Args2),
    R =.. [F|Args2].

rewrite_list([], _, _, []).
rewrite_list([H|T], X, Y, [H2|T2]) :-
    rewrite(H, X, Y, H2),
    rewrite_list(T, X, Y, T2).

/* === TESTING FRAMEWORK === */

test(Query, X, Inf, Res, Test) :-
    statistics(inferences, I1),
    findall(X, Query, Res),
    statistics(inferences, I2),
    Inf is I2 - I1,
    (   call(Test)
    ->  format('✓ Pass (~w inferences)~n', [Inf])
    ;   format('✗ Fail (~w inferences)~n', [Inf]),
        fail
    ).
