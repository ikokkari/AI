/* As an exercise, implement some built-in predicates for lists. To
 * ensure that there are no name clashes, name the implementation of
 * each built-in predicate foo as my_foo in our code. As with all
 * programming, a very small set of given operations is needed for
 * the language to be able to express the other operations and thus
 * every possible computation. */

/* Linear stack version without tail calls. */

my_length([], 0).
my_length([_ | T], N) :-
    my_length(T, N2),
    plus(N2, 1, N).

/* O(1) space version with accumulators and tail calls. */

my_length_acc(L, N) :-
    my_length_acc(L, 0, N).

my_length_acc([], N, N).

my_length_acc([_ | T], Acc, N) :-
    plus(Acc, 1, Acc2),
    my_length_acc(T, Acc2, N).

/* Like member/2, but cuts at first occurrence of X. */

my_memberchk(X, [X|_]) :-
    !.
my_memberchk(X, [_|L]) :-
    my_memberchk(X, L).

/* In Prolog, between/3 is used to simulate for-loops through integers.
 * Of course, this being Prolog, this predicate is a lot more flexible
 * than the purely forward-going for-loop of imperative languages. This
 * predicate could be made even more flexible using constraint logic
 * programming, but that is a topic for later. */

my_between(M, N, _) :- M > N, !, fail.

my_between(N, N, N) :- !.

my_between(M, N, M) :- M < N.

my_between(M, N, I) :-
    M2 is M + 1,
    my_between(M2, N, I).

/* Use of the metapredicate var(X) makes my_nth1 reversible. */

my_nth1(1, [X|_], X).

my_nth1(N, [_|T], X) :-
    var(N),
    !,
    my_nth1(N2, T, X),
    N is N2 + 1.

my_nth1(N, [_|T], X) :-
    N > 1,
    N2 is N - 1,
    my_nth1(N2, T, X).

/* The metapredicate var(X) is again used to make my_take reversible. See
 * what happens if you move the base case formula before the other two. */

my_take(N, [X|L1], [X|L2]) :-
    nonvar(N),
    N > 0,
    !,
    N2 is N - 1,
    my_take(N2, L1, L2).

my_take(N, [X|L1], [X|L2]) :-
    var(N),
    my_take(N2, L1, L2),
    N is N2 + 1.

/* Base case now comes exceptionally after the other cases. Why? */

my_take(0, _, []). 

/* Ditto with drop. */

my_drop(N, [_|L], L2) :-
    nonvar(N),
    N > 0,
    !,
    N2 is N - 1,
    my_drop(N2, L, L2).

my_drop(N, [_|L], L2) :-
    var(N),
    my_drop(N2, L, L2),
    N is N2 + 1.

my_drop(0, L, L).

/* Create a copy of list with every occurrence of X left out. This
 * could be made more memory-efficient in cases where the list does
 * not contain any occurrences of X, since the original immutable
 * list could serve as the answer. */

my_delete([], _, []).
my_delete([X|T], X, T2) :-
    !,
    my_delete(T, X, T2).
my_delete([H|T], X, [H|T2]) :-
    my_delete(T, X, T2).