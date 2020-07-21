/* Prefix, suffix and sublist relations. Note the flexibity of
 * append as the fundamental building block of these operations. */

prefix(L1, L2) :- append(L1, _, L2).
suffix(L1, L2) :- append(_, L1, L2).
sublist([H|L1], L2) :-
    prefix([H|L1], L2).
sublist(L1, [_|L2]) :-
    sublist(L1, L2).

/* Determine whether the elements of list are strictly ascending. */

is_ascending([]).
is_ascending([_]) :- !.
is_ascending([X, Y | T]) :-
    X < Y,
    is_ascending([Y|T]).

/* Compute the accumulation of the given list. The use of plus
 * makes this predicate more reversible than it would be using
 * the ordinary arithmetic plus operator. */

accum(L, A) :-
    accum(L, A, 0).

accum([], [], _).
accum([X|T], [X2|A], Acc) :-
    plus(X, Acc, X2),
    accum(T, A, X2).

/* Compute the forward differences of the list elements. */

forward_difference([], []) :- !.
forward_difference([_], []) :- !.
forward_difference([X, Y | T], [D | T2]) :-
    plus(X, D, Y),
    forward_difference([Y | T], T2).

/* Test whether the list has a zigzag shape. */

zigzag([]).
zigzag([_]).
zigzag([X, Y]) :- X \= Y, !.
zigzag([X, Y, Z | T]) :-
    X < Y, !, Y > Z, !, zigzag([Y, Z | T]).
zigzag([X, Y, Z | T]) :-
    X > Y, Y < Z, zigzag([Y, Z | T]).

/* The classic subset sum problem leads to a branching recursion
 * where both possible rules can be examined to find the solution. */

/* Successful base case where the remaining goal is zero. */
subset_sum(_, [], 0) :- !.

/* Unlike in other languages, we don't need the base case for failure,
 * since the Prolog evaluation will just quietly backtrack to previous
 * choice points anyway in such cases. The first recursive rule
 * examines leaving the first element out of the subset, and the second
 * rule examines taking it in the subset. These sorts of "take it or
 * leave it" local two-way decisions are often used to organize the
 * search in these types of seemingly exponentially branching problems. */
 
subset_sum([_|T], L, G) :- 
    subset_sum(T, L, G).
subset_sum([H|T], [H|L], G) :-
    plus(G2, H, G),
    subset_sum(T, L, G2).

/* Recursive rules for the predicate to zip two lists together
 * use an additional parameter that did not exist in the top-level
 * rule. This is a common technique to turns while-loops into
 * recursion where the local variables of the while-loop become
 * these additional parameters of the predicate. */

my_zip(L1, L2, L) :-
    my_zip(L1, L2, L, left).

/* The base cases unify the result parameter with the correct answer. */ 
my_zip([], L, L, right).
my_zip(L, [], L, left).

/* In the higher cases, the additional state parameter determines which
 * one of the two possible recursive rules gets applied. */

my_zip([X|L1], L2, [X|L3], left) :-
    my_zip(L1, L2, L3, right).
my_zip(L1, [X|L2], [X|L3], left) :-
    my_zip(L1, L2, L3, right).

/* The predicate append is a lot more flexible than most people would
 * think it is, looking at its humble operation. However, with the Prolog
 * pattern matching engine underneath, this operation can be used to
 * iterate through all possible ways to split a list into two parts. */

cyclic_shift([], []).
cyclic_shift([H|L1], L2) :-
    append([H|Left], Right, [H|L1]),
    append(Right, [H|Left], L2).