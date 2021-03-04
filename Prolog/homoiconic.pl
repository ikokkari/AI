:- use_module(library(clpfd)).

/* Some simple predicates to use in example queries. */

sq(N, N2) :-
    N2 #= N * N.

even(N) :- 
    N mod 2 #= 0.

my_times(X, Y, Z) :-
    X * Y #= Z.

/* Prolog is a homoiconic language: the language itself talks about
 * expressions of that language. Let's see what this entails. */

/* Some predicates to simulate the behaviour of classic functional
 * programming operations inside Prolog. The metapredicate call
 * allows us to evaluate Prolog terms as Prolog queries. */

/* my_filter(even, [42, 17, 99, 0], [42, 0]). */

my_filter(_, [], []) :- !.

my_filter(P, [H|T], [H|T2]) :-
    call(P, H),
    !,
    my_filter(P, T, T2).

my_filter(P, [_|T], T2) :-
    my_filter(P, T, T2).

/* my_map(sq, [1, 2, 3, 4], [1, 4, 9, 16]). */

my_map(_, [], []) :- !.

my_map(P, [H|T], [H2|T2]) :-
    call(P, H, H2),
    my_map(P, T, T2).

/* my_take_while(even, [17, 99, -1, 0, 15, 99], [17, 19, -1]). */

my_take_while(_, [], []) :- !.

my_take_while(P, [H|T], [H|T2]) :-
    call(P, H),
    !,
    my_take_while(P, T, T2).

my_take_while(_, [_|_], []). /* Compare to rule in my_filter */

/* my_fold(0, plus, [1, 2, 3, 4], 10). */

my_fold(X, _, [], X).

my_fold(X, P, [H|T], Y) :-
    call(P, X, H, Z),
    my_fold(Z, P, T, Y).

/* Term rewriting in style of Replace in Wolfram Mathematica. */

/* Prolog operator == checks for strict term equality without unify. */

rewrite(T, X, Y, Y) :-
    T == X,
    !.

/* Regardless of substitution, unbound variables remain as they were. */

rewrite(T, _, _, T) :-
    var(T),
    !.

/* Use the ..= ("univ") operator to split the term into list of subterms,
 * recursively rewrite these subterms, and combine the results into the
 * final answer. */

rewrite(T, X, Y, R) :-
    T =.. [F|Args],
    rewrite_list(Args, X, Y, Args2),
    R =.. [F|Args2].

/* The logic for rewriting a list of terms follows the usual recursion
 * for processing a list one element at the time. We should perhaps use
 * tail recursion in this one in case somebody tries to rewrite a million
 * node binary search tree... */

rewrite_list([] , _, _, []).
rewrite_list([H|T], X, Y, [H2|T2]) :-
    rewrite(H, X, Y, H2),
    rewrite_list(T, X, Y, T2).

/* This predicate will be used in automated testers in the Prolog labs.
 * 
 * Query	The query to test for correctness.
 * X		The expression to solve the query for.
 * Inf		The number of inferences needed to solve the query.
 * Res		List of all results found for the Query.
 * Test		Acceptance query for Result as passing criterion.    
*/

test(Query, X, Inf, Res, Test) :-
    /* SWI-Prolog built-in predicate */
    statistics(inferences, I1),
    /* Find all solutions in one swoop */
    call(findall(X, Query, Res)),
    statistics(inferences, I2),
    Inf is I2 - I1,
    call(Test) -> /* shorthand for if-then-else */
    	(write('success'), nl, !) ;
    	(write('failure'), nl, fail).
test(_, _, 0, _, _).