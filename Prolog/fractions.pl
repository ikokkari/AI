/* A helper predicate for computing the sign of an integer. */

sign(0, 0).
sign(X, -1) :- 
    X < 0,
    !.
sign(X, 1) :- 
    X > 0.

/* The recursive rules to simplify an integer fraction. The base
 * cases of simplification are when everything is an integer. */

simp(N, N/1) :- 
    integer(N),
    !.

simp(N1 / D1, N / D) :-
    integer(N1),
    integer(D1), 
    !,
    sign(D1, S),
    G is gcd(N1, D1), /* gcd is SWI-Prolog built-in */
    N is S*N1/G,
    D is S*D1/G.

/* Each arithmetic operator gets its own rule to simplify it. */

simp(T1 + T2, NR / DR) :- 
    simp(T1, N1 / D1),
    simp(T2, N2 / D2),
    N is N1*D2 + N2*D1,
    D is D1 * D2,
    simp(N / D, NR / DR).

simp(T1 - T2, NR / DR) :- 
    simp(T1, N1 / D1),
    simp(T2, N2 / D2),
    N is N1*D2 - N2*D1,
    D is D1 * D2,
    simp(N / D, NR / DR).
 
simp(T1 * T2, NR / DR) :-
    simp(T1, N1 / D1),
    simp(T2, N2 / D2),
    N is N1 * N2,
    D is D1 * D2,
    simp(N / D, NR / DR).

simp(T1 / T2, NR / DR) :-
    simp(T1, N1 / D1),
    simp(T2, N2 / D2),
    N is N1 * D2,
    D is D1 * N2,
    simp(N / D, NR / DR).

/* With predicates for simplifying integer fractions available, we can
 * solve a famous problem that turns out to be surprisingly tricky to
 * solve with imperative languages. However, the pattern matching engine
 * combined with backtracking search makes Prolog shine with this problem.
 * First, some utility predicates to build up possible terms from the
 * given elements. */

binterm(E1, E2, (E1)+(E2)).
binterm(E1, E2, (E1)-(E2)).
binterm(E1, E2, (E1)*(E2)).
/* Division E1/E2 requires that E2 does not simplify to zero. */
binterm(E1, E2, (E1)/(E2)) :-
    not(simp(E2, 0/_)).

/* All possible ways to make an arithmetic expression out of the
 * elements of the given list. */

make_expr([X], X) :- !.

make_expr(L, E) :-
    append([X|Left], [Y|Right], L),
    make_expr([X|Left], E1),
    make_expr([Y|Right], E2),
    binterm(E1, E2, E).

/* All possible ways to extract a subsequence of given list. */

subseq([], []).
subseq([], [_|_]).
subseq([X|L], [X|L2]) :- subseq(L, L2).
subseq([X|L], [_|L2]) :- subseq([X|L], L2).

/* Given a Goal and a list L of numbers, use those numbers to build
 * an expression E that evaluates to Goal. The predicates subseq,
 * permutation and make_expr are used to generate all expressions
 * that can be built from numbers in L, and the predicate simp is
 * used to test whether the expression simplifies to Goal. */ 

solve(L, E, Goal) :-
    simp(Goal, G),
    /* generate */
    subseq(Ls, L),
    permutation(Ls, L2),
    make_expr(L2, E),
    /* test */ 
    simp(E, G).

/* There is no finesse in this: the brute force search generates
 * all possible expressions that can be constructed from elements
 * of the list L, and evaluates each expression to see if maybe it
 * happens to evaluate to Goal. Hey, we could get lucky. */