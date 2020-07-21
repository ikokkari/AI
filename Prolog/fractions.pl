/* A helper predicate for computing the sign of an integer. */

sign(0, 0).
sign(X, -1) :- X < 0.
sign(X, 1) :- X > 0.

/* The recursive rules to simplify an integer fraction. One rule
 * turns an integer N into N/1, and four other rules simplify
 * formulas depending on the arithmetic operator in the root node
 * of the expression tree. */

simp(N, N/1) :- integer(N).

simp(N1/D1, N/D) :-
    integer(N1),
    integer(D1), 
    !,
    sign(D1, S),
    G is gcd(N1, D1), /* gcd is SWI-Prolog built-in */
    N is S*N1/G,
    D is S*D1/G.

simp(T1 + T2, NR/DR) :- 
    simp(T1, N1/D1),
    simp(T2, N2/D2),
    N is N1*D2 + N2*D1,
    D is D1 * D2,
    simp(N/D, NR/DR).

simp(T1 - T2, NR/DR) :- 
    simp(T1, N1/D1),
    simp(T2, N2/D2),
    N is N1*D2 - N2*D1,
    D is D1 * D2,
    simp(N/D, NR/DR).
 
simp(T1 * T2, NR/DR) :-
    simp(T1, N1/D1),
    simp(T2, N2/D2),
    N is N1 * N2,
    D is D1 * D2,
    simp(N/D, NR/DR).

simp(T1 / T2, NR/DR) :-
    simp(T1, N1/D1),
    simp(T2, N2/D2),
    N is N1 * D2,
    D is D1 * N2,
    simp(N/D, NR/DR).

/* With the engine for simplifying integer fractions available, we can
 * solve a famous problem that turns out to be surprisingly tricky to
 * solve with imperative languages. However, the pattern matching engine
 * combined with backtracking search makes Prolog shine with this problem.
 * First, some utility predicates to build up possible terms from the
 * given list literals. */

binterm(E1, E2, (E1)+(E2)).
binterm(E1, E2, (E1)-(E2)).
binterm(E1, E2, (E1)*(E2)).
binterm(E1, E2, (E1)/(E2)) :-
    not(simp(E2, 0/_)).

make_expr([X], X) :- !.
make_expr(L, E) :-
    append([X|Left], [Y|Right], L),
    make_expr([X|Left], E1),
    make_expr([Y|Right], E2),
    binterm(E1, E2, E).

sublist([], _).
sublist([X|L], [X|L2]) :- sublist(L, L2).
sublist([X|L], [_|L2]) :- sublist([X|L], L2).

/* Given a Goal and a list of numbers, use those numbers to build
 * an expression that evaluates to Goal. The predicates sublist,
 * permutation and make_expr are used to generate all expressions
 * that can be built from numbers in L, and the predicate simp is
 * used to test whether the expression simplifies to Goal. */ 

solve(L, E, Goal) :-
    simp(Goal, G),
    /* generate */
    sublist(Ls, L),
    permutation(Ls, L2),
    make_expr(L2, E),
    /* test */ 
    simp(E, G).
