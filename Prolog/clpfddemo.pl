:- use_module(library(clpfd)).

/* In ordinary Prolog, each variable is either unbound, or bound to
 * some expression. A bound variable cannot be "reassigned", but can
 * become unbound only in backtracking. The clpfd extension allows
 * some variables to be internally marked as having integer domains.
 * Instead of being merely bound or unbound, each integer variable
 * carries a set of equality and inequality constraints. If these
 * constraints become contradictory, such as X #< 3 /\ X #> 10 for
 * some integer variable X, the execution fails and backtracks to
 * the most recent choice point.
 * 
 * Once you get accustomed to this mechanism, you can just use it
 * as routine and forget that it even existed. Predicates written in
 * the CLP formalism silently work just like ordinary predicates
 * except better, in that they terminate with solutions for some
 * queries that would make ordinary predicates crash or fall into
 * an infinite loop.
 * 
 * Integer variables with arbitrary constraints can always be turned
 * back into ordinary Prolog variables with predicates indomain/1 and
 * labeling/2. These predicates bind one variable or a list of
 * variables to the values in their domain one value at the time.
 */

/* Determine if (X, Y, Z) could be side lengths of a triangle. */

triangle(X, Y, Z) :-
    X #> 0,
    Y #> 0,
    Z #> 0,
    X + Y #> Z,
    X + Z #> Y,
    Y + Z #> X.

/* Determine whether the parameter list is strictly ascending. */

is_ascending([]).
is_ascending([_]).
is_ascending([N1, N2 | T]) :-
    N1 #< N2,
    is_ascending([N2 | T]).

/* Use of lazy constraints allows the my_length predicate to be
 * tail recursive out of the box. */

my_length([], 0).
my_length([_|T], N) :-
    N #= N1 + 1,
    my_length(T, N1).

/* Factorials. */

n_factorial(0, 1).

n_factorial(N, F) :-
        N #> 0,
        N1 #= N - 1,
        F #= N * F1,
        n_factorial(N1, F1).

/* List of prime factors of the given integer N. */

factors(N, F) :-
    factors(N, F, 2).

factors(1, [], _).

factors(N, [N], First) :-
    N #> 1,
    First #> N.

factors(N, [A|F], First) :-
    N #> 1,
    two_factors(N, A, B, First),
    factors(B, F, A).

two_factors(N, A, B) :-
    two_factors(N, A, B, 1).

two_factors(N, A, B, First) :-    
    A #>= First,     
    B #>= A,
    A*A #=< N,
    B #< N,
    A * B #= N,
    indomain(A),
    /* Eliminate the cut to produce all two-factor breakdowns. */
    !. 

two_factors(N, N, 1, _).

/* Given two lists consisting of digits 1 to 9, determine how many items
 * both lists have in common, as used in the game of "Bulls and Cows". */

/* Top level call brings in the state parameters. */
bulls(L1, L2, B) :-
    bulls(L1, L2, B, 0),
    all_distinct(L1),
    all_distinct(L2).

/* Base case unifies result with final state. */
bulls([], [], B, B).

/* General case for two matching digits. */
bulls([D|T1], [D|T2], B, N) :-
    D in 1..9,
    N2 #= N + 1,
    bulls(T1, T2, B, N2).

/* General case for non-matching position. */
bulls([D1|T1], [D2|T2], B, N) :-
    dif(D1, D2),
	[D1, D2] ins 1..9,
    bulls(T1, T2, B, N).

/* Individual eating step of the Cookie Monster problem. */

eat([], _, []).
eat([H|T], N, [H|T2]) :-
    0 #< H,
    H #< N,
    eat(T, N, T2).
eat([H|T], N, T2) :-
    H #= N,
    eat(T, N, T2).
eat([H|T], N, [H2|T2]) :-
    H #> N,
    H2 #= H - N,
    eat(T, N, T2).

/* To learn more about CLP(FD) and its use in discrete optimization,
 * you should check out the CLP(FD) Tutorial by Anne Ogborn, available
 * at http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html */