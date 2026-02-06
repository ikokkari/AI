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
 
/* === CONSTRAINT PROPAGATION DEMO === */

% Triangle inequality with constraint propagation
triangle(X, Y, Z) :-
    [X, Y, Z] ins 1..100,  % Domain declaration
    X + Y #> Z,
    X + Z #> Y,
    Y + Z #> X.

% Strictly ascending list (reversible!)
is_ascending([]).
is_ascending([_]).
is_ascending([N1, N2 | T]) :-
    N1 #< N2,
    is_ascending([N2 | T]).

/* === REVERSIBLE ARITHMETIC === */

% Length (works both directions!)
my_length([], 0).
my_length([_|T], N) :-
    N #= N1 + 1,  % Constraint, not computation
    my_length(T, N1).

% Factorial (reversible for small values)
n_factorial(0, 1).
n_factorial(N, F) :-
    N #> 0,
    N1 #= N - 1,
    F #= N * F1,  % Constraint posted before recursion
    n_factorial(N1, F1).

/* === PRIME FACTORIZATION === */

factors(N, F) :-
    N #> 0,
    factors(N, F, 2).

factors(1, [], _).
factors(N, [N], First) :-
    N #> 1,
    First #> N.  % N is prime
factors(N, [A|F], First) :-
    N #> 1,
    smallest_factor(N, A, First),
    B #= N // A,
    factors(B, F, A).

% Find smallest factor >= First
smallest_factor(N, A, First) :-
    A #>= First,
    A * A #=< N,
    N mod A #= 0,
    indomain(A),
    !.

/* === BULLS (MASTERMIND) === */

% Count matching positions in two digit lists
bulls(L1, L2, B) :-
    same_length(L1, L2),
    L1 ins 1..9,
    L2 ins 1..9,
    all_distinct(L1),
    all_distinct(L2),
    bulls_count(L1, L2, B).

bulls_count([], [], 0).
bulls_count([D|T1], [D|T2], B) :-
    bulls_count(T1, T2, B1),
    B #= B1 + 1.
bulls_count([D1|T1], [D2|T2], B) :-
    dif(D1, D2),
    bulls_count(T1, T2, B).

/* === COOKIE MONSTER === */

% eat(Piles, N, Result) - eat N cookies from piles
% If pile < N: unchanged
% If pile = N: removed
% If pile > N: reduced by N
eat([], _, []).
eat([H|T], N, [H|T2]) :-
    H #< N,
    eat(T, N, T2).
eat([H|T], N, T2) :-
    H #= N,
    eat(T, N, T2).
eat([H|T], N, [H2|T2]) :-
    H #> N,
    H2 #= H - N,
    eat(T, N, T2).
