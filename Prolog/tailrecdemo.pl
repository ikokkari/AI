/* The plain recursive version. Since each non-tail recursive call
 * grows the stack, recursions for deep lists would crash with a
 * stack overflow. */

maxlist_rec([X], X).
maxlist_rec([H|T], X) :-
    maxlist_rec(T, M),
    X is max(H, M).

/* The tail-recursive accumulator version illustrates the general pattern.
 * The rule for the top-level call converts the local variables of the
 * implicit while-loop solution into additional parameter MaxSoFar in the
 * recursive rule below. */

maxlist_acc([H|T], X) :- 
    maxlist_acc(T, X, H).

/* The rule for base case unifies the result X, this far usually kept
 * unbound, with the correct result that we read from other variables.
 * This base case rule matches only the situation where the condition
 * of the imagined while-loop would be false; in this problem, when
 * the remaining list from which we look for elements is empty. */

maxlist_acc([], X, X).

/* The general recursive rule simulates going to the next round of the
 * while-loop with the new values for local variables. */

maxlist_acc([H|T], X, MaxSoFar) :-
    M2 is max(H, MaxSoFar),
    maxlist_acc(T, X, M2).

/* The naive recursion to reverse a list takes quadratic time by
 * appending each element separately to the end of the recursively
 * computed reversal of the tail of the list. */

my_rev([], []).
my_rev([H|T], R) :-
    my_rev(T, R2),
    append(R2, [H], R).

/* Accumulator and tail recursion build up the reversed list into
 * a separate parameter, allowing constant time update per element.
 * Not only does accumulator eliminate the need for stack, but
 * turns a "Shlemiel" quadratic time algorithm into a linear one. */

/* The rule for top-level call again adds a new parameter to the call,
 * used to keep track of the local variable of the implicit while-loop. */

my_rev_acc(L, R) :- 
    my_rev_acc(L, R, []).

/* The rule for the base case unifies the result with the correct answer. */

my_rev_acc([], R, R).

/* The rule for the general case keeps the result unbound (or bound to
 * whatever it happens to be bound in the query) and updates the values
 * of additional parameters to correspond to the values of these local
 * variables of the simulated while-loop. */

my_rev_acc([H|T], R, Acc) :-
    my_rev_acc(T, R, [H|Acc]).

/* Just for fun, let us measure the logical inferences needed to reverse
 * list each way. The results show how much it pays not to be a Shlemiel. */

measure_reverse(N, I, J) :-
    /* Trick to create a list of N different elements */
    findall(X, between(1, N, X), L),
    /* One of SWI-Prolog introspective predicates */
    statistics(inferences, I1),
    my_rev(L, _), /* Shlemiel O(n^2) */
    statistics(inferences, I2),
    I is I2 - I1,
    statistics(inferences, I3),
    my_rev_acc(L, _), /* Linear O(n) */
    statistics(inferences, I4),
    J is I4 - I3.

/* Count how many times X occurs in the list L.
 * First, the recursive solution. */

count_rec(_, [], 0) :-
    !.

count_rec(X, [X|L], Result) :-
    !,
    count_rec(X, L, Result2),
    plus(Result2, 1, Result).

/* More efficient tail recursive solution. */

count_rec(X, [_|L], Result) :-
    count_rec(X, L, Result).

count_acc(X, L, Result) :-
    count_acc(X, L, 0, Result).

count_acc(_, [], Result, Result) :-
    !.

count_acc(X, [X|L], Tally, Result) :-
    !,
    plus(Tally, 1, Tally2),
    count_acc(X, L, Tally2, Result).

count_acc(X, [_|L], Tally, Result) :-
    count_acc(X, L, Tally, Result).


/* Tail recursion can be applied to problems of arithmetic just as well
 * as to problems of lists. Falling power is a version of integer power
 * that is more suitable in some combinatorial computations. */

falling_power(_, 0, 1) :- !.
falling_power(X, N, P) :-
    N > 0,
    X2 is X - 1,
    N2 is N - 1,
    falling_power(X2, N2, P2),
    P is X * P2.

/* The tail recursive version is again showcase of the general pattern.
 * Perhaps the reader could fill in the comments at this point. */

/* In the top-level call, ... */

falling_power_acc(X, N, P) :-
    falling_power_acc(X, N, P, 1).

/* The rule for the base case ... */

falling_power_acc(_, 0, P, P) :- !.

/* The general case updates the ... */

falling_power_acc(X, N, P, SoFar) :-
    N > 0,
    X2 is X - 1,
    N2 is N - 1,
    SoFar2 is SoFar * X,
    falling_power_acc(X2, N2, P, SoFar2).

/* Primality testing by trial division is a classic example of a loop. */

/* The simple cases for small N. */
is_prime(N) :- N < 2, !, fail.
is_prime(2) :- !.
is_prime(N) :- N mod 2 =:= 0, !, fail.

/* Otherwise, here comes the top-level call... */

is_prime(X) :- is_prime(X, 3).

/* The additional accumulator parameter D keeps track of which divisor
 * we are currently investigating. Every composite integer N must have
 * a divisor that is at most equal to the square root of N, so we can
 * stop looking if one has not been found until then. */

is_prime(X, D) :-
    D2 is D * D,
    D2 > X,
    !.

/* Otherwise, continued success required that D not divide N. Note
 * again how the case for failure is not needed, since Prolog evaluation
 * fails by itself anyway when no rules match the query. */

is_prime(X, D) :-
    X mod D > 0,
    D2 is D + 2,
    is_prime(X, D2).

/* The famously bouncy and chaotic Collatz 3n+1 series. The use of an
 * accumulator again avoids the stack overflow from deep recursions. */

/* Top-level call adds in the parameters that correspond to the local
 * variables of the implicit while-loop simulated with tail recursion. */

collatz(N, S) :-
    collatz(N, S, []).

/* The base case rule unifies the result with the correct value. */

collatz(1, S, S2) :-
    reverse([1|S2], S), !.

/* One of two recursive rules applies depending on the parity of N. */

collatz(N, S, SoFar) :-
    N > 1,
    0 =:= N mod 2,
    !,
    N2 is div(N, 2), /* Prolog integer division operator */
    collatz(N2, S, [N|SoFar]).

collatz(N, S, SoFar) :-
    N > 1,
    1 =:= N mod 2,
    !,
    N2 is 3 * N + 1,
    collatz(N2, S, [N|SoFar]).

/* Every computer science course must use Fibonacci numbers somewhere.
 * Plain recursive version is exponentially slow in every language. */

fib_rec(0, 1) :- !.
fib_rec(1, 1) :- !.
fib_rec(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib_rec(N1, F1),
    fib_rec(N2, F2),
    F is F1 + F2.

/* The accumulator version remembers the two previous Fibonacci numbers
 * F2 and F1 before the current Fibonacci number F to be computed. */

fib_acc(0, 1) :- !.
fib_acc(1, 1) :- !.

fib_acc(N, F) :-
    fib_acc(N, F, 1, 1).

/* The base case rule unifies result with the previous Fibonacci number. */

fib_acc(1, F, _, F) :- !.

/* The general case shifts the sliding window from (F2, F1) to (F1, F2+F1). */

fib_acc(N, F, F2, F1) :-
    N > 0,
    FF is F2 + F1,
    N2 is N - 1,
    fib_acc(N2, F, F1, FF).

/* Express a fraction A/B as sum of distinct unit fractions
 * using the greedy Egyptian fractions method. */

greedy_egyptian(F, Result) :-
    greedy_egyptian(F, [], Result2),
	reverse(Result2, Result).

/* Base case of tail recursion is when fraction is unit fraction. */

greedy_egyptian(1/N, Units, [1/N | Units]) :- !.

/* Extract the largest unit fraction that fits in A/B, and continue
 * converting the remaining fraction into unit fractions. */

greedy_egyptian(A/B, Units, Result) :-
    N is div(B, A) + 1,
    Num is N*A - B,
    Den is N*B,
    G is gcd(Num, Den),
    Num2 is div(Num, G),
    Den2 is div(Den, G),
    greedy_egyptian(Num2/Den2, [1/N | Units], Result).



/* Merge sort is the most straightforward O(n log n) sorting algorithm. */

/* Split the list into two lists alternating the elements. */

split([], [], []) :- !.
split([X], [X], []) :- !.
split([X, Y | T], [X | T1], [Y | T2]) :-
    split(T, T1, T2).

/* Merge two sorted lists into a single sorted list. */

merge([], [], []) :- !.
merge([H | T], [], [H | T]) :- !.
merge([], [H | T], [H | T]) :- !.

/* Comparing the first elements decides which one gets in. */

merge([H1 | T1], [H2 | T2], [H1 | T]) :-
    H1 < H2, !,
    merge(T1, [H2 | T2], T).
merge([H1 | T1], [H2 | T2], [H2 | T]) :-
    merge([H1 | T1], T2, T).

/* With these preliminaries, the actual merge sort algorithm is
 * just a couple of lines in any language. */

mergesort([], []) :- !.
mergesort([X] , [X] ) :- !.

mergesort([H1, H2 | T], S) :-
    split([H1, H2 | T], L1, L2),
    mergesort(L1, S1),
    mergesort(L2, S2),
    merge(S1, S2, S).
