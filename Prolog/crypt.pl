/* Cryptarithmetic solvers (as in SEND + MORE = MONEY) are the classic
 * example of constraint satisfaction search in Prolog. However, this
 * particular example could (no surprise there) be written much nicer
 * with the constraint logic programming extension. The global constraint
 * of every letter getting a unique value needs a lot more work under
 * ordinary Prolog. */

/* Ensure that all elements of list are strictly different. */

strict_neq([]).
strict_neq([H|T]) :-
    strict_nmbr(H, T),
    strict_neq(T).

/* Checks that X is not identical to any value in the list. */

strict_nmbr(_, []).
strict_nmbr(X, [H|T]) :-
    X \== H,
    strict_nmbr(X, T).

/* Checks that X occurs in the list just once. */

just_once(_, []).
just_once(X, [H|T]) :-
    X == H, !,
    strict_nmbr(X, T).
just_once(X, [_|T]) :- just_once(X, T).
 
/* Collects to Vars all free variables in lists L1, L2 and L3. */

vars(L1, L2, L3, Vars) :-
    append(L1, L2, LL),
    append(LL, L3, L),
    setof(X, (member(X, L), var(X)), Vars).

/* Solve a cryptarithmetic problem. */

solve(First, Second, Sum) :-
    reverse(First, FirstR),
    reverse(Second, SecondR),
    reverse(Sum, SumR),
    vars(First, Second, Sum, Vars),
    solve(FirstR, SecondR, SumR, 0, Vars),
    strict_neq(Vars).

/* Base cases of the recursion. */

solve([], L, L, 0, _) :- !.
solve(L, [], L, 0, _) :- !.
solve([], [], [1], 1, _) :- !.
solve([], Second, Sum, 1, Vars) :- solve([1], Second, Sum, 0, Vars).
solve(First, [], Sum, 1, Vars) :- solve(First, [1], Sum, 0, Vars).

/* Solve with first digits H1, H2, H3 and carry C. */

solve([H1|T1], [H2|T2], [H3|T3], C, Vars) :-
    between(0, 9, H1),
    just_once(H1, Vars),
    between(0, 9, H2),
    just_once(H2, Vars),
    plus(H1, H2, S1),
    plus(S1, C, S),
    add(S, H3, T1, T2, T3, Vars).

/* Digits and carry add up to a single digit. */

add(H3, H3, T1, T2, T3, Vars) :-
    H3 < 10, /* single-digit number */
    just_once(H3, Vars),
    solve(T1, T2, T3, 0, Vars).

/* Digits and carry add up to two-digit number with carry. */

add(S, H3, T1, T2, T3, Vars) :-
    S > 9,
    plus(H3, 10, S),
    just_once(H3, Vars),
    solve(T1, T2, T3, 1, Vars).
