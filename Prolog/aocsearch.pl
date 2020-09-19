/* State space from https://adventofcode.com/2016/day/13 */

bit_parity(N, R) :-
    bit_parity(N, R, 0).

bit_parity(0, R, R) :- !.

bit_parity(N, R, P) :-
    N > 0,
    M is N mod 2,
    N2 is N div 2,
    P2 is (M + P) mod 2,
    bit_parity(N2, R, P2).

is_open(X, Y) :-
    X >= 0,
    Y >= 0,
    N is X*X + 3*X + 2*X*Y + Y + Y*Y + 42,
    bit_parity(N, 0).

step((X1, Y), (X2, Y)) :-
    (   plus(X1, 1, X2) ; plus(X2, 1, X1) ),
    is_open(X2, Y).

step((X, Y1), (X, Y2)) :-
    (   plus(Y1, 1, Y2) ; plus(Y2, 1, Y1) ),
    is_open(X, Y2).
 
/* Depth-limited depth-first search. */
             
dldfs(Start, Goal, Sol, Depth) :- 
    dldfs(Start, Goal, Sol, [], Depth).

dldfs(Goal, Goal, [Goal], _, _) :-
    !.

dldfs(Start, Goal, [Start|Sol], Visited, Depth) :-
    Depth > 0,
    NewDepth is Depth - 1,
    step(Start, Next),
    not(member(Next, [Start|Visited])),
    dldfs(Next, Goal, Sol, [Start|Visited], NewDepth).

/* Iterative deepening DFS. */

idfs(Start, Goal, Sol, GiveUp) :- 
    idfs(Start, Goal, Sol, 1, GiveUp).

idfs(Start, Goal, Sol, Depth, _) :-
    dldfs(Start, Goal, Sol, [], Depth),
    !.

idfs(Start, Goal, Sol, Depth, GiveUp) :-
    Depth < GiveUp,
    NewDepth is Depth + 1,
    write('Setting upper bound to '), write(NewDepth), nl,
    idfs(Start, Goal, Sol, NewDepth, GiveUp).
    
