/* === PREFIX, SUFFIX, SUBLIST === */
prefix(L1, L2) :- append(L1, _, L2).
suffix(L1, L2) :- append(_, L1, L2).

% sublist(Sub, List) - Sub is contiguous subsequence
sublist([H|L1], L2) :- prefix([H|L1], L2).
sublist(L1, [_|L2]) :- sublist(L1, L2).

/* === ASCENDING === */
is_ascending([]).
is_ascending([_]).
is_ascending([X, Y | T]) :-
    X < Y,
    is_ascending([Y|T]).

/* === ACCUMULATION (Running Sum) === */
accum(L, A) :- accum(L, A, 0).
accum([], [], _).
accum([X|T], [X2|A], Acc) :-
    plus(X, Acc, X2),
    accum(T, A, X2).

/* === FORWARD DIFFERENCES === */
differences([], []).
differences([_], []).
differences([X, Y | T], [D | T2]) :-
    plus(X, D, Y),
    differences([Y | T], T2).

/* === ZIGZAG PATTERN === */
zigzag([]).
zigzag([_]).
zigzag([X, Y]) :- X \= Y.
zigzag([X, Y, Z | T]) :-
    peak_or_valley(X, Y, Z),
    zigzag([Y, Z | T]).

peak_or_valley(X, Y, Z) :- X < Y, Y > Z.  % Y is peak
peak_or_valley(X, Y, Z) :- X > Y, Y < Z.  % Y is valley

/* === SUBSET SUM === */
subset_sum(_, [], 0).
subset_sum([_|T], L, G) :- 
    subset_sum(T, L, G).  % Exclude first element
subset_sum([H|T], [H|L], G) :-
    plus(G2, H, G),
    subset_sum(T, L, G2).  % Include first element

/* === INTERLEAVE (simpler than my_zip) === */
interleave([], L, L).
interleave([X|Xs], Ys, [X|Zs]) :-
    interleave(Ys, Xs, Zs).

/* === CYCLIC SHIFT (all rotations) === */
cyclic_shift([], []).
cyclic_shift([H|L1], L2) :-
    append([H|Left], Right, [H|L1]),
    append(Right, [H|Left], L2).
