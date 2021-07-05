/* Prolog solutions for more problems adapted from this
 * author's collection "109 Python Problems for CCPS 109"
 * at https://github.com/ikokkari/PythonProblems */ 

:- use_module(library(clpfd)).

/* Word shape */

word_shape(Word, Shape) :-
    atom_codes(Word, Codes),
    shape_list(Codes, Shape).

shape_list([_], []).

shape_list([C1, C2 | T], [+1 | T2]) :-
    C1 < C2, !,
    shape_list([C2 | T], T2).

shape_list([C1, C2 | T], [-1 | T2]) :-
    C1 > C2, !,
    shape_list([C2 | T], T2).

shape_list([C, C | T], [0 | T2]) :-
    shape_list([C | T], T2).

/* Reverse the vowels */

lower_vowel('a').
lower_vowel('e').
lower_vowel('i').
lower_vowel('o').
lower_vowel('u').

upper_vowel('A').
upper_vowel('E').
upper_vowel('I').
upper_vowel('O').
upper_vowel('U').

is_vowel(C) :-
    lower_vowel(C) ; upper_vowel(C).

to_upper('a', 'A').
to_upper('e', 'E').
to_upper('i', 'I').
to_upper('o', 'O').
to_upper('u', 'U').

to_lower(U, L) :-
    to_upper(L, U).

reverse_vowels(Input, Unpit) :-
    atom_chars(Input, InputC),
    find_vowels(InputC, Vowels),
    reverse(Vowels, Stack),
    process_vowels(InputC, Stack, UnpitC),
    atomics_to_string(UnpitC, Unpit).

find_vowels([], []).
find_vowels([C | T], [C | T2] ) :-
	is_vowel(C),
    !,
	find_vowels(T, T2).
find_vowels([_ | T], T2) :-
    find_vowels(T, T2).

convert(V, C, V) :-
    lower_vowel(V),
    lower_vowel(C),
    !.

convert(V, C, VV) :-
    lower_vowel(V),
    upper_vowel(C),
    to_upper(V, VV),
    !.

convert(V, C, V) :-
    upper_vowel(V),
    upper_vowel(C),
    !.

convert(V, C, VV) :-
    upper_vowel(V),
    lower_vowel(C),
    to_lower(V, VV).

process_vowels([], [], []).
process_vowels([C | T], [V | T2], [VV | T3]) :-
    is_vowel(C),
    !,
    convert(V, C, VV),
    process_vowels(T, T2, T3).
process_vowels([C | T], Stack, [C | T3]) :-
    process_vowels(T, Stack, T3).

/* McCulloch's second machine */

mcculloch(Digits, Result) :-
    atom_chars(Digits, Chars),
    mcculloch_c(Chars, ResultC),
    atomics_to_string(ResultC, Result).

mcculloch_c(['2' | X], X).

mcculloch_c(['3' | X], Z) :-
    mcculloch_c(X, Y),
    append(Y, ['2'], YY),
    append(YY, Y, Z).

mcculloch_c(['4' | X], Z) :-
    mcculloch_c(X, Y),
    mcculloch_c(Y, ZZ),
    reverse(ZZ, Z).

mcculloch_c(['5' | X], Z) :-
    mcculloch_c(X, Y),
    append(Y, Y, Z).

/* Dice chirality */

dice_hand(A, B, C, H) :-
    A in 1..6,
    B in 1..6,
    C in 1..6,
    A + B #\= 7,
    A + C #\= 7,
    B + C #\= 7,
    dice_hand_solve(A, B, C, H).

other_hand(left, right).
other_hand(right, left).
    
dice_hand_solve(1, 2, 3, left).
dice_hand_solve(2, 3, 1, left).
dice_hand_solve(3, 1, 2, left).
dice_hand_solve(1, 3, 2, right).
dice_hand_solve(3, 2, 1, right).
dice_hand_solve(2, 1, 3, right).

dice_hand_solve(A, B, C, H) :-
    A in 4..6,
    A + A2 #= 7,
    other_hand(H, H2),
    dice_hand_solve(A2, B, C, H2).

dice_hand_solve(A, B, C, H) :-
    A in 1..3,
    B in 4..6,
    B + B2 #= 7,
    other_hand(H, H2),
    dice_hand_solve(A, B2, C, H2).

dice_hand_solve(A, B, C, H) :-
    A in 1..3,
    B in 1..3,
    C in 4..6,
    C + C2 #= 7,
    other_hand(H, H2),
    dice_hand_solve(A, B, C2, H2).

/* Postfix evaluate */

/* Step 1: Recursive call to version with local variables
 * as extra parameters. */

postfix_evaluate(Expr, Result) :-
    postfix_evaluate(Expr, Result, []).

/* Step 2: Base case unifies the result variable. */

postfix_evaluate([], Result, [Result]).

/* Step 3: Given the state of variables before body of the
 * simulated while-loop, compute the state of variables
 * after executing that body for the tail recursive call. */

postfix_evaluate([C | T], Result, Stack) :-
    integer(C),
    postfix_evaluate(T, Result, [C | Stack]).

postfix_evaluate(['+' | T], Result, [V1, V2 | Stack]) :-
    V #= V1 + V2,
    postfix_evaluate(T, Result, [V | Stack]).

postfix_evaluate(['-' | T], Result, [V1, V2 | Stack]) :-
    V #= V2 - V1,
    postfix_evaluate(T, Result, [V | Stack]).

postfix_evaluate(['*' | T], Result, [V1, V2 | Stack]) :-
    V #= V1 * V2,
    postfix_evaluate(T, Result, [V | Stack]).

postfix_evaluate(['/' | T], Result, [V1, V2 | Stack]) :-
    V1 #\= 0,
    V #= V2 div V1,
    postfix_evaluate(T, Result, [V | Stack]).

postfix_evaluate(['/' | T], Result, [V1, _ | Stack]) :-
    V1 #= 0,
    postfix_evaluate(T, Result, [0 | Stack]).

/* Lattice paths */

:- dynamic p/3.
:- dynamic tabu/1.

lattice_paths(N, M, Tabu, R) :-
    assert(tabu(Tabu)),
    paths(N, M, R),
    retractall(p),
    retract(tabu(Tabu)).
	
paths(0, 0, 1).

paths(N, M, 0) :-
    ( N < 0 ; M < 0 ),
    !.

paths(N, M, R) :-
    p(N, M, R),
    !.

paths(N, M, 0) :-
	tabu(Tabu),
    member((N, M), Tabu),
    !.

paths(N, M, R) :-
    plus(N1, 1, N),
    plus(M1, 1, M),
    paths(N1, M, P1),
    paths(N, M1, P2),
    R is P1 + P2,
    assert(p(N, M, R)).