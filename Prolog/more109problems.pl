/* Prolog solutions for more problems adapted from this
 * author's collection "109 Python Problems for CCPS 109"
 * at https://github.com/ikokkari/PythonProblems */ 

:- use_module(library(clpfd)).

/* === WORD SHAPE === */
word_shape(Word, Shape) :-
    atom_codes(Word, Codes),
    shape_list(Codes, Shape).

shape_list([_], []).
shape_list([C1, C2 | T], [Dir | T2]) :-
    compare(Order, C1, C2),
    direction(Order, Dir),
    shape_list([C2 | T], T2).

direction(<, +1).
direction(>, -1).
direction(=, 0).

/* === REVERSE VOWELS === */
is_vowel(C) :- 
    member(C, [a,e,i,o,u,'A','E','I','O','U']).

reverse_vowels(Input, Output) :-
    atom_chars(Input, Chars),
    findall(V, (member(V, Chars), is_vowel(V)), Vowels),
    reverse(Vowels, ReversedVowels),
    replace_vowels(Chars, ReversedVowels, OutputChars),
    atom_chars(Output, OutputChars).

replace_vowels([], [], []).
replace_vowels([C|Cs], [V|Vs], [V2|Result]) :-
    is_vowel(C), !,
    match_case(V, C, V2),
    replace_vowels(Cs, Vs, Result).
replace_vowels([C|Cs], Vs, [C|Result]) :-
    replace_vowels(Cs, Vs, Result).

match_case(V, Template, Result) :-
    (   char_type(Template, lower)
    ->  char_type(Result, to_lower(V))
    ;   char_type(Result, to_upper(V))
    ).

/* === MCCULLOCH'S MACHINE === */
mcculloch(Digits, Result) :-
    atom_chars(Digits, Chars),
    mcculloch_c(Chars, ResultC),
    atom_chars(Result, ResultC).

mcculloch_c(['2' | X], X).
mcculloch_c(['3' | X], Z) :-
    mcculloch_c(X, Y),
    append(Y, ['2'|Y], Z).
mcculloch_c(['4' | X], Z) :-
    mcculloch_c(X, Y),
    mcculloch_c(Y, YY),
    reverse(YY, Z).
mcculloch_c(['5' | X], Z) :-
    mcculloch_c(X, Y),
    append(Y, Y, Z).

/* === DICE CHIRALITY === */
dice_hand(A, B, C, H) :-
    [A, B, C] ins 1..6,
    A + B #\= 7,
    A + C #\= 7,
    B + C #\= 7,
    dice_hand_solve(A, B, C, H).

other_hand(left, right).
other_hand(right, left).

% Base cases: all permutations of 1,2,3
dice_hand_solve(1, 2, 3, left).
dice_hand_solve(2, 3, 1, left).
dice_hand_solve(3, 1, 2, left).
dice_hand_solve(1, 3, 2, right).
dice_hand_solve(3, 2, 1, right).
dice_hand_solve(2, 1, 3, right).

% Recursive: flip faces 4-6 to 1-3 and swap handedness
dice_hand_solve(A, B, C, H) :-
    A in 4..6,
    A2 #= 7 - A,
    other_hand(H, H2),
    dice_hand_solve(A2, B, C, H2).
dice_hand_solve(A, B, C, H) :-
    A in 1..3, B in 4..6,
    B2 #= 7 - B,
    other_hand(H, H2),
    dice_hand_solve(A, B2, C, H2).
dice_hand_solve(A, B, C, H) :-
    A in 1..3, B in 1..3, C in 4..6,
    C2 #= 7 - C,
    other_hand(H, H2),
    dice_hand_solve(A, B, C2, H2).

/* === POSTFIX EVALUATION === */
postfix_evaluate(Expr, Result) :-
    postfix_evaluate(Expr, Result, []).

postfix_evaluate([], Result, [Result]).
postfix_evaluate([N|T], Result, Stack) :-
    integer(N),
    postfix_evaluate(T, Result, [N|Stack]).
postfix_evaluate([Op|T], Result, [V1,V2|Stack]) :-
    member(Op, [+, -, *, /]),
    eval_op(Op, V1, V2, V),
    postfix_evaluate(T, Result, [V|Stack]).

eval_op(+, V1, V2, V) :- V #= V2 + V1.
eval_op(-, V1, V2, V) :- V #= V2 - V1.
eval_op(*, V1, V2, V) :- V #= V2 * V1.
eval_op(/, V1, V2, V) :- V1 #\= 0, V #= V2 div V1.
eval_op(/, 0, _, 0).  % Division by zero

/* === LATTICE PATHS WITH MEMOIZATION === */
:- dynamic p/3.

lattice_paths(N, M, Tabu, R) :-
    retractall(p(_, _, _)),
    nb_setval(tabu, Tabu),
    paths(N, M, R),
    nb_delete(tabu),
    retractall(p(_, _, _)).

paths(0, 0, 1) :- !.
paths(N, M, 0) :- (N < 0 ; M < 0), !.
paths(N, M, R) :- p(N, M, R), !.
paths(N, M, 0) :-
    nb_getval(tabu, Tabu),
    member((N, M), Tabu), !.
paths(N, M, R) :-
    N1 is N - 1,
    M1 is M - 1,
    paths(N1, M, P1),
    paths(N, M1, P2),
    R is P1 + P2,
    assertz(p(N, M, R)).
