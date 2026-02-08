/* Version December 24, 2020 */

/* Your predicates may use the finite domain constraint solver. */

:- use_module(library(clpfd)).

/* Write your predicates in this presently empty space. Each predicate
 * foo that passes the corresponding test predicate test_foo is worth
 * three points to your course grade, up to the maximum of thirty points
 * for ten properly solved problems. */







/* Complete the predicate all/0 to call the test predicates for the
 * Prolog predicates that you have defined in the above space. */

all :-
	/* Fill in the calls to test predicates here. */
	true.

/* DO NOT MODIFY ANYTHING BELOW THIS LINE!!!! */

/* General tester logic, same for all predicates. */

run_tests(Tests, I, F) :-
    run_tests(Tests, 0, 0, I, F).

run_tests([], I, F, I, F) :- !.

run_tests([T|Tests], CI, CF, I, F) :-
    statistics(inferences, I1),
    /* Trick to execute query once without binding its variables. */
    not(not(call(T))),
    !,
    statistics(inferences, I2),
    II is I2 - I1,
    C is CI + II,
    run_tests(Tests, C, CF, I, F).

run_tests([T|Tests], CI, CF, I, F) :-
    write("FAILED: "), write(T), nl,
    CFF is CF + 1,
    run_tests(Tests, CI, CFF, I, F).

/* Helper predicates to write some mass tests. */

total(L, S) :-
    total(L, S, 0).
total([], S, S).
total([H|T], S, SS) :-
    plus(SS, H, SSS),
    total(T, S, SSS).

/* The element X appears in list L exactly C times. */

count(X, L, C) :-
    count(X, L, C, 0).

count(_, [], C, C) :- !.

count(X, [X|T], C, Curr) :-
    !,
    plus(Curr, 1, C2),
    count(X, T, C, C2).

count(X, [_|T], C, Curr) :-
    count(X, T, C, Curr).

/* Run the tests for the given predicate and print the report. */

test_harness(Pred, Tests) :-
    write(Pred), write(": "),
    run_tests(Tests, I, F),
    write("Executed "), write(I), write(" total inferences. "),
    write("Failed "), write(F), write(" test cases."), nl.

/* Test predicates for the individual predicates. */

test_duplicate_digit_bonus :-
	test_harness("duplicate_digit_bonus", [
	% Original tests
	duplicate_digit_bonus(333444555666, 50),
	duplicate_digit_bonus(1223334444555556666667777777, 211111),
	duplicate_digit_bonus(9999999999088888888888, 2100000000),
	duplicate_digit_bonus(2111111747111117777700, 12002),
	(X is 2^50, duplicate_digit_bonus(X, 11)),
	(X is 444^555, duplicate_digit_bonus(X, 216)),
	% From spec
	duplicate_digit_bonus(2223, 10),
	duplicate_digit_bonus(3222, 20),
	duplicate_digit_bonus(9999997777774444488872222, 21210),
	(N is 10^20, duplicate_digit_bonus(N, 2000000000000000000)),
	(N is 1234^5678, duplicate_digit_bonus(N, 15418)),
	% No repeated digits - zero bonus
	duplicate_digit_bonus(1, 0),
	duplicate_digit_bonus(12345, 0),
	duplicate_digit_bonus(1234567890, 0),
	% Single block at end (double bonus)
	duplicate_digit_bonus(122, 2),
	duplicate_digit_bonus(1222, 20),
	duplicate_digit_bonus(12222, 200),
	% Single block not at end
	duplicate_digit_bonus(221, 1),
	duplicate_digit_bonus(2221, 10),
	% Entire number is one block
	duplicate_digit_bonus(77, 2),
	duplicate_digit_bonus(777, 20),
	duplicate_digit_bonus(7777, 200),
	duplicate_digit_bonus(77777, 2000),
	% Two blocks, end block gets double
	duplicate_digit_bonus(22233, 12),
	% All digits same, large
	duplicate_digit_bonus(11111111111, 2000000000),
	% Deterministic
	(findall(B, duplicate_digit_bonus(2223, B), L), L = [10])
	]).           
              
test_three_summers :-
	test_harness("three_summers", [
	% Original tests
	(findall(X, between(1, 20, X), L),
	findall((A, B, C), three_summers(L, 40, A, B, C), LL), length(LL, 33)),
	(findall(Z, (between(1, 20, X), Z is X*X), L),
	findall(N, (between(100, 200, N), three_summers(L, N, A, B, C)), LL),
	sort(LL, Ls), length(Ls, 66)),
	% Basic example from spec
	three_summers([3, 7, 9, 10, 12, 14], 30, 7, 9, 14),
	% All three solutions for [1..6], goal=12
	(findall((A,B,C), three_summers([1,2,3,4,5,6], 12, A, B, C), L), length(L, 3)),
	% Smallest possible list (exactly 3 elements)
	three_summers([1, 2, 3], 6, 1, 2, 3),
	\+ three_summers([1, 2, 3], 7, _, _, _),
	% No solution exists
	\+ three_summers([1, 2, 3, 4, 5], 100, _, _, _),
	\+ three_summers([10, 20, 30], 15, _, _, _),
	% Unique solution
	(findall((A,B,C), three_summers([1, 5, 10, 20], 36, A, B, C), L), length(L, 0)),
	(findall((A,B,C), three_summers([1, 5, 10, 20], 35, A, B, C), L), length(L, 1)),
	% Elements must be in order from list
	three_summers([1, 2, 3, 4, 5, 6], 6, 1, 2, 3),
	\+ three_summers([1, 2, 3, 4, 5, 6], 6, 3, 2, 1),
	% Consecutive integers, count solutions (verified: 10 triples sum to 15 from 1..10)
	(findall(X, between(1, 10, X), L),
	findall((A,B,C), three_summers(L, 15, A, B, C), LL), length(LL, 10)),
	% Sparse list with gaps
	three_summers([2, 5, 11, 20, 50], 36, 5, 11, 20),
	\+ three_summers([2, 5, 11, 20, 50], 77, _, _, _),
	% Goal equals smallest three
	(findall(X, between(1, 100, X), L),
	three_summers(L, 6, 1, 2, 3)),
	% Goal equals largest three
	(findall(X, between(1, 100, X), L),
	three_summers(L, 297, 98, 99, 100))
	]).


test_tukeys_ninther :-
    test_harness("tukeys_ninther", [
    % Original tests
    tukeys_ninther([55, 99, 131, 42, 88, 11, 17, 16, 104, 2,
                     8, 7, 0, 1, 69, 8, 93, 9, 12, 11, 16, 1, 77, 90, 15, 4, 123], 15),
    (L = [4, 42, 987, 3123, 83120, 555321, 9815212, 34343434, 982264982],
        findall(M, (permutation(L, LL), tukeys_ninther(LL, M)), TN),
        count(987, TN, 0),
        count(3123, TN, 77760),
        count(83120, TN, 207360),
        count(555321, TN, 77760),
        count(9815212, TN, 0)),
    % From spec: single element
    tukeys_ninther([15], 15),
    tukeys_ninther([42, 7, 15], 15),
    tukeys_ninther([99, 42, 17, 7, 1, 9, 12, 77, 15], 15),
    % Sorted triplet
    tukeys_ninther([1, 2, 3], 2),
    % Reverse sorted triplet
    tukeys_ninther([3, 2, 1], 2),
    % All same elements
    tukeys_ninther([5, 5, 5], 5),
    tukeys_ninther([7, 7, 7, 7, 7, 7, 7, 7, 7], 7),
    % Sorted list of 9
    tukeys_ninther([1, 2, 3, 4, 5, 6, 7, 8, 9], 5),
    % Reverse sorted list of 9
    tukeys_ninther([9, 8, 7, 6, 5, 4, 3, 2, 1], 5),
    % All permutations of [1,2,3] — median is always 2
    (findall(M, (permutation([1,2,3], P), tukeys_ninther(P, M)), Ms),
     msort(Ms, [2,2,2,2,2,2])),
    % Ninther of 27 sorted elements
    tukeys_ninther([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27], 14),
    % Negative numbers
    tukeys_ninther([-5, -2, -8], -5),
    tukeys_ninther([-9, -3, -6, -1, -7, -4, -8, -2, -5], -5),
    % The minimum and maximum of a length-3 list can never be the ninther
    (findall(M, (permutation([10, 20, 30], P), tukeys_ninther(P, M)), Ms),
     \+ member(10, Ms), \+ member(30, Ms))
    ]).         

test_give_change :-
	test_harness("give_change", [
	% Original tests
	give_change(100, [55, 10, 1], [55, 10, 10, 10, 10, 1, 1, 1, 1, 1]),
	\+ give_change(34, [20, 9, 6], _),
	(findall(Y, (between(1, 1000, N), give_change(N, [42, 17, 5, 1], Y)), L), flatten(L, LL), total(LL, 500500)),
	% From spec
	give_change(120, [50, 40, 5], [50, 50, 5, 5, 5, 5]),
	give_change(100, [42, 17, 11, 6, 1], [42, 42, 11, 1, 1, 1, 1, 1]),
	% Zero amount
	give_change(0, [50, 20, 10], []),
	% Exact single coin
	give_change(50, [50, 20, 10], [50]),
	give_change(20, [50, 20, 10], [20]),
	% All same denomination
	give_change(15, [5], [5, 5, 5]),
	give_change(1, [1], [1]),
	% Greedy fails but non-greedy would succeed
	\+ give_change(6, [4, 3], _),
	\+ give_change(12, [9, 5], _),
	% Greedy succeeds with unit coin (always works)
	give_change(99, [50, 20, 10, 5, 1], [50, 20, 20, 5, 1, 1, 1, 1]),
	% Large single coin
	give_change(100, [100], [100]),
	\+ give_change(101, [100], _),
	% Coin too large for amount, skip to smaller
	give_change(3, [50, 20, 10, 1], [1, 1, 1]),
	% Deterministic
	(findall(C, give_change(120, [50, 40, 5], C), L), length(L, 1)),
	% Amount not reachable with given coins
	\+ give_change(7, [5, 3], _),
	\+ give_change(1, [5, 3], _),
	% Greedy fails: 40 = 20+20, but greedy takes 25 first
	\+ give_change(40, [25, 20], _)
	]).        

test_extract_increasing :-
	test_harness("extract_increasing", [
	% Original tests
	extract_increasing("0123456789", [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
	extract_increasing("77777777777777777777777",
	                   [7, 77, 777, 7777, 77777, 777777]),
	extract_increasing("3141592653589793238462643383279502884",
                       [3, 14, 15, 92, 653, 5897, 9323, 84626, 433832, 795028]),
	extract_increasing("2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932003059921817413596629043572900334295260",
                       [2, 7, 18, 28, 182, 845, 904, 5235, 36028, 74713, 526624, 977572,
                         4709369, 9959574, 96696762, 772407663, 3535475945, 7138217852,
                         51664274274, 66391932003, 599218174135, 966290435729]),
	% From spec
	extract_increasing("1234567890987654321",
                       [1, 2, 3, 4, 5, 6, 7, 8, 9, 98, 765, 4321]),
	extract_increasing("122333444455555666666",
                       [1, 2, 23, 33, 44, 445, 555, 566, 666]),
	% Single digit
	extract_increasing("5", [5]),
	extract_increasing("0", [0]),
	% All same digit
	extract_increasing("1111111", [1, 11, 111]),
	% Strictly ascending digits
	extract_increasing("13579", [1, 3, 5, 7, 9]),
	% Descending digits
	extract_increasing("987654321", [9, 87, 654]),
	% All zeros - only first one qualifies
	extract_increasing("0000000", [0]),
	% Two digits
	extract_increasing("19", [1, 9]),
	extract_increasing("91", [9]),
	% Repeated twos
	extract_increasing("2222222222", [2, 22, 222, 2222]),
	% Length check (1+2+3+4+5+6+7=28, leaving 4 leftover)
	(extract_increasing("11111111111111111111111111111111", L), length(L, 7))
	]).

test_pancake_scramble :-
	test_harness("pancake_scramble", [
	pancake_scramble("", ""),
	pancake_scramble("q", "q"),
	pancake_scramble("ab", "ba"),
	pancake_scramble("artificial intelligence", "englen acftariiilitliec"),
	pancake_scramble("pancakes with jam", "mjhi eanpackswt a"),
	pancake_scramble("Prolog Schmolog", "glmc ooPrlgShoo")
	]).          
              
test_domino_cycle :-
	test_harness("domino_cycle", [
	% Original tests
	domino_cycle([(3, 5), (5, 2), (2, 3)]),
	domino_cycle([(4, 4)]),
	\+ domino_cycle([(4, 1), (1, 7), (7, 2)]),
	(domino_cycle([(A, 3), (3, 1), (1, A), (1, 1), (1, A)]), A = 1),
	\+ domino_cycle([(B, 5), (5, 2), (B, 3), (3, 4)]),
	(findall(C, (length(C, 5), domino_cycle(C)), L), length(L, 7776)),
	% Single tile must self-loop
	domino_cycle([(1, 1)]),
	domino_cycle([(6, 6)]),
	\+ domino_cycle([(3, 5)]),
	\+ domino_cycle([(1, 6)]),
	% Pips out of range
	\+ domino_cycle([(0, 0)]),
	\+ domino_cycle([(7, 7)]),
	\+ domino_cycle([(0, 3), (3, 0)]),
	\+ domino_cycle([(-1, -1)]),
	% Two-tile cycles
	domino_cycle([(2, 5), (5, 2)]),
	domino_cycle([(1, 1), (1, 1)]),
	\+ domino_cycle([(2, 5), (5, 3)]),
	% Chain connects but cycle doesn't close
	\+ domino_cycle([(1, 2), (2, 3), (3, 4)]),
	% Longer valid cycle
	domino_cycle([(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 1)]),
	% Unification: find closing value
	(domino_cycle([(4, 5), (5, 2), (2, 3), (3, X)]), X = 4),
	% Unification: enumerate valid first pips
	(findall(A2, domino_cycle([(A2, 1), (1, 2), (2, A2)]), Sols), length(Sols, 6)),
	% Counting: all valid single-tile cycles (6 total: (1,1)..(6,6))
	(findall(C2, (length(C2, 1), domino_cycle(C2)), L2), length(L2, 6)),
	% Counting: two-tile cycles
	(findall(C3, (length(C3, 2), domino_cycle(C3)), L3), length(L3, 36)),
	% Counting: three-tile cycles
	(findall(C4, (length(C4, 3), domino_cycle(C4)), L4), length(L4, 216))
	]).        

test_taxi_zum_zum :-
	test_harness("taxi_zum_zum", [
	% Original tests
	taxi_zum_zum("f", (0, 1)),
	taxi_zum_zum("fflllfrlflrfrlrrl", (3, 2)),
	taxi_zum_zum("rrrrrrrrrllllrrrrrrrrrrrr", (0, 0)),
	taxi_zum_zum("frfflffllfffr", (2, 0)),
	taxi_zum_zum("lffrfrrfflfllrfflf", (-2, 1)),
	% Empty moves: stay at origin
	taxi_zum_zum("", (0, 0)),
	% From spec
	taxi_zum_zum("rfrfrfrf", (0, 0)),
	taxi_zum_zum("llflflrlfr", (1, 0)),
	% Only turns, no movement
	taxi_zum_zum("llll", (0, 0)),
	taxi_zum_zum("rrrr", (0, 0)),
	taxi_zum_zum("lr", (0, 0)),
	% Straight lines in each direction
	taxi_zum_zum("fff", (0, 3)),
	taxi_zum_zum("rfff", (3, 0)),
	taxi_zum_zum("rrfff", (0, -3)),
	taxi_zum_zum("lfff", (-3, 0)),
	% Square: return to origin
	taxi_zum_zum("fffrfffrfffrfff", (0, 0)),
	% Negative coordinates
	taxi_zum_zum("llf", (0, -1)),
	taxi_zum_zum("lf", (-1, 0)),
	% Closed loop
	taxi_zum_zum("ffrffrffrff", (0, 0)),
	% Many forwards
	taxi_zum_zum("ffffffffff", (0, 10)),
	% U-turn
	taxi_zum_zum("fllf", (0, 0)),
	% Spiral outward
	taxi_zum_zum("frflfrflf", (2, 3)),
	% Three right turns = one left turn
	taxi_zum_zum("lfff", (-3, 0)),
	taxi_zum_zum("rrrfff", (-3, 0))
	]).
                                 
test_group_and_skip :-
	test_harness("group_and_skip", [
	% Original tests
	group_and_skip(99, 5, 3, [3, 4, 3, 3, 2, 4]),
	group_and_skip(123456789, 1000, 1, [123, 456, 789]),
	group_and_skip(255, 2, 1, [1, 1, 1, 1, 1, 1, 1, 1]),
	group_and_skip(10^9, 13, 3, [3, 8, 5, 10, 8, 6, 11, 8, 9, 7, 0, 2, 1, 12]),
	% Integer base conversions (In=1)
	group_and_skip(12345, 2, 1, [1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1]),
	group_and_skip(12345, 8, 1, [3, 0, 0, 7, 1]),
	group_and_skip(255, 16, 1, [15, 15]),
	group_and_skip(12345, 10, 1, [1, 2, 3, 4, 5]),
	group_and_skip(9876543210, 1000, 1, [9, 876, 543, 210]),
	% Simple small cases
	group_and_skip(1, 2, 1, [1]),
	group_and_skip(2, 2, 1, [1, 0]),
	group_and_skip(3, 2, 1, [1, 1]),
	% Coins example from spec: N=100, Out=8, In=5
	group_and_skip(100, 8, 5, [5, 2, 4, 3, 4, 4]),
	% Arithmetic expression evaluation
	group_and_skip(2^16, 2, 1, [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
	group_and_skip(2^8, 16, 1, [1, 0, 0]),
	% All remainders zero except leading digit
	group_and_skip(1000000, 10, 1, [1, 0, 0, 0, 0, 0, 0]),
	% Length of result list
	(group_and_skip(2^20 - 1, 2, 1, L), length(L, 20))
	]).

test_bulgarian_solitaire :-
	test_harness("bulgarian_solitaire", [
	% Original tests
	bulgarian_solitaire([1, 1, 1], 2, 2),
	bulgarian_solitaire([5, 4, 1], 4, 10),
	bulgarian_solitaire([6, 4, 2, 1, 3, 5], 6, 0),
	bulgarian_solitaire([8, 3, 3, 1], 5, 9),
	bulgarian_solitaire([10, 10, 10, 10, 10, 5], 10, 74),
	bulgarian_solitaire([3000, 2050], 100, 7325),
	% From spec
	bulgarian_solitaire([3, 5, 2, 1, 4], 5, 0),
	bulgarian_solitaire([5, 2, 2, 1], 4, 11),
	bulgarian_solitaire([1250, 1250, 2550], 100, 8185),
	% Already at goal for various K
	bulgarian_solitaire([1], 1, 0),
	bulgarian_solitaire([1, 2], 2, 0),
	bulgarian_solitaire([2, 1], 2, 0),
	bulgarian_solitaire([1, 2, 3], 3, 0),
	bulgarian_solitaire([3, 2, 1], 3, 0),
	bulgarian_solitaire([2, 3, 1], 3, 0),
	bulgarian_solitaire([1, 2, 3, 4, 5, 6, 7], 7, 0),
	bulgarian_solitaire([7, 6, 5, 4, 3, 2, 1], 7, 0),
	% Any permutation of goal is zero moves
	bulgarian_solitaire([4, 2, 5, 1, 3], 5, 0),
	bulgarian_solitaire([10, 9, 8, 7, 6, 5, 4, 3, 2, 1], 10, 0)
	]).

test_only_odd_digits :-
	test_harness("only_odd_digits", [
	% Original tests
	only_odd_digits(1),
	only_odd_digits(999919999199991),
	only_odd_digits(135797531),
	\+ only_odd_digits(1354797531),
	\+ only_odd_digits(7717936191),
	\+ only_odd_digits(0),
	(findall(N, (between(1, 1000, N), only_odd_digits(N)), L), length(L, 155)),
	(findall(N, (between(1, 100000, N), only_odd_digits(N)), L), length(L, 3905)),
	% Single odd digits
	only_odd_digits(3),
	only_odd_digits(5),
	only_odd_digits(7),
	only_odd_digits(9),
	% Single even digits
	\+ only_odd_digits(2),
	\+ only_odd_digits(4),
	\+ only_odd_digits(6),
	\+ only_odd_digits(8),
	% Even digit hiding in various positions
	\+ only_odd_digits(213),          % even digit at start
	\+ only_odd_digits(149),          % even digit in middle (not really - let me fix)
	\+ only_odd_digits(152),          % even digit at end
	\+ only_odd_digits(51173911390),  % zero buried deep
	% All same odd digit, various lengths
	only_odd_digits(1111111),
	only_odd_digits(3333333333),
	only_odd_digits(77777),
	% Large number, all odd
	only_odd_digits(1379531793513795),
	% Fails on last digit only
	\+ only_odd_digits(11111111112),
	% Fails on first digit only
	\+ only_odd_digits(21111111111),
	% Powers of ten (all fail)
	\+ only_odd_digits(10),
	\+ only_odd_digits(100),
	\+ only_odd_digits(1000000)
	]).

test_josephus :-
	test_harness("josephus", [
	% Original tests
	josephus([joe, moe, bob, rob, josephus], 2, bob),
	josephus([joe, moe, bob, rob, josephus], 99, josephus),
	(findall(N, between(1, 30, N), L), josephus(L, 4, 6)),
	(findall(N, between(1, 1000, N), L), josephus(L, 13, 396)),
	(findall(N, between(1, 10000, N), L), josephus(L, 77, 7373)),
	(findall(N, (permutation([1, 2, 3, 4, 5], P), josephus(P, 3, N)), L), count(2, L, 24)),
	% From spec
	(findall(N, between(1, 1234, N), L), josephus(L, 42, 426)),
	% Single person always survives
	josephus([solo], 1, solo),
	josephus([solo], 99, solo),
	% Two people
	josephus([a, b], 1, b),
	josephus([a, b], 2, a),
	josephus([a, b], 3, b),
	% K=1: always the last person in list
	josephus([a, b, c, d, e], 1, e),
	(findall(N, between(1, 100, N), L), josephus(L, 1, 100)),
	% K=2: classic Josephus, power of 2
	(findall(N, between(1, 16, N), L), josephus(L, 2, 1)),
	% Non-integer elements
	josephus([alice, bob, charlie, diana], 2, alice),
	josephus([foo(1), foo(2), foo(3)], 2, foo(3)),
	% Large K relative to list size
	josephus([a, b, c], 100, b),
	% Every permutation of [1,2,3,4,5] with K=2
	(findall(N, (permutation([1, 2, 3, 4, 5], P), josephus(P, 2, N)), L), count(4, L, 24)),
	% Result depends on position, not identity
	(findall(N, between(1, 10, N), L), josephus(L, 3, 4))
	]).

test_first_missing_positive :-
	test_harness("first_missing_positive", [ 
	% Original tests
	first_missing_positive([99999, 123, 1, 24, 5, 9999999, 222, 3, 4, 7777777, 2], 6),
	(findall(X, first_missing_positive([99, 4, 1, 3, 7, 2], X), L), L = [5]),
	(findall(Y, between(1, 1000, Y), LLL), reverse(LLL, LL), findall(X, first_missing_positive(LL, X), L), L = [1001]),
	first_missing_positive([-1, -2, -3, -4, -4, 0, 1], 2),
	first_missing_positive([1, 2, [3, 4]], 3),
	% Empty list -> 1
	first_missing_positive([], 1),
	% No positive integers at all
	first_missing_positive([-5, -10, 0, -999], 1),
	% Non-integer terms everywhere
	first_missing_positive([bob, jack, foo(bar, baz, qux)], 1),
	first_missing_positive([hello, 3.14, a+b, [], "string"], 1),
	% Consecutive from 1 with no gaps
	first_missing_positive([1, 2, 3, 4, 5], 6),
	first_missing_positive([5, 3, 1, 4, 2], 6),
	% Gap at 1
	first_missing_positive([42, 99, 123456, -3, 777], 1),
	first_missing_positive([2, 3, 4, 5, 6], 1),
	% Gap at 2
	first_missing_positive([1, 3, 4, 5, 6], 2),
	% Duplicates don't help
	first_missing_positive([1, 1, 1, 1, 1], 2),
	first_missing_positive([1, 2, 2, 2, 3, 3], 4),
	% Mixed types including integers
	first_missing_positive([foo, 1, bar(3), 2, [5], 3], 4),
	% Lists as elements are not recursed into
	first_missing_positive([[1], [2], [3]], 1),
	first_missing_positive([2, 3, 4, [1, 1, 1, 1]], 1),
	% Large gap at start, dense later
	first_missing_positive([100, 101, 102, 103], 1),
	% Result is deterministic (single solution)
	(findall(X, first_missing_positive([3, 1, 4, 1, 5, 9, 2, 6], X), L), L = [7])
	]).

test_riffle :-
	test_harness("riffle", [
	riffle([1,2,3,4], [5,6,7,8], [1,5,2,6,3,7,4,8], left),
	riffle([1,2,3,4], [5,6,7,8], [5,1,6,2,7,3,8,4], right),
	(riffle([42, bob, 99], [55, jack, tom], [55|_], M), M = right),
	\+ riffle([11, 12, 13, 14], [1, 2, 3, 4, 5, 6], L, M),
	(findall(M, riffle([11, 12, 13, 14], [1, 2, 3, 4], L, M), Z), length(Z, Z2), Z2 = 2)
	]).

test_sz :-
	test_harness("sz", [
	% Original tests
	sz(272, 77777777777777770000),
	(findall(S, sz(555, S), L), L = [7770]),
	(findall(S, sz(2727, S), L), L = [777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777]),
	sz(1, 7),
	sz(2^20, 700000000000000000000),
	sz(129, 777777777777777777777),
	% From spec
	sz(42, 7770),
	sz(400000, 70000000),
	sz(123, 777777777777777),
	% Trivially verifiable
	sz(7, 7),
	sz(77, 77),
	sz(777, 777),
	sz(7777, 7777),
	sz(10, 70),
	sz(100, 700),
	sz(1000, 7000),
	sz(2, 70),
	sz(5, 70),
	sz(11, 77),
	% Deterministic
	(findall(S, sz(42, S), L), L = [7770]),
	% seven_zero helper tests
	(seven_zero(N, 5, 8), N =:= 7777700000000),
	(seven_zero(N, 20, 0), N =:= 77777777777777777777),
	(seven_zero(N, 0, 10), N =:= 0)
	]).

test_crag :-
	test_harness("crag", [
	crag(5, 4, 5, 10),
	(findall(S, crag(3, 4, X, S), L), sort(L, [4, 5, 6, 8, 26])),
	(findall((A, B, C), crag(A, B, C, 26), L), length(L, 12)),
	\+ crag(6, 6, 6, 18),
	(findall((A, B, C), crag(A, B, C, 25), X), length(X, 6))          
	]).          

test_count_dominators :-
	test_harness("count_dominators", [
	% Original tests
	count_dominators([], 0),
	count_dominators([33, 22, 11, 64, -2, 5], 2),
	(findall(X, between(1, 1000, X), L), reverse(L, LL), findall(D, count_dominators(LL, D), LD), LD = [1000]),
	(findall(L, (between(1, 5, X), between(1, 5, Y), L = [X, Y], count_dominators(L, 2)), V), length(V, 10)),
	count_dominators([[1,2,3]], 1),
	% From spec
	count_dominators([42, 99, 17, 3, 9], 3),
	count_dominators([4, 3, 2, 1], 4),
	count_dominators([1, 2, 3, 4], 1),
	% Single element - always a dominator
	count_dominators([1], 1),
	count_dominators([999], 1),
	% Two elements
	count_dominators([5, 3], 2),
	count_dominators([3, 5], 1),
	count_dominators([5, 5], 1),
	% All equal - only last is dominator
	count_dominators([7, 7, 7, 7, 7], 1),
	% Strictly decreasing - all are dominators
	count_dominators([10, 9, 8, 7, 6, 5, 4, 3, 2, 1], 10),
	% Strictly increasing - only last
	count_dominators([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 1),
	% Negative numbers
	count_dominators([-1, -5, -10, -3, -20], 3),
	count_dominators([-10, -9, -8, -7], 1),
	% Peak in middle
	count_dominators([1, 2, 100, 3, 2, 1], 4),
	% Sorted then reversed: all dominators
	(findall(X, between(1, 100, X), L), reverse(L, LL), count_dominators(LL, 100)),
	% Sorted: only last
	(findall(X, between(1, 100, X), L), count_dominators(L, 1)),
	% Deterministic
	(findall(D, count_dominators([42, 99, 17, 3, 9], D), L), L = [3]),
	% Count two-element lists from [1..5] where only 1 dominator
	(findall(L, (between(1, 5, X), between(1, 5, Y), L = [X, Y], count_dominators(L, 1)), V), length(V, 15))
	]).

test_running_median :-
	test_harness("running_median", [
	% Original tests
	running_median([99, 42, 17, 55, -4, 18, 77], [99, 42, 42, 42, 17, 18, 18]),
	(running_median([42, 42, 42, 42, 42, 42, 42], L), L = [42, 42, 42, 42, 42, 42, 42]),
	running_median([1,2,3,4,5,6], [1,2,2,3,4,5]),
	running_median([1, 1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1, 1]),
	running_median([A, B], [A, B]),
	% From spec
	running_median([1, 2, 3, 4, 5], [1, 2, 2, 3, 4]),
	running_median([42, 42, 99, 42, 42], [42, 42, 42, 42, 42]),
	% Edge cases: empty and single
	running_median([], []),
	running_median([7], [7]),
	% Two elements pass through
	running_median([5, 3], [5, 3]),
	running_median([100, -100], [100, -100]),
	% Strictly decreasing
	running_median([6, 5, 4, 3, 2, 1], [6, 5, 5, 4, 3, 2]),
	% Alternating high/low
	running_median([1, 100, 1, 100, 1, 100], [1, 100, 1, 100, 1, 100]),
	running_median([100, 1, 100, 1, 100, 1], [100, 1, 100, 1, 100, 1]),
	% Spike in middle
	running_median([1, 1, 1000, 1, 1], [1, 1, 1, 1, 1]),
	% Negative numbers
	running_median([-5, -3, -1, -7, -2], [-5, -3, -3, -3, -2]),
	% All same value absorbed
	running_median([5, 5, 5, 5], [5, 5, 5, 5]),
	% Three elements
	running_median([3, 1, 2], [3, 1, 2]),
	running_median([1, 3, 2], [1, 3, 2]),
	% Large values
	running_median([1000000, 1, 500000], [1000000, 1, 500000]),
	% Deterministic
	(findall(M, running_median([99, 42, 17, 55, -4, 18, 77], M), L), length(L, 1))
	]).

test_safe_squares_rooks :-
	test_harness("safe_squares_rooks", [
	% Original tests
	safe_squares_rooks([(2, 2), (3, 1), (5, 5), (2, 5)], 5, 4),
	(findall((X, X), between(1, 50, X), L), safe_squares_rooks(L, 50, S), S = 0),
	safe_squares_rooks([(4,3), (2,2), (1,2)], 10, 56),
	safe_squares_rooks([(1, 1), (3, 1), (3, 2)], 5, 9),
	safe_squares_rooks([(1, 1), (2, 2), (3, 4)], 1000, 994009),
	% From spec
	safe_squares_rooks([(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)], 5, 0),
	safe_squares_rooks([], 100, 10000),
	% No rooks
	safe_squares_rooks([], 1, 1),
	safe_squares_rooks([], 5, 25),
	% Single rook
	safe_squares_rooks([(1, 1)], 1, 0),
	safe_squares_rooks([(3, 3)], 5, 16),
	safe_squares_rooks([(1, 1)], 10, 81),
	% Duplicate rooks (same square)
	safe_squares_rooks([(1, 1), (1, 1)], 5, 16),
	safe_squares_rooks([(2, 3), (2, 3), (2, 3)], 5, 16),
	% All rooks in same row
	safe_squares_rooks([(1, 1), (1, 2), (1, 3)], 5, 8),
	% All rooks in same column
	safe_squares_rooks([(1, 1), (2, 1), (3, 1)], 5, 8),
	% Full row and column coverage
	(findall((1, C), between(1, 10, C), L), safe_squares_rooks(L, 10, 0)),
	(findall((R, 1), between(1, 10, R), L), safe_squares_rooks(L, 10, 0)),
	% Large board, few rooks
	safe_squares_rooks([(1, 1)], 1000, 998001),
	safe_squares_rooks([(1, 1), (1001, 1001)], 1001, 998001),
	% Corner rooks
	safe_squares_rooks([(1, 1), (1, 5), (5, 1), (5, 5)], 5, 9),
	% Deterministic
	(findall(S, safe_squares_rooks([(2, 2)], 5, S), L), L = [16])
	]).

test_trick_winner :-
	test_harness("trick_winner", [
	trick_winner([(five, spades), (queen, diamonds), (ace, spades), (ten, spades)], (ace, spades)),
	(findall(X, trick_winner([(six, spades), (two, hearts), (X, spades), (nine, clubs)], (six, spades)), L), length(L, 4)),
	(findall(X, trick_winner([(five, diamonds), X, (ten, hearts), (ten, diamonds)], X), L), length(L, 4)),
	\+ trick_winner([(seven, spades), (two, hearts), (six, spades), (nine, clubs)], (two, hearts)),
	(findall(X, trick_winner([(ace, S), (two, S), (six, S), (king, S)], X), Z), length(Z, 4))
	]).

test_sum_of_two_squares :-
	test_harness("sum_of_two_squares", [
	% Original tests
	\+ sum_of_two_squares(11, _, _),
	sum_of_two_squares(50, 7, 1),
	(X is 123^2 + 456^2, sum_of_two_squares(X, 456, 123)),
	(X is 555^2 + 666^2, sum_of_two_squares(X, 810, 309)),
	(findall(N, (between(1, 2000, N), sum_of_two_squares(N, _, _)), L), length(L, 591)),
	% From spec
	sum_of_two_squares(2, 1, 1),
	sum_of_two_squares(100, 8, 6),
	(N is 333^2 + 444^2, sum_of_two_squares(N, 528, 171)),
	(findall(N, (between(1, 100, N), sum_of_two_squares(N, _, _)), L), length(L, 35)),
	% Small values
	\+ sum_of_two_squares(1, _, _),
	sum_of_two_squares(5, 2, 1),
	\+ sum_of_two_squares(3, _, _),
	\+ sum_of_two_squares(6, _, _),
	\+ sum_of_two_squares(7, _, _),
	sum_of_two_squares(8, 2, 2),
	sum_of_two_squares(10, 3, 1),
	sum_of_two_squares(13, 3, 2),
	% Equal squares
	sum_of_two_squares(18, 3, 3),
	sum_of_two_squares(32, 4, 4),
	sum_of_two_squares(72, 6, 6),
	% Largest A is returned (not smallest)
	sum_of_two_squares(25, 4, 3),
	\+ sum_of_two_squares(25, 3, 4),
	% Deterministic (cut ensures single solution)
	(findall((A,B), sum_of_two_squares(50, A, B), L), length(L, 1)),
	(findall((A,B), sum_of_two_squares(325, A, B), L), length(L, 1)),
	% Numbers that cannot be expressed
	\+ sum_of_two_squares(3, _, _),
	\+ sum_of_two_squares(15, _, _),
	\+ sum_of_two_squares(99, _, _),
	% Larger values
	sum_of_two_squares(1000, 30, 10)
	]).

test_hitting_integer_powers :-
	test_harness("hitting_integer_powers", [
	% Original tests
	hitting_integer_powers(2, 7, 100, 73, 26),
	hitting_integer_powers(3, 6, 100, 137, 84),
	hitting_integer_powers(4, 5, 1000, 916, 789),
	hitting_integer_powers(10, 11, 1000, 1107, 1063),
	hitting_integer_powers(42, 51, 10000, 29546, 28087),
	% From spec
	hitting_integer_powers(6, 10, 1000, 595, 463),
	hitting_integer_powers(2, 10, 10000, 13301, 4004)
	]).

test_sum_of_distinct_cubes :-
	test_harness("sum_of_distinct_cubes", [
	% Original tests
	sum_of_distinct_cubes(777777777, [919, 117, 29, 6]),
	(sum_of_distinct_cubes(123456789, L), L = [497, 88, 22, 8, 7, 6, 5]),
	(X is 10^16+1, sum_of_distinct_cubes(X, L), L = [215443, 4027, 139, 12, 10, 8, 5, 3]),
	sum_of_distinct_cubes(1, [1]),
	% From spec
	sum_of_distinct_cubes(100, [4, 3, 2, 1]),
	\+ sum_of_distinct_cubes(721, _),
	sum_of_distinct_cubes(12345, [20, 12, 10, 9, 8, 6, 5, 3, 2]),
	sum_of_distinct_cubes(999999999999, [9999, 669, 81, 27, 7, 6, 2]),
	(X is 123^3 + 456^3 + 789^3, sum_of_distinct_cubes(X, [837, 113, 30, 13, 6, 5, 4])),
	% Single cube
	sum_of_distinct_cubes(8, [2]),
	sum_of_distinct_cubes(27, [3]),
	sum_of_distinct_cubes(64, [4]),
	sum_of_distinct_cubes(1000, [10]),
	% Two cubes
	sum_of_distinct_cubes(9, [2, 1]),
	sum_of_distinct_cubes(35, [3, 2]),
	% No solution
	\+ sum_of_distinct_cubes(2, _),
	\+ sum_of_distinct_cubes(3, _),
	\+ sum_of_distinct_cubes(4, _),
	\+ sum_of_distinct_cubes(5, _),
	\+ sum_of_distinct_cubes(6, _),
	\+ sum_of_distinct_cubes(7, _),
	% Deterministic
	(findall(L, sum_of_distinct_cubes(100, L), Ls), length(Ls, 1)),
	% Sum of 1^3 + 2^3 + ... + n^3 = (n(n+1)/2)^2
	sum_of_distinct_cubes(36, [3, 2, 1])
	]).

test_fibonacci_sum :-
	test_harness("fibonacci_sum", [
	% Original tests
	fibonacci_sum(10, [8, 2]),
	fibonacci_sum(42, [34, 8]),
	fibonacci_sum(100, [89, 8, 3]),
	fibonacci_sum(12345, [10946, 987, 377, 34, 1]),
	fibonacci_sum(665544332211, [591286729879, 53316291173, 20365011074, 433494437, 102334155, 39088169, 1346269, 28657, 6765, 1597, 34, 2]),
	(X is 10^100, fibonacci_sum(X, L), length(L, 137)),
	(X is 10^1000, fibonacci_sum(X, L), length(L, 1316)),
	fibonacci_sum(1, [1]),
	fibonacci_sum(58001746501815487425285,
	              [43566776258854844738105, 10284720757613717413913, 3928413764606871165730, 218922995834555169026, 2880067194370816120, 23416728348467685, 8944394323791464, 190392490709135, 72723460248141, 27777890035288, 4052739537881, 1548008755920, 86267571272, 4807526976, 1836311903, 701408733, 165580141, 63245986, 24157817, 514229, 196418, 46368, 10946, 4181, 1597, 233, 55, 21, 1]),
	% From spec
	fibonacci_sum(30, [21, 8, 1]),
	fibonacci_sum(1000000, [832040, 121393, 46368, 144, 55]),
	% Fibonacci numbers themselves (single element result)
	fibonacci_sum(1, [1]),
	fibonacci_sum(2, [2]),
	fibonacci_sum(3, [3]),
	fibonacci_sum(5, [5]),
	fibonacci_sum(8, [8]),
	fibonacci_sum(13, [13]),
	fibonacci_sum(21, [21]),
	fibonacci_sum(55, [55]),
	fibonacci_sum(89, [89]),
	% One less than a Fibonacci number
	fibonacci_sum(7, [5, 2]),
	fibonacci_sum(12, [8, 3, 1]),
	fibonacci_sum(20, [13, 5, 2]),
	fibonacci_sum(54, [34, 13, 5, 2]),
	% No consecutive Fibonacci numbers in result
	fibonacci_sum(4, [3, 1]),
	fibonacci_sum(6, [5, 1]),
	fibonacci_sum(9, [8, 1]),
	fibonacci_sum(11, [8, 3]),
	% Deterministic
	(findall(L, fibonacci_sum(100, L), Ls), length(Ls, 1)),
	% Result elements are non-consecutive Fibonacci numbers
	fibonacci_sum(1000, [987, 13]),
	% Large Fibonacci number
	fibonacci_sum(144, [144]),
	fibonacci_sum(233, [233])
	]).