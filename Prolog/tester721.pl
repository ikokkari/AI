/* Version July 6, 2020 */

:- use_module(library(clpfd)).

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
    write('FAILED: '), write(T), nl,
    CFF is CF + 1,
    run_tests(Tests, CI, CFF, I, F).

/* Helper predicates to write some mass tests. */

total(L, S) :-
    total(L, S, 0).
total([], S, S).
total([H|T], S, SS) :-
    plus(SS, H, SSS),
    total(T, S, SSS).

count(X, L, C) :-
    count(X, L, C, 0).

count(_, [], C, C) :- !.

count(X, [X|T], C, Curr) :-
    !,
    plus(Curr, 1, C2),
    count(X, T, C, C2).

count(X, [_|T], C, Curr) :-
    count(X, T, C, Curr).

/* Test predicates for the individual predicates. */

test_duplicate_digit_bonus :-
    run_tests([
	duplicate_digit_bonus(333444555666, 50),
    duplicate_digit_bonus(1223334444555556666667777777, 211111),
    duplicate_digit_bonus(9999999999088888888888, 2100000000),
    duplicate_digit_bonus(2111111747111117777700, 12002),
    (X is 2^50, duplicate_digit_bonus(X, 11)),
    (X is 444^555, duplicate_digit_bonus(X, 216))            
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.
              
              
test_three_summers :-
    run_tests([
	(findall(X, between(1, 20, X), L),
    findall((A, B, C), three_summers(L, 40, A, B, C), LL), length(LL, 33)),
    (findall(Z, (between(1, 20, X), Z is X*X), L),
    findall(N, (between(100, 200, N), three_summers(L, N, A, B, C)), LL),
    sort(LL, Ls), length(Ls, 66))
                
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.
                        
test_tukeys_ninther :-
    run_tests([
    tukeys_ninther([55, 99, 131, 42, 88, 11, 17, 16, 104, 2,
                     8, 7, 0, 1, 69, 8, 93, 9, 12, 11, 16, 1, 77, 90, 15, 4, 123], 15),
    (L = [4, 42, 987, 3123, 83120, 555321, 9815212, 34343434, 982264982],
        findall(M, (permutation(L, LL), tukeys_ninther(LL, M)), TN),
        count(987, TN, 0),
    	count(3123, TN, 77760),
    	count(83120, TN, 207360),
        count(555321, TN, 77760),
        count(9815212, TN, 0))
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.           

test_give_change :-
    run_tests([
    give_change(100, [55, 10, 1], [55, 10, 10, 10, 10, 1, 1, 1, 1, 1]),
    \+ give_change(34, [20, 9, 6], _),
    (findall(Y, (between(1, 1000, N), give_change(N, [42, 17, 5, 1], Y)), L), flatten(L, LL), total(LL, 500500))
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.         

test_extract_increasing :-
    run_tests([
    extract_increasing('0123456789', [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
    extract_increasing('77777777777777777777777',
                  	   [7, 77, 777, 7777, 77777, 777777]),
    extract_increasing('3141592653589793238462643383279502884',
                       [3, 14, 15, 92, 653, 5897, 9323, 84626, 433832, 795028]),
    extract_increasing('2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932003059921817413596629043572900334295260',
                       [2, 7, 18, 28, 182, 845, 904, 5235, 36028, 74713, 526624, 977572,
                         4709369, 9959574, 96696762, 772407663, 3535475945, 7138217852,
                         51664274274, 66391932003, 599218174135, 966290435729])
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.          

test_pancake_scramble :-
    run_tests([
    pancake_scramble("", ""),
    pancake_scramble("q", "q"),
    pancake_scramble("ab", "ba"),
	pancake_scramble("artificial intelligence", "englen acftariiilitliec"),
    pancake_scramble("pancakes with jam", "mjhi eanpackswt a"),
    pancake_scramble("Prolog Schmolog", "glmc ooPrlgShoo")
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.          
              
test_domino_cycle :-
    run_tests([
    domino_cycle([(3, 5), (5, 2), (2, 3)]),
    domino_cycle([(4, 4)]),
    \+ domino_cycle([(4, 1), (1, 7), (7, 2)]),
    (domino_cycle([(A, 3), (3, 1), (1, A), (1, 1), (1, A)]), A = 1),
    \+ domino_cycle([(B, 5), (5, 2), (B, 3), (3, 4)]),
    findall(C, (length(C, 5), domino_cycle(C)), L), length(L, 7776)            
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.          

test_taxi_zum_zum :-
    run_tests([
    taxi_zum_zum('fflllfrlflrfrlrrl', (3, 2)),
    taxi_zum_zum('rrrrrrrrrrrrrrrrrrrrrr', (0, 0))
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_group_and_skip :-
    run_tests([
    group_and_skip(99, 5, 3, [3, 4, 3, 3, 2, 4]),
    group_and_skip(123456789, 1000, 1, [123, 456, 789]),
    group_and_skip(255, 2, 1, [1, 1, 1, 1, 1, 1, 1, 1]),
	group_and_skip(10^9, 13, 3, [3, 8, 5, 10, 8, 6, 11, 8, 9, 7, 0, 2, 1, 12])	
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.              
              
test_bulgarian_solitaire :-
    run_tests([
    bulgarian_solitaire([6, 4, 2, 1, 3, 5], 6, 0),
    bulgarian_solitaire([8, 3, 3, 1], 5, 9),
    bulgarian_solitaire([10, 10, 10, 10, 10, 5], 10, 74),
    bulgarian_solitaire([3000, 2050], 100, 7325)          
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl. 

test_only_odd_digits :-
    run_tests([
    only_odd_digits(1),
    only_odd_digits(999919999199991),
	only_odd_digits(135797531),
    \+ only_odd_digits(1354797531),
    \+ only_odd_digits(7717936191),
    \+ only_odd_digits(0),
    (findall(N, (between(1, 1000, N), only_odd_digits(N)), L), length(L, 155)),
    (findall(N, (between(1, 100000, N), only_odd_digits(N)), L), length(L, 3905))
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.          

test_josephus :-
    run_tests([
  	josephus([joe, moe, bob, rob, josephus], 2, bob),
    josephus([joe, moe, bob, rob, josephus], 99, josephus),
    (findall(N, between(1, 30, N), L), josephus(L, 4, 6)),
    (findall(N, between(1, 1000, N), L), josephus(L, 13, 396)),
    (findall(N, between(1, 10000, N), L), josephus(L, 77, 7373))
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_first_missing_positive :-
    run_tests([ 
	/* first_missing_positive */
	first_missing_positive([99999, 123, 1, 24, 5, 9999999, 222, 3, 4, 7777777, 2], 6),
	(findall(X, first_missing_positive([99, 4, 1, 3, 7, 2], X), L), L = [5]),
	(findall(Y, between(1, 1000, Y), LLL), reverse(LLL, LL), findall(X, first_missing_positive(LL, X), L), L = [1001]),
	first_missing_positive([-1, -2, -3, -4, -4, 0, 1], 2),
	first_missing_positive([1, 2, [3, 4]], 3)
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_riffle :-
    run_tests([
	riffle([1,2,3,4], [5,6,7,8], [1,5,2,6,3,7,4,8], left),
	riffle([1,2,3,4], [5,6,7,8], [5,1,6,2,7,3,8,4], right),
	(riffle([42, bob, 99], [55, jack, tom], [55|_], M), M = right),
	\+ riffle([11, 12, 13, 14], [1, 2, 3, 4, 5, 6], L, M),
	(findall(M, riffle([11, 12, 13, 14], [1, 2, 3, 4], L, M), Z), length(Z, Z2), Z2 = 2)
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_sz :-
    run_tests([
    sz(272, 77777777777777770000),
	(findall(S, sz(555, S), L), L = [7770]),
	(findall(S, sz(2727, S), L), L = [777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777]),
	sz(1, 7)      
     ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_crag :-
    run_tests([
    crag(5, 4, 5, 10),
	(findall(S, crag(3, 4, X, S), L), sort(L, [4, 5, 6, 8, 26])),
	(findall((A, B, C), crag(A, B, C, 26), L), length(L, 12)),
	\+ crag(6, 6, 6, 18),
	(findall((A, B, C), crag(A, B, C, 25), X), length(X, 6))          
     ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.          

test_count_dominators :-
    run_tests([
    count_dominators([], 0),
	count_dominators([33, 22, 11, 64, -2, 5], 2),
	(findall(X, between(1, 1000, X), L), reverse(L, LL), findall(D, count_dominators(LL, D), LD), LD = [1000]),
	(findall(L, (between(1, 5, X), between(1, 5, Y), L = [X, Y], count_dominators(L, 2)), V), length(V, 10)),
	count_dominators([[1,2,3]], 1)
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_running_median :-
    run_tests([
    running_median([99, 42, 17, 55, -4, 18, 77], [99, 42, 42, 42, 17, 18, 18]),
	(running_median([42, 42, 42, 42, 42, 42, 42], L), L = [42, 42, 42, 42, 42, 42, 42]),
	running_median([1,2,3,4,5,6], [1,2,2,3,4,5]),
	running_median([1, 1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1, 1]),
	running_median([A, B], [A, B])
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_safe_squares_rooks :-
    run_tests([
	safe_squares_rooks([(2, 2), (3, 1), (5, 5), (2, 5)], 5, 4),
	(findall((X, X), between(1, 50, X), L), safe_squares_rooks(L, 50, S), S = 0),
	safe_squares_rooks([(4,3), (2,2), (1,2)], 10, 56),
	safe_squares_rooks([(1, 1), (3, 1), (3, 2)], 5, 9),
	safe_squares_rooks([(1, 1), (2, 2), (3, 4)], 1000, 994009)
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.          

test_trick_winner :-
    run_tests([
    trick_winner([(five, spades), (queen, diamonds), (ace, spades), (ten, spades)], (ace, spades)),
	(findall(X, trick_winner([(six, spades), (two, hearts), (X, spades), (nine, clubs)], (six, spades)), L), length(L, 4)),
	(findall(X, trick_winner([(five, diamonds), X, (ten, hearts), (ten, diamonds)], X), L), length(L, 4)),
	\+ trick_winner([(seven, spades), (two, hearts), (six, spades), (nine, clubs)], (two, hearts)),
	(findall(X, trick_winner([(ace, S), (two, S), (six, S), (king, S)], X), Z), length(Z, 4))
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_sum_of_two_squares :-
    run_tests([
    \+ sum_of_two_squares(11, _, _),
    sum_of_two_squares(50, 7, 1),
    (X is 123^2 + 456^2, sum_of_two_squares(X, 456, 123)),
    (X is 555^2 + 666^2, sum_of_two_squares(X, 810, 309)),
    (findall(N, (between(1, 2000, N), sum_of_two_squares(N, _, _)), L), length(L, 591))
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_hitting_integer_powers :-
    run_tests([
    hitting_integer_powers(2, 7, 100, 73, 26),
    hitting_integer_powers(3, 6, 100, 137, 84),
    hitting_integer_powers(4, 5, 1000, 916, 789),
    hitting_integer_powers(10, 11, 1000, 1107, 1063),
    hitting_integer_powers(42, 51, 10000, 29546, 28087)
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_sum_of_distinct_cubes :-
    run_tests([
    sum_of_distinct_cubes(777777777, [919, 117, 29, 6]),
	(sum_of_distinct_cubes(123456789, L), L = [497, 88, 22, 8, 7, 6, 5]),
	(X is 10^16+1, sum_of_distinct_cubes(X, L), L = [215443, 4027, 139, 12, 10, 8, 5, 3]),
	sum_of_distinct_cubes(1, [1])
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.

test_fibonacci_sum :-
    run_tests([
    fibonacci_sum(10, [8, 2]),
    fibonacci_sum(42, [34, 8]),
    fibonacci_sum(100, [89, 8, 3]),
    fibonacci_sum(12345, [10946, 987, 377, 34, 1]),
    fibonacci_sum(665544332211, [591286729879, 53316291173, 20365011074, 433494437, 102334155, 39088169, 1346269, 28657, 6765, 1597, 34, 2]),
	(X is 10^100, fibonacci_sum(X, L), length(L, 137)),
	(X is 10^1000, fibonacci_sum(X, L), length(L, 1316)),
	fibonacci_sum(1, [1]),
	fibonacci_sum(58001746501815487425285,
                  [43566776258854844738105, 10284720757613717413913, 3928413764606871165730, 218922995834555169026, 2880067194370816120, 23416728348467685, 8944394323791464, 190392490709135, 72723460248141, 27777890035288, 4052739537881, 1548008755920, 86267571272, 4807526976, 1836311903, 701408733, 165580141, 63245986, 24157817, 514229, 196418, 46368, 10946, 4181, 1597, 233, 55, 21, 1])
	], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.