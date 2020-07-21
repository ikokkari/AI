/* Prolog programming project from the year 2018 of this course. 
 * Write predicates to recognize and generate poker hands of given
 * type, using no numbers but symbolic literals only. */

suits([clubs, diamonds, hearts, spades]).
ranks([two, three, four, five, six, seven,
          eight, nine, ten, jack, queen, king, ace]).
suit(X) :- suits(S), member(X, S).
rank(X) :- ranks(S), member(X, S).

/* Individual playing cards are expressed as terms (rank, suit). */

card((R, S)) :- rank(R), suit(S).

/* Literals for ranks are organized in a total order. */

rsucc(two, three).
rsucc(three, four).
rsucc(four, five).
rsucc(five, six).
rsucc(six, seven).
rsucc(seven, eight).
rsucc(eight, nine).
rsucc(nine, ten).
rsucc(ten, jack).
rsucc(jack, queen).
rsucc(queen, king).
rsucc(king, ace).

/* Recursive rules to generate all lower suits and ranks. */

lower_suit(S1, S2) :-
    suits(S),
    lower_in(S1, S2, S).

lower_rank(R1, R2) :-
    ranks(S),
    lower_in(R1, R2, S).

lower_in(X, Y, [X|S]) :- member(Y, S).
lower_in(X, Y, [_|S]) :- lower_in(X, Y, S).

lower_card((R1, S1), (R2, S2)) :-
    lower_rank(R1, R2),
    card((R1, S1)),
    card((R2, S2)).

lower_card((R, S1), (R, S2)) :-
    lower_suit(S1, S2).

sublist([], []).
sublist([], [_|_]).
sublist([X|T1], [X|T2]) :- sublist(T1, T2).
sublist(T1, [_|T2]) :- sublist(T1, T2).

/* A predicate that accepts and generates sorted hands of cards. All
 * following poker hand predicates may accept and generate only sorted
 * hands, as enforced by this predicate. */

sorted_hand([], 0).
sorted_hand([C], 1) :- card(C).
sorted_hand([C1, C2 | T], Len) :-
    Len >= 2,
    lower_card(C2, C1),
    plus(L, 1, Len),
    sorted_hand([C2|T], L).

/* Predicates to recognize types of five card poker hands. Prolog
 * pattern matching truly shines in writing these predicates. */

/* Two rules for four-of-kind depending on whether the fifth card
 * is lower or higher ranking than the four equal ranked cards. */

four_of_kind([(R, spades),(R, hearts),(R, diamonds),(R, clubs),(R2, S)]) :-
    rank(R),
    lower_card((R2, S), (R, clubs)).

four_of_kind([(R2, S), (R, spades),(R, hearts),(R, diamonds),(R, clubs)]) :-
    rank(R),
    lower_card((R, spades), (R2, S)).

/* Similarly, two symmetric rules are needed to recognize a full house. */

full_house([(R, S1), (R, S2), (R, S3), (R2, S4), (R2, S5)]) :-
    sorted_hand([(R, S1), (R, S2), (R, S3), (R2, S4), (R2, S5)], 5).

full_house([(R2, S1), (R2, S2), (R, S3), (R, S4), (R, S5)]) :-
    sorted_hand([(R2, S1), (R2, S2), (R, S3), (R, S4), (R, S5)], 5).

/* Prolog pattern matching can handle flushes and straights easy peasy. */

has_flush([(R1, S), (R2, S), (R3, S), (R4, S), (R5, S)]) :-
    sorted_hand([(R1, S), (R2, S), (R3, S), (R4, S), (R5, S)], 5).

has_straight([(R1,S1), (R2,S2), (R3,S3), (R4,S4), (R5,S5)]) :-
    rsucc(R5, R4),
    rsucc(R4, R3),
    rsucc(R3, R2),
    rsucc(R2, R1),
    suit(S1),
    suit(S2),
    suit(S3),
    suit(S4),
	suit(S5).

/* The "bicycle straight" has to be handled with a separate rule. */

has_straight([(ace, S1), (five, S2), (four, S3), (three, S4), (two, S5)]) :-
    suit(S1),
    suit(S2),
    suit(S3),
    suit(S4),
    suit(S5).

/* Flushes and straights need to be distinguished from straight flushes.
 * The use of not seems slightly impure in general. */

flush(H) :-
    has_flush(H),
    not(has_straight(H)).

straight(H) :-
    has_straight(H),
    not(has_flush(H)).

straight_flush(H) :-
    has_straight(H),
    has_flush(H).

/* Three rules are needed to recognize three of a kind, depending on
 * how the ranks of the other two cards place around the triple rank. */

three_of_kind([(R, S1), (R, S2), (R, S3), (R2, S4), (R3, S5)]) :-
    lower_rank(R2, R),
    lower_rank(R3, R2),
    sorted_hand([(R, S1), (R, S2), (R, S3), (R2, S4), (R3, S5)], 5).

three_of_kind([(R2, S4), (R, S1), (R, S2), (R, S3), (R3, S5)]) :-
    lower_rank(R, R2),
    lower_rank(R3, R),
    sorted_hand([(R2, S4), (R, S1), (R, S2), (R, S3), (R3, S5)], 5).

three_of_kind([(R2, S4), (R3, S5), (R, S1), (R, S2), (R, S3)]) :-
    lower_rank(R3, R2),
    lower_rank(R, R3),
    sorted_hand([(R2, S4), (R3, S5), (R, S1), (R, S2), (R, S3)], 5).

/* Same for the three possibilities of the fifth card in two pair. */

two_pair([(R1, S1), (R1, S2), (R2, S3), (R2, S4), (R3, S5)]) :-
    lower_rank(R2, R1),
    lower_rank(R3, R2),
    sorted_hand([(R1, S1), (R1, S2), (R2, S3), (R2, S4), (R3, S5)], 5).

two_pair([(R1, S1), (R1, S2), (R3, S5), (R2, S3), (R2, S4)]) :-
    lower_rank(R3, R1),
    lower_rank(R2, R3),
    sorted_hand([(R1, S1), (R1, S2), (R3, S5), (R2, S3), (R2, S4)], 5).

two_pair([(R3, S5), (R1, S1), (R1, S2), (R2, S3), (R2, S4)]) :-
    lower_rank(R1, R3),
    lower_rank(R2, R1),
    sorted_hand([(R3, S5), (R1, S1), (R1, S2), (R2, S3), (R2, S4)], 5).

/* A helper predicate to check that hand contains no pairs. */

no_pairs([], 0).
no_pairs([C], 1) :-
    card(C).
no_pairs([(R1, _), (R2, S2) | T], L) :-
    lower_rank(R2, R1),
    plus(L2, 1, L),
    no_pairs([(R2, S2) | T], L2).

only_one_pair([(R, S1), (R, S2) | T], L) :-
    lower_card((R, S2), (R, S1)),
    plus(L2, 1, L),
    no_pairs([(R, S2) | T], L2).

only_one_pair([(R1, S1), (R2, S2) | T], L) :-
    lower_rank(R2, R1), 
    lower_card((R2, S2), (R1, S1)),
    plus(L2, 1, L),
    only_one_pair([(R2, S2) | T], L2).

one_pair(H) :-
    only_one_pair(H, 5),
    sorted_hand(H, 5).

/* The test predicate seen earlier in homoiconic.pl file. */

test(Query, X, Inf, Res, Test) :-
    statistics(inferences, I1),
    findall(X, Query, Res),
    statistics(inferences, I2),
    Inf is I2 - I1,
    call(Test) -> 
    	(write('success '), write(Inf), nl, !) ;
    	(write('failure '), write(Res), nl, fail).
test(_, _, 0, _, _).

/* Three sets of poker hand test cases. We can now solve all kinds of
 * combinatorial questions about possible poker hands under interesting
 * constraints. These might make a set of homework exercises for some
 * course on discrete math and combinatorics. */

test_all1(Inf) :-
    write('1. Card comparisons: '),
    /* How many cards are between eight of spades and jack of diamonds? */
	test((lower_card(C, (jack, diamonds)), lower_card((eight, spades), C)),
         C, Inf1, R1, length(R1, 9)),
    
    write('2. Sorting your hand: '),
    /* How many sorted five-card poker hands have queen of hearts and then
     * the six of diamonds as the fourth card? */ 
    test((H1 = [_, _, _, (six, diamonds), _], sorted_hand(H1, 5), member((queen, hearts), H1)),
         H1, Inf2, R2, length(R2, 8976)),
    
    write('3. Four of a kind: '),
    /* How many four of kind hands contain the jack of diamonds? */
    test((four_of_kind(H2), member((jack, diamonds), H2)), H2, Inf3, R3, length(R3, 60)),
    
    write('4. Full house: '),
    /* How many full houses have a diamond as second card, and jack of spades as third? */
    test((H3=[_, (_, diamonds), (jack, spades), _, _], full_house(H3)), H3, Inf4, R4,
         length(R4, 18)), 
    
    write('5. Flush: '),
    /* How many flushes have a ten as second card, and a six as a third card? */
    test((H4=[_, (ten, _), (six, _), _, _], flush(H4)), H4, Inf5, R5, length(R5, 96)),
    
    write('6. Straight: '),
    /* How many straights start and end with a diamond? */
    test((H5=[(_,diamonds),_,_,_,(_, diamonds)], straight(H5)), H5, Inf6, R6, length(R6, 630)),
    
    write('7. Straight flush: '),
    /* How many straight flushes do not contain an eight? */
    test((straight_flush(H6), not(member((eight, _), H6))), H6, Inf7, R7, length(R7, 20)),
    
    write('8. Three of a kind: '),
    /* How many three of a kind hands do not contain any spades? */
    test((three_of_kind(H7), not(member((_, spades), H7))), H7, Inf8, R8, length(R8, 7722)),
    
    write('9. One pair: '),
    /* How many hands that have one pair have the suit pattern HSHSH? */
    test((H8=[(_,hearts), (_,spades), (_,hearts), (_, spades), (_, hearts)], one_pair(H8)),
         H8, Inf9, R9, length(R9, 1430)),
    
    write('10. Two pair: '),
    /* How many sorted two pair hands have the suit pattern C*C*H ? */
    test((H9 = [(_, clubs),(_, _),(_, clubs),(_, _),(_, hearts)], two_pair(H9)), H9, Inf10,
         R10, length(R10, 858)),
    
    /* Total inferences */
    Inf is Inf1 + Inf2 + Inf3 + Inf4 + Inf5 + Inf6 + Inf7 + Inf8 + Inf9 + Inf10.

test_all2(Inf) :-
    write('1. Card comparisons: '),
    /* How many sorted hands contain at least one card from each suit? */
	test((sorted_hand(H1,5), memberchk((_,spades),H1), memberchk((_,hearts),H1), memberchk((_,diamonds),H1), (memberchk((_,clubs),H1))),
         H1, Inf1, R1, length(R1, 685464)),
    
    write('2. No pairs: '),
    /* How many hands made of cards lower than nine don't contain any pairs? */
    test((lower_rank(R, nine),H11=[(R,_)|_],sorted_hand(H11, 5), not(one_pair(H11) ; two_pair(H11) ; three_of_kind(H11) ; four_of_kind(H11) ; full_house(H11))),
         H11, Inf2, R2, length(R2, 21504)),
    
    write('3. Sorted hand: '),
    /* No sorted hand contains its first card later again. */
    test((sorted_hand([H2|T],5), member(H2, T)), H2, Inf3, R3, length(R3, 0)),
    
    write('4. Full house: '),
    /* How many full houses have seven of hearts as the middle card? */
    test((H3=[_, _, (seven, hearts), _, _], full_house(H3)), H3, Inf4, R4,
         length(R4, 42)), 
    
    write('5. Flush: '),
    /* How many inferences are needed to find out that a flush can't start and end with different suits ? */
    test((HH = [(_,spades),_,_,_,(_,clubs)], flush(HH)),HH,Inf5,R5,length(R5, 0)),
    
    write('6. Straight: '),
    /* How many straights start with jack of spades? */
    test((H5=[(jack,spades),_,_,_,_], straight(H5)), H5, Inf6, R6, length(R6, 255)),
    
    write('7. Four of a kind: '),
    /* How many four of a kinds do not contain an eight? */
    test((four_of_kind(H6), not(member((eight, _), H6))), H6, Inf7, R7, length(R7, 528)),
    
    write('8. Three of a kind: '),
    /* How many three of a kinds have some higher and some lower additional card? */
    test( (H7 = [_,(R,_),(R,_),(R,_),_], three_of_kind(H7)), H7, Inf8, R8, length(R8, 18304)),
    
    write('9. One pair: '),
    /* No one pair is also two pair. */
    test((H8=[(_,diamonds),_,_,_,_],one_pair(H8), two_pair(H8)),
         H8, Inf9, R9, length(R9, 0)),
    
    write('10. High card: '),
    /* Using only cards lower than ten, how many hands have nothing better than a high card? */
    test((lower_rank(R, ten), X = [(R,_)|_],sorted_hand(X, 5), not(one_pair(X) ; two_pair(X) ; three_of_kind(X) ;
                                                                   four_of_kind(X) ; full_house(X) ; flush(X) ; straight(X) ;
                                                                   straight_flush(X))), X, Inf10,
         R10, length(R10, 53040)),
    
    /* Total inferences */
    Inf is Inf1 + Inf2 + Inf3 + Inf4 + Inf5 + Inf6 + Inf7 + Inf8 + Inf9 + Inf10.


test_all3(Inf) :-
    write('1. Lower card: '),
    /* How many cards are lower than seven of hearts? */
    test((lower_card(C, (seven, hearts))), C, Inf1, R1, length(R1, 22)),
    
    write('2. Sorted four card hands: '),
    /* How many sorted 4-card hands have some nine as second card and some two as last card? */
    test((H1 = [_,(nine,_), _, (two,_)], sorted_hand(H1, 4)), H1, Inf2, R2, length(R2, 9268)),
    
    write('3. Impossible sorted hand: '),
    /* No sorted five-card hand has the same rank as its first and last card. */
    test((H2 = [(R,_),_,_,_,(R,_)], sorted_hand(H2, 5)), H2, Inf3, R3, length(R3, 0)),
    
    write('4. Four of a kind: '),
    /* How many four of a kinds have three lucky sevens in the middle? */
    test((H3 = [_, (seven,_), (seven,_), (seven,_),_], four_of_kind(H3)), H3, Inf4, R4, 
         length(R4, 48)),
    
    write('5. Full house: '),
    /* How many full houses have three lucky sevens in the middle? None! */
    test((H4 = [_, (seven,_), (seven,_), (seven,_),_], full_house(H4)), H4, Inf5, R5, 
         length(R5, 0)),
    
    write('6. Straight: '),
    /* How many straights have a spade as both second and last card, and also a four somewhere? */
    test(((X1 = four ; X2 = four; X3 = four; X4 = four; X5 = four),
             H5 = [(X1,_),(X2,spades),(X3,_),(X4,_),(X5,spades)], straight(H5)),
         H5, Inf6, R6, length(R6, 252)),
    
    write('7. Flush: '),
    /* How many club flushes exist when using only cards with nominal ranks below ten? */
    test((lower_rank(C, ten), H6 = [(C, clubs), _, _, _, _], flush(H6)), H6, Inf7, R7, length(R7, 52)),
    
    write('8. Three of a kind: '),
    /* Without using any spades, how many three of a kind hands exist? */
    test((lower_suit(S1, spades), lower_suit(S2, spades), lower_suit(S3, spades), lower_suit(S4, spades),
             lower_suit(S5, spades), H7 = [(_,S1),(_,S2),(_,S3),(_,S4),(_,S5)], three_of_kind(H7)),
         H7, Inf8, R8, length(R8, 7722)),
    
    write('9. Two pair: '),
    /* Using only cards higher than eight, how many two pair hands exist? */
    test((lower_rank(seven, X), H8 = [_,_,_,_,(X,_)], two_pair(H8)), H8, Inf9, R9, length(R9, 15120)),
    
    write('10. One pair: '),
    /* How many one pair hands contain only black cards? */
    test((B = [spades, clubs], member(X1, B), member(X2, B), member(X3, B), member(X4, B), member(X5, B),
           H9 = [(_,X1),(_,X2),(_,X3),(_,X4),(_,X5)], one_pair(H9)  
         ), H9, Inf10, R10, length(R10, 22880)),
    
    write('11. Straight flush: '),
    /* How many straight flushes contain a seven? */
    test((straight_flush(H10), member((seven, _), H10)), H10, Inf11, R11, length(R11, 20)),
    
    write('12. Pairs of kings: '),
    /* How many pair of kings hands have eight of spades as lowest card? */
    test((H11 = [(king, _), (king, _), _, _, (eight, spades)],
             one_pair(H11)), H11, Inf12, R12, length(R12, 576)),
    
    write('13. Flushy pair: '),
    /* No spade flush can also be a one pair hand. */
    test((H12 = [(_, spades), (_, spades), (_, spades), (_, spades), (_, spades)], one_pair(H12)),
         H12, Inf13, R13, length(R13, 0)),
    
    write('14. Full house: '),
    /* How many full houses start with jack of diamonds? */
    test((H13 = [(jack, diamonds), _, _, _, _], full_house(H13)), H13, Inf14, R14, length(R14, 36)),
    
    write('15. Four by four: '),
    /* How many four of kinds can be appended to another four to create a sorted 10-card hand? */
    test((X1=[_, _, _, _, C1], four_of_kind(X1), lower_card(C2, C1), X2 = [C2, _, _, _, _],
             four_of_kind(X2), append(X1, X2, H14), sorted_hand(H14, 10)),
         H14, Inf15, R15, length(R15, 47476)),
    
    Inf is Inf1 + Inf2 + Inf3 + Inf4 + Inf5 + Inf6 + Inf7 + Inf8 + Inf9 
    + Inf10 + Inf11 + Inf12 + Inf13 + Inf14 + Inf15.


