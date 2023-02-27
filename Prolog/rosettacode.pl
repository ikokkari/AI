/* http://rosettacode.org/wiki/FizzBuzz */

fizzbuzz(Start, End, L) :-
        findall(Y, (between(Start, End, X), fizzbuzz_item(X, Y)), L).
 
fizzbuzz_item(X, Y) :-
        (  0 is X mod 15
        -> Y = 'FizzBuzz'
        ;  0 is X mod 3
        -> Y = 'Fizz'
        ;  0 is X mod 5
        -> Y = 'Buzz'
        ;  Y = X
        ).
 
/* http://rosettacode.org/wiki/Stack */

push(ELEMENT, STACK, [ELEMENT|STACK]).
 
pop([TOP|STACK], TOP, STACK).
 
empty([]).


/* http://rosettacode.org/wiki/Quaternion_type */

% A quaternion is represented as a complex term qx/4

add(qx(R0,I0,J0,K0), qx(R1,I1,J1,K1), qx(R,I,J,K)) :-
	!, R is R0+R1, I is I0+I1, J is J0+J1, K is K0+K1.
add(qx(R0,I,J,K), F, qx(R,I,J,K)) :-
	number(F), !, R is R0 + F.
add(F, qx(R0,I,J,K), Qx) :-
	add(qx(R0,I,J,K), F, Qx).
mul(qx(R0,I0,J0,K0), qx(R1,I1,J1,K1), qx(R,I,J,K)) :- !,
	R is R0*R1 - I0*I1 - J0*J1 - K0*K1,
	I is R0*I1 + I0*R1 + J0*K1 - K0*J1,
	J is R0*J1 - I0*K1 + J0*R1 + K0*I1,
	K is R0*K1 + I0*J1 - J0*I1 + K0*R1.
mul(qx(R0,I0,J0,K0), F, qx(R,I,J,K)) :-
	number(F), !, R is R0*F, I is I0*F, J is J0*F, K is K0*F.
mul(F, qx(R0,I0,J0,K0), Qx) :-
	mul(qx(R0,I0,J0,K0),F,Qx).
abs(qx(R,I,J,K), Norm) :-
	Norm is sqrt(R*R+I*I+J*J+K*K).
negate(qx(Ri,Ii,Ji,Ki),qx(R,I,J,K)) :-
	R is -Ri, I is -Ii, J is -Ji, K is -Ki.
conjugate(qx(R,Ii,Ji,Ki),qx(R,I,J,K)) :-
	I is -Ii, J is -Ji, K is -Ki.

/* http://rosettacode.org/wiki/Roman_numerals/Decode */

decode_digit(i, 1).
decode_digit(v, 5).
decode_digit(x, 10).
decode_digit(l, 50).
decode_digit(c, 100).
decode_digit(d, 500).
decode_digit(m, 1000).
 
decode_string(Atom, Value) :-
   atom_chars(Atom, String),
   reverse(String, [Last|Rest]),
   decode_digit(Last, Start),
   decode_string(Start, Start, Rest, Value).

decode_string(Sum, _, [], Sum).
 
decode_string(LastSum, LastValue, [Digit|Rest], NextSum) :-
   decode_digit(Digit, Value),
   Value < LastValue,
   Sum is LastSum - Value,
   decode_string(Sum, Value, Rest, NextSum).
 
decode_string(LastSum, LastValue, [Digit|Rest], NextSum) :-
   decode_digit(Digit, Value),
   Value >= LastValue,
   Sum is LastSum + Value,
   decode_string(Sum, Value, Rest, NextSum).

/* 

decode_string(LastSum, LastValue, [Digit|Rest], NextSum) :-
   decode_digit(Digit, Value),
   (   	Value < LastValue ->
	   	Sum is LastSum - Value;
   		Sum is LastSum + Value
   ),
   decode_string(Sum, Value, Rest, NextSum).

*/
 
/* http://rosettacode.org/wiki/Van_Eck_sequence */

van_eck_init(v(0, 0, _assoc)):-
    empty_assoc(_assoc).
 
van_eck_next(v(Index, Last_term, Last_pos), v(Index1, Next_term, Last_pos1)):-
    (get_assoc(Last_term, Last_pos, V) ->
        Next_term is Index - V
        ;
        Next_term = 0
    ),
    Index1 is Index + 1,
    put_assoc(Last_term, Last_pos, Index, Last_pos1).
 
van_eck_sequence(N, Seq):-
    van_eck_init(V),
    van_eck_sequence(N, V, Seq).
 
van_eck_sequence(0, _, []):-!.
van_eck_sequence(N, V, [Term|Rest]):-
    V = v(_, Term, _),
    van_eck_next(V, V1),
    N1 is N - 1,
    van_eck_sequence(N1, V1, Rest).

/* http://rosettacode.org/wiki/Sorting_algorithms/Quicksort */

qsort( [], [] ).
qsort( [H|U], S ) :-
    splitBy(H, U, L, R),
    qsort(L, SL),
    qsort(R, SR),
    append(SL, [H|SR], S).
 
% splitBy( H, U, LS, RS )
% True if LS = { L in U | L <= H }; RS = { R in U | R > H }

splitBy( _, [], [], []) :- !.
splitBy( H, [U|T], [U|LS], RS ) :-
    U =< H,
    !,
    splitBy(H, T, LS, RS).
splitBy( H, [U|T], LS, [U|RS] ) :-
    U > H,
    splitBy(H, T, LS, RS).

/* https://rosettacode.org/wiki/Babbage_problem */

:- use_module(library(clpfd)).

babbage_(B, B, Sq) :- 
	B * B #= Sq, 
	number_chars(Sq, R), 
	append(_, ['2','6','9','6','9','6'], R).
babbage_(B, R, Sq) :- 
	N #= B + 1,
	babbage_(N, R, Sq).
	
babbage :- 
	once(babbage_(1, Num, Square)), 
	format('lowest number is ~p which squared becomes ~p~n', [Num, Square]).

/* https://rosettacode.org/wiki/Rot-13 */

rot13(Str, SR) :-
    string_chars(Str, Sc),
	maplist(rot, Sc, Sc1),
	string_to_list(SR, Sc1).

rot(C, C1) :-
	(   member(C, "abcdefghijklmABCDEFGHIJKLM") -> C1 is C+13;
	    (	member(C, "nopqrstuvwxyzNOPQRSTUVWXYZ") -> C1 is C-13; C1 = C)).

/* https://rosettacode.org/wiki/Look-and-say_sequence */

look_and_say(L) :-
	maplist(write, L), nl,
	encode(L, L1),
	look_and_say(L1).

% This code is almost identical to the code of "run-length-encoding" 
encode(In, Out) :-
	packList(In, R1),
	append(R1,Out).


% use of library clpfd allows packList(?In, ?Out) to works
% in both ways In --> Out and In <-- Out.

:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ?- packList([a,a,a,b,c,c,c,d,d,e], L).
%  L = [[3,a],[1,b],[3,c],[2,d],[1,e]] .
% ?- packList(R,  [[3,a],[1,b],[3,c],[2,d],[1,e]]).
% R = [a,a,a,b,c,c,c,d,d,e] .
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
packList([],[]).

packList([X],[[1,X]]) :- !.


packList([X|Rest],[XRun|Packed]):-
    run(X,Rest, XRun,RRest),
    packList(RRest,Packed).


run(Var,[],[1,Var],[]).

run(Var,[Var|LRest],[N1, Var],RRest):-
    N #> 0,
    N1 #= N + 1,
    run(Var,LRest,[N, Var],RRest).

run(Var,[Other|RRest], [1,Var],[Other|RRest]):-
    dif(Var,Other).

/* https://rosettacode.org/wiki/Morse_code */

text2morse(Text, Morse) :-
	string_lower(Text, TextLower),			% rules are in lower case
	string_chars(TextLower, Chars),			% convert string into list of chars
	chars2morse(Chars, MorseChars),			% convert each char into morse
	string_chars(MorsePlusSpace, MorseChars),	% append returned string list into single string
	string_concat(Morse, ' ', MorsePlusSpace).	% Remove trailing space

chars2morse([], "").
chars2morse([H|CharTail], Morse) :-
	morse(H, M),
	chars2morse(CharTail, MorseTail),
	string_concat(M,' ', MorseSpace),
	string_concat(MorseSpace, MorseTail, Morse).

% space
morse(' ', " ").
% letters
morse('a', ".-").
morse('b', "-...").
morse('c', "-.-.").
morse('d', "-..").
morse('e', ".").
morse('f', "..-.").
morse('g', "--.").
morse('h', "....").
morse('i', "..").
morse('j', ".---").
morse('k', "-.-").
morse('l', ".-..").
morse('m', "--").
morse('n', "-.").
morse('o', "---").
morse('p', ".--.").
morse('q', "--.-").
morse('r', ".-.").
morse('s', "...").
morse('t', "-").
morse('u', "..-").
morse('v', "...-").
morse('w', ".--").
morse('x', "-..-").
morse('y', "-.--").
morse('z', "--..").
% numbers
morse('1', ".----").
morse('2', "..---").
morse('3', "...--").
morse('4', "....-").
morse('5', ".....").
morse('6', "-....").
morse('7', "--...").
morse('8', "---..").
morse('9', "----.").
morse('0', "-----").
% common punctuation
morse('.', ".-.-.-").
morse(',', "--..--").
morse('/', "-..-.").
morse('?', "..--..").
morse('=', "-...-").
morse('+', ".-.-.").
morse('-', "-....-").
morse('@', ".--.-.").


/* https://rosettacode.org/wiki/Guess_the_number/With_feedback */

main :-
    play_guess_number.


/* Parameteres */

low(1).
high(10).


/* Basic Game Logic */

play_guess_number :-
    low(Low),
    high(High),
    random(Low, High, N),
    tell_range(Low, High),
    repeat,                         % roughly, "repeat ... (until) Guess == N "
        ask_for_guess(Guess),
        give_feedback(N, Guess),
    Guess == N.

/* IO Stuff */

tell_range(Low, High) :-
    format('Guess an integer between ~d and ~d.~n', [Low,High]).

ask_for_guess(Guess) :-
    format('Guess the number: '),
    read(Guess).

give_feedback(N, Guess) :-
    ( \+integer(Guess) -> writeln('Invalid input.')
    ; Guess < N        -> writeln('Your guess is too low.')
    ; Guess > N        -> writeln('Your guess is too high.')
    ; Guess =:= N      -> writeln("Correct!")
    ).

/* https://rosettacode.org/wiki/Towers_of_Hanoi */

hanoi(N) :- move(N,left,center,right).

move(0,_,_,_) :- !.
move(N,A,B,C) :-
    M is N-1,
    move(M,A,C,B),
    inform(A,B),
    move(M,C,B,A).

inform(X,Y) :- write([move,a,disk,from,the,X,pole,to,Y,pole]), nl.

/* http://rosettacode.org/wiki/Fractal_tree */

fractal :-
	new(D, window('Fractal')),
	send(D, size, size(800, 600)),
	drawTree(D, 400, 500, -90, 9),
	send(D, open).
 
drawTree(_D, _X, _Y, _Angle, 0).
 
drawTree(D, X1, Y1, Angle, Depth) :-
    X2 is X1 + cos(Angle * pi / 180.0) * Depth * 10.0,
    Y2 is Y1 + sin(Angle * pi / 180.0) * Depth * 10.0,
	new(Line, line(X1, Y1, X2, Y2, none)),
	send(D, display, Line),
	A1 is Angle - 30,
	A2 is Angle + 30,
	De is Depth - 1,
        drawTree(D, X2, Y2, A1, De),
        drawTree(D, X2, Y2, A2, De).
