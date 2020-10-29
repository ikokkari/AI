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
