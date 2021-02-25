my_repeat.

my_repeat :-
    my_repeat.

my_not(P) :-
    call(P),
    !,
    fail.

my_not(_).

my_max(X, Y, X) :-
    X >= Y,
    !.

my_max(X, Y, Y) :-
    X < Y.

my_plus(X, Y, Z) :-
    var(X),
    !,
    X is Z - Y.

my_plus(X, Y, Z) :-
    var(Y),
    !,
    Y is Z - X.

my_plus(X, Y, Z) :-
    Z is X + Y.