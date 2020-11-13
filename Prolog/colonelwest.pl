criminal(X) :-
    american(X),
    weapon(Y),
    sells(X, Y, Z),
    hostile(Z).

owns(nono, m1).
owns(nono, tricycle).

missile(m1).

sells(west, X, nono) :-
    owns(nono, X),
    missile(X).

weapon(X) :-
    missile(X).

hostile(X) :-
    enemy(X, america).

american(west).

enemy(nono, america).