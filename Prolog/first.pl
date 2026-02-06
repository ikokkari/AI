male(joe).
male(moe).
female(tina).

parent(joe, moe).
parent(joe, tina).
parent(tina, bob).

sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    not(X = Y).

ancestor(X, Y) :-
    parent(X, Y).

ancestor(X, Y) :-
    parent(X, Z),
    ancestor(Z, Y).
