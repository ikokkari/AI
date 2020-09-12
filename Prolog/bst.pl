/* Since Prolog expressions are just a bunch of nodes pointing to each
 * other in memory, expressions can grow very large without running out
 * of memory. Entire data structures can be stored as single expressions.
 * To illustrate this idea, here are some predicates to perform binary
 * search tree operations. For simplicity, element removal and tree
 * rebalancing operations have been left out. */

/* Since Prolog does not have destructive assignment, all data structures
 * are immutable. However, tree structures can and will automatically
 * share their identical subtrees. The operation to add key X to the
 * given BST creates a new BST otherwise identical to the previous tree
 * except along the insertion path. */

bst_add(nil, X, tr(nil, X, nil)) :- !.
bst_add(tr(T1, X, T2), X, tr(T1, X, T2)).
bst_add(tr(T1, R, T2), X, tr(T3, R, T2)) :- 
    X < R,
    !,
    bst_add(T1, X, T3).
bst_add(tr(T1, R, T2), X, tr(T1, R, T3)) :-
    X > R,
    bst_add(T2, X, T3).

/* The predicates to test element containment. The metapredicates
 * var and nonvar are used to make the predicate more general. */

bst_contains(tr(T1, R, _), X) :- 
    nonvar(R),
    nonvar(X),
    X < R,
    bst_contains(T1, X).
bst_contains(tr(T1, _, _), X) :- 
    var(X),
    bst_contains(T1, X).
bst_contains(tr(_, X, _), X).
bst_contains(tr(_, R, T2), X) :- 
    nonvar(R),
    nonvar(X),
    X > R, bst_contains(T2, X).
bst_contains(tr(_, _, T2), X) :- 
    var(X),
    bst_contains(T2, X).

/* The rules for finding the minimum element are trivial. */

bst_min(tr(nil, X, _), X) :- !.
bst_min(tr(T1, _, _), X) :-
    bst_min(T1, X).

/* Accumulators strike again with BST creation. For convenience,
 * a predicate to create a BST that contains all keys from L,
 * inserting these keys in the order that they appear in L. */

bst_create(L, Result) :- /* top level */
    bst_create(L, Result, nil).

bst_create([], Result, Result). /* base case */

bst_create([H|T], Result, SoFar) :- /* general case */
    bst_add(SoFar, H, SoFar2),
    bst_create(T, Result, SoFar2).

/* Prolog does not have infinity for integers, so let us define one.
 * To emphasize that all names are meaningless and made up, we shall
 * call this infinity "bob" instead of "inf". */

lt(-bob, X) :- integer(X), !.
lt(X, bob) :- integer(X), !.
lt(X, Y) :- X < Y.

/* Verify that T is a legal BST. The additional parameters Min and Max
 * remember the lower and upper bounds that the key in the current node
 * must be between. */

bst_verify(T) :- /* top level */
    bst_verify(T, -inf, inf).

bst_verify(nil, _, _). /* base case */

bst_verify(tr(T1, X, T2), Min, Max) :- /* general case */
    lt(Min, X),
    lt(X, Max),
    bst_verify(T1, Min, X),
    bst_verify(T2, X, Max).