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

/* Define that a binary search tree is either nil or bst(Left, E, Right)
 * where Left and Right are the left and right subtrees and E is the
 * element in their common root node. */

/* Add the new element X into the empty tree. */

bst_add(nil, X, bst(nil, X, nil)).

/* Adding an existing element changes nothing in the tree. */

bst_add(bst(T1, X, T2), X, bst(T1, X, T2)).

/* Add the new element X into the left subtree. */

bst_add(bst(T1, R, T2), X, bst(T3, R, T2)) :- 
    X < R,
    !,
    bst_add(T1, X, T3).

/* Add the new element X into the right subtree. */

bst_add(bst(T1, R, T2), X, bst(T1, R, T3)) :-
    X > R,
    bst_add(T2, X, T3).

/* The predicate to test for tree membership. The metapredicates
 * var and nonvar are used to make the predicate more general so that
 * we can iterate over the members of the given BST with the general
 * query where X is unbound. */

/* Find the element in the left subtree. */

bst_contains(bst(T1, R, _), X) :-
    nonvar(X),
    X < R,
    bst_contains(T1, X).

bst_contains(bst(T1, _, _), X) :- 
    var(X),
    bst_contains(T1, X).

/* Find the element in the root. */

bst_contains(bst(_, X, _), X).

/* Find the element in the right subtree. */

bst_contains(bst(_, R, T2), X) :- 
    nonvar(R),
    nonvar(X),
    X > R,
    bst_contains(T2, X).

bst_contains(bst(_, _, T2), X) :- 
    var(X),
    bst_contains(T2, X).

/* The rules for finding the minimum element are trivial. The
 * rules for finding the maximum would be symmetric. */

bst_min(bst(nil, X, _), X) :- !.

bst_min(bst(T1, _, _), X) :-
    bst_min(T1, X).

/* Convert the list of elements into a BST by adding them one
 * at the time to an empty tree. Tail recursion with accumulator
 * does the job handily. */

bst_create(L, Result) :- /* top level */
    bst_create(L, nil, Result).

bst_create([], Result, Result). /* base case */

bst_create([H|T], SoFar, Result) :- /* general case */
    bst_add(SoFar, H, SoFar2),
    bst_create(T, SoFar2, Result).

/* Prolog does not have infinity for integers, so let us define one.
 * To emphasize that all names are meaningless and made up, we shall
 * call this infinity "bob" instead of "inf". */

lt(-bob, bob) :- !.
lt(-bob, X) :- integer(X), !.
lt(X, bob) :- integer(X), !.
lt(X, Y) :- X < Y.

/* Verify that T is a legal BST. The additional parameters Min and Max
 * remember the lower and upper bounds that the key in the current node
 * must be between. */

bst_verify(T) :- /* top level */
    bst_verify(T, -bob, bob).

bst_verify(nil, _, _). /* base case */

bst_verify(bst(T1, R, T2), Min, Max) :- /* general case */
    lt(Min, R),
    lt(R, Max),
    bst_verify(T1, Min, R),
    bst_verify(T2, R, Max).