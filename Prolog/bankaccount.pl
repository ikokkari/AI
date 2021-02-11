/* A bank account is represented with term of form 
 * acc(Owner, Balance), and methods are predicates
 * that produce the new object from the old, usually
 * sharing a lot of term subtrees. */

acc_construct(Owner, acc(Owner, 0)).

acc_deposit(acc(Owner, Balance), Amount, acc(Owner, NewBalance)) :-
    NewBalance is Balance + Amount.

acc_withdraw(acc(Owner, Balance), Amount, acc(Owner, NewBalance)) :-
    Amount =< Balance,
    NewBalance is Balance - Amount.

acc_withdraw(acc(Owner, Balance), Amount, acc(Owner, Balance)) :-
    Amount > Balance.
