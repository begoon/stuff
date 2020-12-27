parent(alexander, tessa).
parent(alexander, danyil).
parent(olga, tessa).
parent(olga, danyil).

sibling(A, B) :- \+(A = B), parent(Z, A), parent(Z, B).
couple(A, B) :- \+(A = B), parent(A, Z), parent(B, Z).

family(F, M, []) :-
    couple(F, M).

family(F, M, [C|T]) :-
    couple(F, M),
    parent(F, C),
    parent(M, C),
    family(F, M, T).

child(C) :-
    parent(_, C).

children([]).

children([H|T]) :-
    child(H),
    children(T).
