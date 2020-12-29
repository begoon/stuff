% https://rextester.com/l/prolog_online_compiler

parent(alexander, tessa).
parent(alexander, danyil).
parent(olga, tessa).
parent(olga, danyil).

sibling(A, B) :- parent(Z, A), parent(Z, B), \+(A = B).
couple(A, B) :- parent(A, Z), parent(B, Z), \+(A = B).

family(F, M, []) :-
    couple(F, M).

family(F, M, [C|T]) :-
    couple(F, M),
    parent(F, C),
    parent(M, C),
    family(F, M, T).

child(C) :-
    parent(_, C).
    
q1 :- 
    family(alexander, olga, [tessa, danyil]),
    write('family1'), nl,
    family(olga, alexander, [tessa, danyil]),
    write('family2'), nl.

q2 :- 
    family(A, olga, [tessa, danyil]),
    write([A]), nl.

q3 :- 
    family(A, olga, [B, C]),
    write([A, B, C]), nl.


:- q1, q2, q3.
