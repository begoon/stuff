parent(sasha, tessa).
parent(sasha, danila).
parent(olga, tessa).
parent(olga, danila).

sibling(A, B) :- \+(A = B), parent(Z, A), parent(Z, B).
couple(A, B) :- \+(A = B), parent(A, Z), parent(B, Z).

