parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

female(pam).
female(liz).
female(ann).
female(pat).
male(tom).
male(bob).
male(jim).

offspring(Y, X) :- parent(X, Y).

mother(X, Y) :-
    parent(X, Y),
    female(X).

grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).

sister(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    female(X),
    X \= Y.

% Exercises 1.3
happy(X) :- parent(X, _).

hastwochildren(X) :-
    parent(X, Y),
    parent(X, Z),
    (female(Y) ; female(Z)),
    Y \= Z.

% Exercises 2.2
point(_X,_Y).
rectangle(point(_X1,_Y1), point(_X2,_Y2), point(_X3,_Y3), point(_X4,_Y4)).
square(point(_X1,_Y1), point(_X2,_Y2), point(_X3,_Y3), point(_X4,_Y4)).
circel(point(_X,_Y), _Radius).

% Exercises 2.5
same_x(point(_X1,_), point(_X2,_)).
same_y(point(_,_Y1), point(_,_Y2)).
regular(rectangle2(P1,P2,P3,P4)) :-
    same_x(P1,P2),
    same_x(P2,P3),
    same_y(P1,P4),
    same_y(P2,P3).
