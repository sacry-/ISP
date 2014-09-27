
male(bob).
male(hans).
male(heinrich).
male(klaus).
male(jeremie).

female(mary).
female(judy).
female(sabine).
female(emma).
female(foxie).

parent(bob,judy).
parent(bob,heinrich).
parent(mary,judy).
parent(mary,heinrich).

parent(hans,klaus).
parent(sabine,klaus).
parent(hans,foxie).
parent(sabine,foxie).

parent(judy,emma).
parent(judy,jeremie).
parent(klaus,emma).
parent(klaus,jeremie).

mother(X,Y) :- parent(X,Y),female(X).
father(X,Y) :- parent(X,Y),male(X).

child(X,Y) :- parent(Y,X).
son(X,Y) :- child(X,Y), male(X).
daughter(X,Y) :- child(X,Y), female(X).

married(X,Y) :- 
	parent(X,Child), 
	parent(Y,Child), 
	X \= Y.

grandfather(X,Y) :- father(X,Z),parent(Z,Y).
grandmother(X,Y) :- mother(X,Z),parent(Z,Y).

grandparent(X,Z,Y) :-
	grandfather(X,Y),
	grandmother(Z,Y),
	married(X,Z).

granddaughter(Y,X) :-
	daughter(Y,M), parent(X,M).

grandson(Y,X) :-
	son(Y,M), parent(X,M).

motherInLaw(X,Y) :-
	mother(X,Z),
	married(Z,Y).

fatherInLaw(X,Y) :-
	father(X,Z),
	married(Z,Y).

sibling(X,Y) :- 
	mother(M,X),father(F,X),
	mother(M,Y),father(F,Y),
	X\=Y.

uncle(X,Y) :-
	sibling(X,Z),
	parent(Z,Y),
	male(X).

aunt(X,Y) :-
	parent(Z,Y),
	sibling(Z,X),
	female(X).


