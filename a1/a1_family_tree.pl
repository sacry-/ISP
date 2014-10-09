
/**

bob|-----------|mary      	hans|-------------|sabine            elvira
     |       |                    |         |                      |
  heinrich  judy|--------------|klaus     foxie|-----|horst|-----|agne (test-tube-baby)
                  |          |                    |           |
                emma       jeremie              dieter      hubert|---------|kloeten
                                        															 |
                                                                     loeten
						   
**/

male(bob).
male(hans).
male(heinrich).
male(klaus).
male(jeremie).
male(horst).
male(dieter).
male(hubert).
male(kloeten).
male(loeten).

female(mary).
female(judy).
female(sabine).
female(emma).
female(foxie).
female(agne).
female(elvira).

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

parent(foxie,dieter).
parent(horst,dieter).
parent(agne, hubert).
parent(horst, hubert).
parent(elvira, agne).

parent(kloeten,loeten).
parent(dieter,loeten).

mr(a,b).

married2(X,Y) :- mr(X,Y).
married2(X,Y) :- mr(Y,X).


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

nephew(Y,X) :-
	child(Y,M), parent(X,M).

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

% sibling(X,Y) :- sibling(Y,X).
% Terminiert nicht, da Abbruchbedingung verdeckt wird.

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

cousin(X,Y) :-
	sibling(U,Z),
	parent(Z,X),
	parent(U,Y),
	X\=Y.

halfSibling(X,Y) :-
	mother(M,X),father(F,X),
	mother(M,Y),father(F1,Y),
	F\=F1,X\=Y.

halfSibling(X,Y) :-
	father(F,X),mother(M,X),
	father(F,Y),mother(M1,Y),
	M\=M1,X\=Y.

testTubeBaby(X) :-
	mother(Z,X),
	not(father(_,X)).

testTubeBaby2(X) :-
	not(father(_,X)),
	mother(Z,X).

