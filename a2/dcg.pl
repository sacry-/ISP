:- ensure_loaded([dictionary]).
%:- ensure_loaded([readsentence]).

/**

bob|-----------|mary      	hans|-------------|sabine            elvira
     |       |                    |         |                      |
  heinrich  judy|--------------|klaus     foxie|-----|horst|-----|agne|----|kai (test-tube-baby)
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
male(kai).

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
	male(Y), child(Y,M), parent(X,M).

niece(Y,X) :-
	female(Y), child(Y,M), parent(X,M).

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
	mother(_,X),
	not(father(_,X)).

testTubeBaby2(X) :-
	not(father(_,X)),
	mother(_,X).




question --> open_q(_).
question --> closed_q(_).

open_q(N) --> ip(N,G), v(N,G), rel(N,G).
closed_q(N) --> is_q(N).
closed_q(N) --> has_q(N).

is_q(N) --> is_w(N,G), np(N,G), rel(N,G).

rel(N,G) --> det(N,G), n(N,G).
rel(N,G) --> det(N,G), n(N,G), pp.

np(N,G) --> name(N,G).
np(N,G) --> rel(N,G).

pp --> prep, np(_,_).

vp --> v.
vp --> v, np.

det(N,G) --> det_b(N,G).
det(N,G) --> det_u(N,G).

ip(N,G) --> 	 [X], {lex(X,_,ip,  	N,G)}.
is_w(N,G) -->  [X], {lex(X,_,is_w,	N,G)}.
has_w(N,G) --> [X], {lex(X,_,has_w, N,G)}.
n(N,G)  --> 	 [X], {lex(X,_,n,   	N,G)}.
v(N,G)  --> 	 [X], {lex(X,_,v,   	N,G)}.
prep    --> 	 [X], {lex(X,_,prep,	_,_)}.
name(N,G)  --> [X], {lex(X,_,name,  N,G)}.
det_b(N,G) --> [X], {lex(X,_,det_b, N,G)}.
det_u(N,G) --> [X], {lex(X,_,det_u, N,G)}.

/*
Positivbeispiele:
question([ist,horst,der,onkel,von,emma],[]).
question([wer,ist,der,onkel,von,X],[]).
question([ist,der,onkel,von,agne,der,X,von,bob],[]).

*/

/* Negativbeispiele
question([ist,der,onkel,von,agne,die,X,von,bob],[]).


*/

% has_q(N) --> has_w(N,G), np(N,G), rel(_,_).

% question([ist,der,onkel,von,agne,die,mutter,von,bob],[]).
% die beiden Teilsätze besziehen sich auf das gleiche Objekt.
% => gleiches Gender und Numerus

% question([hat,bob,eine,mutter],[]).
% die beiden Teilsätze können sich auf unterschiedliche Objekte beziehen
% => unabhängiges Gender und Numerus

% Fragen mit "Ist" und "Hat"
% müssen also getrennt behandelt werden.

% Die Integration von Hat-Fragen öffnet neue Probleme:
% z.B. wird "Hat Bob der Vater?" akzeptiert.
% Die Bestimmtheit/Unbestimmtheit von Ist und Hat
% müsste zusätzlich übergeben werden.





