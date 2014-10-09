

% Dies muss definiert werden, damit es funktioniert.
succ(_).

% successor.pl
zahl(0).
zahl(succ(X)):- zahl(X).

% z.B.
% ? zahl(0).
% true.
% ? zahl(succ(succ(0))).
% true
% ? zahl(vater(0)).
% false.



% addition_succ.pl
add(0,Y,Y).
add(succ(X),Y,succ(Z)) :- add(X,Y,Z).

% mult(X,Y,Z).
mult(0,_,0).
mult(succ(X),Y,R) :- add(Y,Z,R), mult(X,Y,Z).


s(_).
z(0).
z(s(X)):- z(X).

add2(0,Y,Y).
add2(s(X),Y,s(Z)) :- add2(X,Y,Z).

/**
0 + 1 = X
?- add(0,succ(0),X).
X = succ(0).

2 + 1 = X
?- add(succ(succ(0)),succ(0),X).
X = succ(succ(succ(0))).

X + 1 = 1
?- add(X,succ(0),succ(0)).
X = 0 ;
false.

X + 1 = 0
?- add(X,succ(0),0).
false.
**/
->(_,_).

% hanoi.pl
transportiere(1, Von, _, Nach, [Von -> Nach]).

transportiere(AnzahlScheiben, Von, Lager, Nach, Rges) :-
  AnzahlScheiben > 1,
  ObereScheiben is AnzahlScheiben - 1,
  transportiere(ObereScheiben, Von, Nach, Lager, Rfst),
  transportiere(1, Von, Lager, Nach, Rzwt),
  transportiere(ObereScheiben, Lager, Von, Nach, Rdrt),
  append(Rfst,Rzwt, Rint),
  append(Rint, Rdrt, Rges).
%


range(Low, Low, High).
range(Out,Low,High) :- NewLow is Low+1, range(Out, NewLow, High).

