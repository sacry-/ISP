

% successor.pl
%
% zahl(0).
% zahl(succ(X)):- zahl(X).

% Überarbeitete Version mit succ/2
zahl(0).
zahl(Y) :- succ(X,Y), zahl(X).

% addition_succ.pl
% add(0,Y,Y).
% add(succ(X),Y,succ(Z)) :-
%  add(X,Y,Z).
add(0, X, X).
add(X, Y, Z) :-
	succ(N, X),
	succ(M, Z),
	add(N, Y, M).

% hanoi.pl
transportiere(1, Von, _, Nach) :-
  write('Bringe eine Scheibe von '), write(Von), write(' nach '), write(Nach), write(.), nl.

transportiere(AnzahlScheiben, Von, Lager, Nach) :-
  AnzahlScheiben > 1,
  ObereScheiben is AnzahlScheiben - 1,
  transportiere(ObereScheiben, Von, Nach, Lager),
  transportiere(1, Von, Lager, Nach),
  transportiere(ObereScheiben, Lager, Von, Nach).
%

