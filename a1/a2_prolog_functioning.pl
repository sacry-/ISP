

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
% add(0,Y,Y).
% add(succ(X),Y,succ(Z)) :-
%  add(X,Y,Z).


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

