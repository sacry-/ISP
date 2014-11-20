% Die Schnittstelle umfasst
%   start_description	;Beschreibung des Startzustands
%   start_node          ;Test, ob es sich um einen Startknoten handelt
%   goal_node           ;Test, ob es sich um einen Zielknoten handelt
%   state_member        ;Test, ob eine Zustandsbeschreibung in einer Liste 
%                        von Zustandsbeschreibungen enthalten ist
%   expand              ;Berechnung der Kind-Zustandsbeschreibungen
%   eval-path		;Bewertung eines Pfades


start_description([
  block(block1),
  block(block2),
  block(block3),
% block(block4),  %mit Block4
  on(table,block2),
  on(table,block3),
  on(block2,block1),
%  on(table,block4), %mit Block4
  clear(block1),
  clear(block3),
%  clear(block4), %mit Block4
  handempty
  ]).

goal_description([
  block(block1),
  block(block2),
  block(block3),
%  block(block4), %mit Block4
%  on(block4,block2), %mit Block4
  on(table,block3),
  on(table,block1),
%  on(block1,block4), %mit Block4
  on(block1,block2), %ohne Block4
  clear(block3),
  clear(block2),
  handempty
  ]).



start_node((start,_,_)).

goal_node((_,State,_)) :- goal_description(G), equal_as_set(State, G).
  "Zielbedingungen einlesen"
  "Zustand gegen Zielbedingungen testen".

equal_as_set([],[]).
equal_as_set(R1, [F|R2]) :- member(F,R1), equal_as_set(R1, R2).
equal_as_set([F|R], R2) :- member(F,R2), equal_as_set(R, R2).


% Aufgrund der Komplexität der Zustandsbeschreibungen kann state_member nicht auf 
% das Standardprädikat member zurückgeführt werden.
%  
state_member(_,[]):- !,fail.

state_member(State,[FirstState|_]):-
  "Test, ob State bereits durch FirstState beschrieben war. Tipp: Eine 
  Lösungsmöglichkeit besteht in der Verwendung einer Mengenoperation, z.B. subtract"  ,!.  

%Es ist sichergestellt, dass die beiden ersten Klauseln nicht zutreffen.
state_member(State,[_|RestStates]):-  
  "rekursiver Aufruf".


eval_path([(_,State,Value)|RestPath]):-
  eval_state(State,"Rest des Literals bzw. der Klausel"
  "Value berechnen".

  

action(pick_up(X),
       [handempty, clear(X), on(table,X)],
       [handempty, clear(X), on(table,X)],
       [holding(X)]).

action(pick_up(X),
       [handempty, clear(X), on(Y,X), block(Y)],
       [handempty, clear(X), on(Y,X)],
       [holding(X), clear(Y)]).

action(put_on_table(X),
       [holding(X)],
       [holding(X)],
       [handempty, clear(X), on(table,X)]).

action(put_on(Y,X),
       [holding(X), clear(Y)],
       [holding(X), clear(Y)],
       [handempty, clear(X), on(Y,X)]).


% Hilfskonstrukt, weil das PROLOG "subset" nicht die Unifikation von Listenelementen 
% durchführt, wenn Variablen enthalten sind. "member" unifiziert hingegen.
%
mysubset([],_).
mysubset([H|T],List):-
  member(H,List),
  mysubset(T,List).


expand_help(State,Name,NewState):-
  "Action suchen"
  "Conditions testen"
  "Del-List umsetzen"
  "Add-List umsetzen".
  
expand((_,State,_),Result):-
  findall((Name,NewState,_),expand_help(State,Name,NewState),Result).


























