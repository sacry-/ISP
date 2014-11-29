% Das Programm wird mit solve(depth), solve(breadth) oder solve(informed) aufgerufen.
solve(Strategy):-
  start_description(StartState),
  solve((start,StartState,_),Strategy).
  
  
% Prädikat search: 
%   1. Argument ist die Liste aller Pfade. Der aktuelle Pfad ist an erster Stelle. 
%   Jeder Pfad ist als Liste von Zuständen repräsentiert, allerdings in falscher 
%   Reihenfolge, d.h. der Startzustand ist an letzter Position.
%   2. Argument ist die Strategie
%   3. Argument ist der Ergebnis-Pfad.
%
solve(StartNode,Strategy) :-
  S = (
      start_node(StartNode),
      search([[StartNode]],Strategy,Path)
    ),
  time(S),!,
  reverse(Path,Path_in_correct_order),
  length(Path, Pn),
  write(' with Length'),writeln(Pn),
  write_solution(Path_in_correct_order).

% auf false/true setzten um auch die Suche anzuzeigen.
enable_writing(false).

newl :- enable_writing(X), X, !, nl.
newl.
write2(X) :- enable_writing(W), W, !, write(X).
write2(_).

write_solution(Path):-
  write2('SOLUTION:'),newl,
  write_actions(Path).  

write_actions([]).

write_actions([(Action,_,_)|Rest]):-
  write2('Action: '),write2(Action),newl,
  write_actions(Rest).





% Abbruchbedingung: Wenn ein Zielzustand erreicht ist, wird der aktuelle Pfad an den
% dritten Parameter übertragen.
%
search([[FirstNode|Predecessors]|_],_,[FirstNode|Predecessors]) :- 
  goal_node(FirstNode),
  newl,write2('SUCCESS'),newl,!.


search([[FirstNode|Predecessors]|RestPaths],Strategy,Solution) :- 
  expand(FirstNode,Children),                                    % Nachfolge-Zustände berechnen
  generate_new_paths(Children,[FirstNode|Predecessors],NewPaths), % Nachfolge-Zustände einbauen 
  insert_new_paths(Strategy,NewPaths,RestPaths,AllPaths),        % Neue Pfade einsortieren
  search(AllPaths,Strategy,Solution).






















generate_new_paths(Children,Path,NewPaths):-
  maplist(get_state,Path,States),
  generate_new_paths_help(Children,Path,States,NewPaths).



% Abbruchbedingung, wenn alle Kindzustände abgearbeitet sind.
%
generate_new_paths_help([],_,_,[]).


% Falls der Kindzustand bereits im Pfad vorhanden war, wird der gesamte Pfad verworfen,
% denn er würde nur in einem Zyklus enden. (Dies betrifft nicht die Fortsetzung des 
% Pfades mit Geschwister-Kindern.) Es wird nicht überprüft, ob der Kindzustand in einem
% anderen Pfad vorkommt, denn möglicherweise ist dieser Weg der günstigere.
%
generate_new_paths_help([FirstChild|RestChildren],Path,States,RestNewPaths):- 
  get_state(FirstChild,State),state_member(State,States),!,
  generate_new_paths_help(RestChildren,Path,States,RestNewPaths).


% Ansonsten, also falls der Kindzustand noch nicht im Pfad vorhanden war, wird er als 
% Nachfolge-Zustand eingebaut.
%
generate_new_paths_help([FirstChild|RestChildren],Path,States,[[FirstChild|Path]|RestNewPaths]):- 
  generate_new_paths_help(RestChildren,Path,States,RestNewPaths).

 
get_state((_,State,_),State).



%%% Strategie:

write_action([[(Action,_)|_]|_]):-
  newl,write2('Action: '),write2(Action),newl.

write_next_state([[_,(_,State)|_]|_]):-
  newl,write2('Go on with: '),write2(State),newl.

write_state([[(_,State)|_]|_]):-
  write2('New State: '),write2(State),newl.

write_fail(depth,[[(_,State)|_]|_]):-
  newl,write2('FAIL, go on with: '),write2(State),newl.

write_fail(_,_):-  nl,write('FAIL').

% Alle Strategien: Keine neuen Pfade vorhanden
insert_new_paths(Strategy,[],OldPaths,OldPaths):-
  write_fail(Strategy,OldPaths),!.

% Tiefensuche
insert_new_paths(depth,NewPaths,OldPaths,AllPaths):-
  append(NewPaths,OldPaths,AllPaths),
  write_action(NewPaths).

% Breitensuche
insert_new_paths(breadth,NewPaths,OldPaths,AllPaths):-
  append(OldPaths,NewPaths,AllPaths),
  write_next_state(AllPaths),
  write_action(AllPaths).


% =============================== INFORMIERTE SUCHE ================================

% Informierte Suche mit A
insert_new_paths(a,NewPaths,OldPaths,AllPaths):-
  eval_paths(a, NewPaths),
  insert_new_paths_informed(NewPaths,OldPaths,AllPaths),
  write_action(AllPaths),
  write_state(AllPaths).
  
  
% Informierte Suche mit Optimistischen Bergsteigen (nur direkte Nachfolger werden betrachtet, der mit den geringsten Restkosten wird genommen)
insert_new_paths(optimist,NewPaths,_,AllPaths):-
  eval_paths(optimist, NewPaths),
  insert_new_paths_informed(NewPaths,[],AllPaths),
  write_action(AllPaths),
  write_state(AllPaths).

% Informierte Suche mit backtracked Hill-Climbing (so wie optimistisches Bergsteigen. jedoch werden auch die alten pfade berücksichtigt)
insert_new_paths(backclimb,NewPaths,OldPaths,AllPaths):-
  eval_paths(backclimb, NewPaths),
  insert_new_paths_informed(NewPaths,[],SortedNewPaths),
  append(SortedNewPaths, OldPaths, AllPaths),
  write_action(AllPaths),
  write_state(AllPaths).


% Informierte Suche mit Gierigem Bergsteigen. So wie A, nur werden die Restkosten ignoriert.
insert_new_paths(greedy,NewPaths,OldPaths,AllPaths):-
  eval_paths(greedy, NewPaths),
  insert_new_paths_informed(NewPaths,OldPaths,AllPaths),
  write_action(AllPaths),
  write_state(AllPaths).





