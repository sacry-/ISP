% Informierte Suche


eval_paths([]).

eval_paths([FirstPath|RestPaths]):-
  eval_path(FirstPath),
  eval_paths(RestPaths).



insert_new_paths_informed([],OldPaths,OldPaths).

insert_new_paths_informed([FirstNewPath|RestNewPaths],OldPaths,AllPaths):-
  insert_path_informed(FirstNewPath,OldPaths,FirstInserted),
  insert_new_paths_informed(RestNewPaths,FirstInserted,AllPaths).


insert_path_informed(NewPath,[],[NewPath]).

% Wenn der Pfad billiger ist, dann wird er vorn angefügt. (Alte Pfade sind ja sortiert.)
%
insert_path_informed(NewPath,[FirstPath|RestPaths],[NewPath,FirstPath|RestPaths]):-
  cheaper(NewPath,FirstPath),!.

% Wenn er nicht billiger ist, wird er in den Rest insortiert und der Kopf 
% der Openliste bleibt Kopf der neuen Liste
%
insert_path_informed(NewPath,[FirstPath|RestPaths],[FirstPath|NewRestPaths]):-
  insert_path_informed(NewPath,RestPaths,NewRestPaths).  


cheaper([(_,_,V1)|_],[(_,_,V2)|_]):-
  V1 =< V2.
  

