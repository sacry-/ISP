

start_description([
  block(block1),
  block(block2),
  block(block3),
  on(table,block2),
  on(table,block3),
  on(block2,block1),
  clear(block1),
  clear(block3),
  handempty
  ]).

/*
  on(table,block4), %mit Block4
  block(block4),  %mit Block4
  clear(block4), %mit Block4
*/

goal_description([
  block(block1),
  block(block2),
  block(block3),
  on(table,block3),
  on(table,block1),
  on(block1,block2), %ohne Block4
  clear(block3),
  clear(block2),
  handempty
  ]).

/*
  block(block4), %mit Block4
  on(block4,block2), %mit Block4
  on(block4,block2), %mit Block4
*/


start_node((start,_,_)).

goal_node((_,State,_)) :- goal_description(G), state_equal(State, G).

state_equal(R, X) :- mysubset(R, X), mysubset(X, R).

mysubset([],_).
mysubset([H|T],List):-
  member(H,List),
  mysubset(T,List).

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


% state_member(State, States). Ist der Zustand State
% in der Liste der Zustände States enthalten?
state_member(_,[]):- !,fail.
state_member(State,[FirstState|_]) :- state_equal(State, FirstState).
state_member(State,[_|RestStates]) :- state_member(State, RestStates).




expand_help(State,Name,NewState).
%  "Action suchen"
%  "Conditions testen"
%  "Del-List umsetzen"
%  "Add-List umsetzen".


expand_help(State,ActName,NewState) :-
  action(ActName, CondList, AddList, DelList)
  
expand((_,State,_),Result):-
  findall((Name,NewState,_),expand_help(State,Name,NewState),Result).



