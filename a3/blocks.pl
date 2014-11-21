
start_description([
  block(block1),
  block(block2),
  block(block3),
  on(table,block2),
  on(table,block3),
  on(block2,block1),
  clear(block1),
  clear(block3),
%  on(table,block4),block(block4),clear(block4),    % mit block4
  handempty
  ]).


goal_description([
  block(block1),
  block(block2),
  block(block3),
  on(table,block3),
  on(table,block1),
  on(block1,block2), %ohne Block4
%  block(block4),on(block1,block4),on(block4,block2), % mit block4
  clear(block3),
  clear(block2),
  handempty
  ]).



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
state_member(_,[]):- !, fail.
state_member(State,[FirstState|_]) :- state_equal(State, FirstState).
state_member(State,[_|RestStates]) :- state_member(State, RestStates).

% expand_help(State, Name, NewState) wird für alle Instanziierungen von Name und NewState
% wahr, die aus State erreicht werden können.
% needs single Var and multi Var-variant
expand_help(State, Action, NewState) :-
  action(Action, Conds, Dels, Adds),    % "Action suchen"
  %verbose('Trying Action'+Action),
  satisfies(State, Conds),              % "Conditions testen"
  subtract(State,Dels,State2),          % "Del-List umsetzen"
  append(State2,Adds,NewState),         % "Add-List umsetzen"
  verbose('Found Action & State: ' + (State -> Action -> NewState)).

% satisfies works by putting each of the facts of the state
% into the database checking the condition. The changes are no immediately retracted,
% because multiple Conds might match this State (f.e. pcik_up(block1) and pick_up(block2)).
% therefore the database is left intact and only reset before the next try.
satisfies(State, Conds) :-
    unassert_all,   % reset database before copying state
    assert_each(State),
    %verbose('Testing Predicates'+Conds),
    all_hold(Conds).
%    verbose('Solution'+Conds).

unassert_all :-
    retractall( on(_,_)    ),
    retractall( block(_)   ),
    retractall( clear(_)   ),
    retractall( handempty  ),
    retractall( holding(_) ).

all_hold([]).
all_hold([C|Conds]) :-
    %verbose('Testing Predicate: ' + C),
    C,
    %verbose('Suceeded '+C),
    all_hold(Conds).


% database manipulations. http://www.swi-prolog.org/pldoc/doc_for?object=asserta/2
% Notiz *1
assert_each([X|XS]) :- asserta(X), assert_each(XS).
assert_each([]).
unassert_each([X|XS]) :- ignore(retract(X)), unassert_each(XS).
unassert_each([]).

expand((_,State,_),Result):-
  findall((Name,NewState,_),expand_help(State,Name,NewState),Result).


eval_path([(_,_State,_Value)|_RestPath]):-
  writeln('TODO: eval_path'), !,fail.
%eval_state(State,"Rest des Literals bzw. der Klausel"
%"Value berechnen".


verbose(_) :- !. % comment to enable verbose
verbose(X) :- verbose_help(X),nl,!.
verbose_help(X+Y) :- verbose_help(X), verbose_help(Y).
verbose_help(X) :- write(X).


% *2:
/*
?- goal_description(G),expand_help(G,A,_).
?- G = [block(block1),clear(block1),holding(block2),block(block2),on(table,block1)], expand_help(G,_,_).
?- G = [block(block1),block(block2),block(block3),on(table,block2),on(table,block3),clear(block3),clear(block2),handempty,clear(block1),on(table,block1)], expand((_,G,_),R).
*/

