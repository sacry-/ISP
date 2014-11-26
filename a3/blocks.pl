
% ============================== GENERAL =================================

start_description([
  block(block1),
  block(block2),
  block(block3),
  on(table,block2),
  on(table,block3),
  on(block2,block1),
  clear(block1),
  clear(block3),
  on(table,block4),block(block4),clear(block4),    % mit block4
  handempty
  ]).


goal_description([
  block(block1),
  block(block2),
  block(block3),
  on(table,block3),
  on(table,block1),
%  on(block1,block2), %ohne Block4
  block(block4),on(block1,block4),on(block4,block2), % mit block4
  clear(block3),
  clear(block2),
  handempty
  ]).

start_node((start,_,_)).

goal_node((_,State,_)) :- goal_description(G), mysubset(G, State).

state_equal(R, X) :- mysubset(R, X), mysubset(X, R).

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





% ================================== EXPAND ==============================================

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

% ================================= INFORMED SEARCH ===============================


                            % eval(Algorithm, Heuristik, Path).
eval_path(Algorithm, Path) :- eval(Algorithm, count_table, Path).

eval(Algorithm, Heuristic, [Child|RestPath]) :-
    length(RestPath, Bisher),
    eval_state(Algorithm, Heuristic, Child, Bisher).

% Bei A gilt: f(n) = g(n) + h(n)
eval_state(a, Heuristic, (_,State,Value), Bisher) :-
    heuristic(Heuristic, State, Restkosten),
    Value is Bisher + Restkosten.

% Die anderen ignorieren die bisherigen Kosten
eval_state(_, Heuristic, (_,State,Value), _) :-
    heuristic(Heuristic, State, Value).

% Heuristik 0: Keine Bewertung
heuristic( zero, _, 0 ).

% Heuristik 1: Wie viele ziele sind noch zu erfüllen?
heuristic( count_missing, State, H ) :-
    goal_description(G),
    subtract(G, State, Missing),
    length(Missing, H).

% Heuristik 1: Wie viele ziele sind noch zu erfüllen? Mit größerer Gewichtung von on(table,X) Zielen.
heuristic( count_table,State,Value) :-
  goal_description(G),
  subtract(State,G,Missing),
  IsOnTable = (member(P, Missing), P = (on(table,_))),
  findall(P, IsOnTable, Ps),
  length(Ps, TableN),
  length(Missing, MissingN),
  Value is (MissingN - TableN) + TableN * 2. % doppelte Wertung der on(table,_) Fakten

% ================================= UTILS =======================================

verbose(_) :- !. % comment to enable verbose
verbose(X) :- verbose_help(X),nl,!.
verbose_help(X+Y) :- verbose_help(X), verbose_help(Y).
verbose_help(X) :- write(X).


mysubset([],_).
mysubset([H|T],List):-
  member(H,List),
  mysubset(T,List).


