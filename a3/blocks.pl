

start_description([
  block(block1),
  block(block2),
  block(block3),
  on(table,block2),
  on(table,block3),
  on(block2,block1),
  clear(block1),
  clear(block3),
/* on(table,block4),block(block4),clear(block4),    % mit block4
*/
  handempty
  ]).


goal_description([
  block(block1),
  block(block2),
  block(block3),
  on(table,block3),
  on(table,block1),
  on(block1,block2), %ohne Block4
/* block(block4),on(block4,block2),on(block4,block2), % mit block4
*/
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
state_member(_,[]):- !,fail.
state_member(State,[FirstState|_]) :- state_equal(State, FirstState).
state_member(State,[_|RestStates]) :- state_member(State, RestStates).

% expand_help(State, Name, NewState) wird für alle Instanziierungen von Name und NewState
% wahr, die aus State erreicht werden können.
% needs single Var and multi Var-variant
expand_help(State, Action, NewState) :-
  action(Action, Conds, Dels, Adds),    % "Action suchen"
  satisfies(State, Conds),              % "Conditions testen"
  subtract(State,Dels,State2),          % "Del-List umsetzen"
  append(State2,Adds,NewState),         % "Add-List umsetzen"
  verbose('***'), verbose((Action, State, _)).
  
/*action(put_on_table(X),
       [holding(X)],
       [holding(X)],
       [handempty, clear(X), on(table,X)]).
  */

% satisfies works by putting each of the facts of the state
% into the database checking the condition and retracting the changes to the database.
% ignore is used to succeed regardles of the false coming from the database changes.
satisfies(State, Conds) :-
    assert_each(State),
    all_hold(Conds),
    unassert_each(State).

satisfies(State,_) :-
    unassert_each(State),
    verbose('Failed conditions on State: '), verbose(State),
    fail. % condition failed. reset database and fail

all_hold([]).
all_hold([C|Conds]) :-
    % verbose('Testing Predicate: '), verbose(C),
    C, all_hold(Conds).

% adds a fail for holding such that it doesn't throw predicate-not-found exception. assertz ads at the end.
no_holding :- assertz( ':-'(holding(_),fail) ).
% ':-'(holding(_),(!,fail)) = holding(_) :- !, fail.

% database manipulations. http://www.swi-prolog.org/pldoc/doc_for?object=asserta/2
% Notiz *1
assert_each(XS) :- assert_help(XS), no_holding.
assert_help([]).
assert_help([X|XS]) :- asserta(X), assert_help(XS).
unassert_each([]).
unassert_each([X|XS]) :- retract(X), unassert_each(XS).

expand((_,State,_),Result):-
  findall((Name,NewState,_),expand_help(State,Name,NewState),Result).

verbose(_) :- !. % comment to enable verbose
verbose(X) :- writeln(X).

eval_path([(_,_State,_Value)|_RestPath]):-
  writeln('TODO: eval_path'), halt.
%eval_state(State,"Rest des Literals bzw. der Klausel"
%"Value berechnen".
get_name((N,_,_), N). 

% *1:
/*
?- assert_each([a,b]).
false.
?- a.
true.
?- b.
true.
?- unassert_each([a,b]).
false.
?- a.
false.
?- b.
false.
*/




% *2:
/*
?- goal_description(G),expand_help(G,A,_).
Testing Predicate: 
handempty
Testing Predicate: 
clear(_G1536)
Testing Predicate: 
on(table,block2)
Testing Predicate: 
on(table,block3)
Action found: 
pick_up(block3)
G = [block(block1), block(block2), block(block3), on(table, block3), on(table, block1), on(block1, block2), clear(block3), clear(block2), handempty],
A = pick_up(block3) ;
Testing Predicate: 
handempty
Testing Predicate: 
clear(_G1536)
Testing Predicate: 
on(_G1549,block2)
Testing Predicate: 
block(block1)
Action found: 
pick_up(block2)
G = [block(block1), block(block2), block(block3), on(table, block3), on(table, block1), on(block1, block2), clear(block3), clear(block2), handempty],
A = pick_up(block2) ;
Testing Predicate: 
holding(_G1536)
Testing Predicate: 
holding(_G1537)
false.

?- G = [block(block1),clear(block1),holding(block2),block(block2),on(table,block1)], expand_help(G,_,_).
Testing Predicate: 
handempty
Testing Predicate: 
handempty
Testing Predicate: 
holding(_G1541)
Action found: 
put_on_table(block2)
G = [block(block1), clear(block1), holding(block2), block(block2), on(table, block1)] ;
Testing Predicate: 
holding(_G1542)
Testing Predicate: 
clear(_G1541)
Action found: 
put_on(block1,block2)
G = [block(block1), clear(block1), holding(block2), block(block2), on(table, block1)].
*/


