
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

                 % eval(Algorithm, Heuristik, Path).
eval_path(Path) :- eval(a, intersect, Path).
%eval_state(State,"Rest des Literals bzw. der Klausel"
%"Value berechnen".


% Bei A gilt: f(n) = g(n) + h(n)
% Bewertung vom Kindknoten := Kosten bis zum aktuellen Knoten + geschätzte Kosten ab dem Kindknoten.

eval(a, Heuristik,  Path) :- Path = [Child|RestPath], eval_bisher(a,Heuristik, RestPath), eval_state(Heuristik, Child).

% eval_A_bisher berechnet die bisherigen Kosten eines Pfades indem die Kosten der vorgängeer inkrementiert werden.
% Das erste Element des Pfades wird nicht besonders behandelt.
eval_bisher(a, _Heuristik, [(start,_,0)]). % Kosten vom Startknoten sind 0.
eval_bisher(a,Heuristik, [(_,_,N),(P,S,M)|RestPath]) :- eval_bisher(a,Heuristik, [(P,S,M)|RestPath]), N is M+1. % 
% Siehe Beispiel: ?- path(_X), eval_A_bisher(_X),_X=[(_,_,Val)|_], writeln(Val).

% soll die Restkosten zum Ende berechnen. % TODO: Restkosten bestimmen.

% Heuristik 1: Negative Anzahl der korrekten Teilziele
eval_state(num_correct,  (_,State,N) ) :-
    goal_description(G),
    intersect(State, G, Int),
    length(Int, M),
    length(G, G_),
    N is G_ - M.
    
% Heuristik 2: Anzahl der inkorrekten Teilziele (entspricht genau gleich der ersten Heuristik)
eval_state(num_incorrect,  (_,State,N) ) :-
    goal_description(G),
    set_subtract(G, State, Diff),
    length(Diff, N).

% Heuristik 3: Negative Anzahl der korrekten Heuristiken mit revelanterer Wertung der auf dem Tisch bezogenen Teilziele
eval_state(three,  (_,State,N) ) :-
    goal_description(G),
    intersect(State, G, Int),
    length(Int, M),
    findall(Fact, (member(Fact, State), Fact = on(table, _)), Facts),
    length(Facts, F),
    length(G, Gs),
    findall(GFact, (member(GFact, G), GFact = on(table, _)), GFacts),
    length(GFacts, GFs),
    S = 0.3,
    N_ is Gs + S*GFs - M - S*F,
    N is round(N_*100).


    
set_subtract(Set1, Set2, Diff) :-
    sort(Set1, S1),
    sort(Set2, S2),
    ord_subtract(S1, S2, Diff).

intersect(Set1, Set2, Int) :-
    sort(Set1, S1),
    sort(Set2, S2),
    ord_intersection(S1, S2, Int).

% Utility to return a few paths to wards a good finishing path.
path(X, P) :- path_(X, Df), reverse(Df, P).
path_(short,
    [
        (start,[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),on(block2,block1),clear(block1),clear(block3),on(table,block4),block(block4),clear(block4),handempty],_G68),
        (pick_up(block1),[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),clear(block3),on(table,block4),block(block4),clear(block4),holding(block1),clear(block2)],_G76),
        (put_on_table(block1),[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),clear(block3),on(table,block4),block(block4),clear(block4),clear(block2),handempty,clear(block1),on(table,block1)],_G547),
        (pick_up(block4),[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),clear(block3),block(block4),clear(block2),clear(block1),on(table,block1),holding(block4)],_G819),
        (put_on(block1,block4),[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),clear(block3),block(block4),clear(block2),on(table,block1),handempty,clear(block4),on(block1,block4)],_G1841),
        (pick_up(block2),[block(block1),block(block2),block(block3),on(table,block3),clear(block3),block(block4),on(table,block1),clear(block4),on(block1,block4),holding(block2)],_G3484),
        (put_on(block4,block2),[block(block1),block(block2),block(block3),on(table,block3),clear(block3),block(block4),on(table,block1),on(block1,block4),handempty,clear(block2),on(block4,block2)],_G6452)
    ]
). % calculated path with breadth-first with block4.

% calculated with depath-first with block 4.
path_(long,
[ (start,[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),on(block2,block1),clear(block1),clear(block3),on(table,block4),block(block4),clear(block4),handempty],_G68), (pick_up(block4),[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),on(block2,block1),clear(block1),clear(block3),block(block4),holding(block4)],_G203), (put_on(block3,block4),[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),on(block2,block1),clear(block1),block(block4),handempty,clear(block4),on(block3,block4)],_G346), (pick_up(block1),[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),block(block4),clear(block4),on(block3,block4),holding(block1),clear(block2)],_G420), (put_on_table(block1),[block(block1),block(block2),block(block3),on(table,block2),on(table,block3),block(block4),clear(block4),on(block3,block4),clear(block2),handempty,clear(block1),on(table,block1)],_G550), (pick_up(block2),[block(block1),block(block2),block(block3),on(table,block3),block(block4),clear(block4),on(block3,block4),clear(block1),on(table,block1),holding(block2)],_G695), (put_on(block1,block2),[block(block1),block(block2),block(block3),on(table,block3),block(block4),clear(block4),on(block3,block4),on(table,block1),handempty,clear(block2),on(block1,block2)],_G832), (pick_up(block4),[block(block1),block(block2),block(block3),on(table,block3),block(block4),on(table,block1),clear(block2),on(block1,block2),holding(block4),clear(block3)],_G906), (put_on_table(block4),[block(block1),block(block2),block(block3),on(table,block3),block(block4),on(table,block1),clear(block2),on(block1,block2),clear(block3),handempty,clear(block4),on(table,block4)],_G1036), (pick_up(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block1),clear(block2),on(block1,block2),clear(block4),on(table,block4),holding(block3)],_G1181), (put_on(block4,block3),[block(block1),block(block2),block(block3),block(block4),on(table,block1),clear(block2),on(block1,block2),on(table,block4),handempty,clear(block3),on(block4,block3)],_G1318), (pick_up(block2),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block4),clear(block3),on(block4,block3),holding(block2),clear(block1)],_G1392), (put_on_table(block2),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block4),clear(block3),on(block4,block3),clear(block1),handempty,clear(block2),on(table,block2)],_G1522), (pick_up(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block4),clear(block1),clear(block2),on(table,block2),holding(block3),clear(block4)],_G1601), (put_on_table(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block4),clear(block1),clear(block2),on(table,block2),clear(block4),handempty,clear(block3),on(table,block3)],_G1812), (pick_up(block4),[block(block1),block(block2),block(block3),block(block4),on(table,block1),clear(block1),clear(block2),on(table,block2),clear(block3),on(table,block3),holding(block4)],_G2034), (put_on(block2,block4),[block(block1),block(block2),block(block3),block(block4),on(table,block1),clear(block1),on(table,block2),clear(block3),on(table,block3),handempty,clear(block4),on(block2,block4)],_G2187), (pick_up(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block1),clear(block1),on(table,block2),clear(block4),on(block2,block4),holding(block3)],_G2327), (put_on(block1,block3),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block2),clear(block4),on(block2,block4),handempty,clear(block3),on(block1,block3)],_G2398), (pick_up(block4),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block2),clear(block3),on(block1,block3),holding(block4),clear(block2)],_G2466), (put_on_table(block4),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block2),clear(block3),on(block1,block3),clear(block2),handempty,clear(block4),on(table,block4)],_G2596), (pick_up(block2),[block(block1),block(block2),block(block3),block(block4),on(table,block1),clear(block3),on(block1,block3),clear(block4),on(table,block4),holding(block2)],_G2675), (put_on(block4,block2),[block(block1),block(block2),block(block3),block(block4),on(table,block1),clear(block3),on(block1,block3),on(table,block4),handempty,clear(block2),on(block4,block2)],_G2806), (pick_up(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block4),clear(block2),on(block4,block2),holding(block3),clear(block1)],_G2880), (put_on_table(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(table,block4),clear(block2),on(block4,block2),clear(block1),handempty,clear(block3),on(table,block3)],_G3010), (pick_up(block1),[block(block1),block(block2),block(block3),block(block4),on(table,block4),clear(block2),on(block4,block2),clear(block3),on(table,block3),holding(block1)],_G3155), (put_on(block3,block1),[block(block1),block(block2),block(block3),block(block4),on(table,block4),clear(block2),on(block4,block2),on(table,block3),handempty,clear(block1),on(block3,block1)],_G3292), (pick_up(block2),[block(block1),block(block2),block(block3),block(block4),on(table,block4),on(table,block3),clear(block1),on(block3,block1),holding(block2),clear(block4)],_G3366), (put_on_table(block2),[block(block1),block(block2),block(block3),block(block4),on(table,block4),on(table,block3),clear(block1),on(block3,block1),clear(block4),handempty,clear(block2),on(table,block2)],_G3496), (pick_up(block1),[block(block1),block(block2),block(block3),block(block4),on(table,block4),on(table,block3),clear(block4),clear(block2),on(table,block2),holding(block1),clear(block3)],_G3575), (put_on(block4,block1),[block(block1),block(block2),block(block3),block(block4),on(table,block4),on(table,block3),clear(block2),on(table,block2),clear(block3),handempty,clear(block1),on(block4,block1)],_G3645), (pick_up(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block4),clear(block2),on(table,block2),clear(block1),on(block4,block1),holding(block3)],_G3779), (put_on(block2,block3),[block(block1),block(block2),block(block3),block(block4),on(table,block4),on(table,block2),clear(block1),on(block4,block1),handempty,clear(block3),on(block2,block3)],_G3850), (pick_up(block1),[block(block1),block(block2),block(block3),block(block4),on(table,block4),on(table,block2),clear(block3),on(block2,block3),holding(block1),clear(block4)],_G4522), (put_on_table(block1),[block(block1),block(block2),block(block3),block(block4),on(table,block4),on(table,block2),clear(block3),on(block2,block3),clear(block4),handempty,clear(block1),on(table,block1)],_G4929), (pick_up(block4),[block(block1),block(block2),block(block3),block(block4),on(table,block2),clear(block3),on(block2,block3),clear(block1),on(table,block1),holding(block4)],_G5224), (put_on(block1,block4),[block(block1),block(block2),block(block3),block(block4),on(table,block2),clear(block3),on(block2,block3),on(table,block1),handempty,clear(block4),on(block1,block4)],_G5569), (pick_up(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block2),on(table,block1),clear(block4),on(block1,block4),holding(block3),clear(block2)],_G5869), (put_on_table(block3),[block(block1),block(block2),block(block3),block(block4),on(table,block2),on(table,block1),clear(block4),on(block1,block4),clear(block2),handempty,clear(block3),on(table,block3)],_G6288), (pick_up(block2),[block(block1),block(block2),block(block3),block(block4),on(table,block1),clear(block4),on(block1,block4),clear(block3),on(table,block3),holding(block2)],_G6595), (put_on(block4,block2),[block(block1),block(block2),block(block3),block(block4),on(table,block1),on(block1,block4),clear(block3),on(table,block3),handempty,clear(block2),on(block4,block2)],_G6883)]
).


tails([],[[]]) :- !.
tails(XS, [XS|RSS]) :- XS = [_|RS],!, tails(RS,RSS).

% verbose(_) :- !. % comment to enable verbose
verbose(X) :- verbose_help(X),nl,!.
verbose_help(X+Y) :- verbose_help(X), verbose_help(Y).
verbose_help(X) :- write(X).


% *2:
/*
?- goal_description(G),expand_help(G,A,_).
?- G = [block(block1),clear(block1),holding(block2),block(block2),on(table,block1)], expand_help(G,_,_).
?- G = [block(block1),block(block2),block(block3),on(table,block2),on(table,block3),clear(block3),clear(block2),handempty,clear(block1),on(table,block1)], expand((_,G,_),R).
*/

