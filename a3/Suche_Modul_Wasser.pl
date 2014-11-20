% Die Schnittstelle umfasst
%   start_description	;Beschreibung des Startzustands
%   start_node          ;Test, ob es sich um einen Startknoten handelt
%   goal_node           ;Test, ob es sich um einen Zielknoten handelt
%   state_member        ;Test, ob eine Zustandsbeschreibung in einer Liste 
%                        von Zustandsbeschreibungen enthalten ist
%   expand              ;Berechnung der Kind-Zustandsbeschreibungen
%   eval-path		;Bewertung eines Pfades



start_description((0,0)).


% Test, ob es sich um einen korrekten Startknoten handelt.
% Trivial geloest: Es wird nur geprueft, ob die Aktion, die
% zu diesem Knoten gefuehrt hat, "start" heisst.
%
start_node((start,_,_)).



goal_node((_,(2,_),_)).

goal_node((_,(_,2),_)).



% state_member muss anwendungsspezifisch definiert werden, weil
% die Zustandsbeschreibung beliebig komplex sein darf.
%  
state_member(State,StateList):-
  member(State,StateList).


expand((_,(L,S),_),Children):-
  fill_large((L,S),(L1,S1)),
  fill_small((L,S),(L2,S2)),
  empty_large((L,S),(L3,S3)),
  empty_small((L,S),(L4,S4)),
  small_into_large((L,S),(L5,S5)),
  large_into_small((L,S),(L6,S6)),
  Children = [(fill_large,(L1,S1),0),
	      (fill_small,(L2,S2),0),
	      (empty_large,(L3,S3),0),
	      (empty_small,(L4,S4),0),
	      (small_into_large,(L5,S5),0),
	      (large_into_small,(L6,S6),0)].  

fill_large((_,Small),(4,Small)).

fill_small((Large,_),(Large,3)).

empty_large((_,Small),(0,Small)).

empty_small((Large,_),(Large,0)).

small_into_large((Large,Small),(LargeNew,SmallNew)):-
  LargeNew is min(Large+Small,4),
  SmallNew is Small-min(4-Large,Small).

large_into_small((Large,Small),(LargeNew,SmallNew)):-
  SmallNew is min(Large+Small,3),
  LargeNew is Large-min(3-Small,Large).
  
  
% Für diese Anwendung ist keine informierte Suche vorgesehen.
% Deshalb wird in jedem Fall der Wert 0 eingetragen.
%  
eval_path([(_,_,0)|_]).
