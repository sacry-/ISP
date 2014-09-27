
/** knowledge 1 - 5 **/
man(tom).
man(jon).
man(tim).

woman(mia).
woman(jody).
woman(yolanda).
party.

happy(yolanda). 
happy(vincent). 

listens2Music(mia). 
listens2Music(yolanda):-  happy(yolanda). 
listens2Music(butch). 

playsAirGuitar(jody).
playsAirGuitar(mia):-  listens2Music(mia). 
playsAirGuitar(yolanda):-  listens2Music(yolanda).
playsAirGuitar(vincent):- 
  listens2Music(vincent), 
  happy(vincent). 
playsAirGuitar(butch):- 
  happy(butch). 
playsAirGuitar(butch):- 
  listens2Music(butch).

loves(Y, X) :-
		man(Y), 
		woman(X).

jealous(X,Y):-  loves(X,Z),  loves(Y,Z).






