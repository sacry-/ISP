:- ensure_loaded([dictionary]).
:- ensure_loaded([readsentence]).
:- ensure_loaded([family_tree]).


ask :- 
    writeln('Ask a question. Or write \"ex1.\" to \"ex4.\" for examples'),
    read_sentence(In),
    trim(Q,In),
    processInput(Q).

processInput(['ex1']) :- Q = [ist,heinrich,der,onkel,von,emma], writeln(Q), question(Q,[]).
processInput(['ex2']) :- Q = [wer,ist,der,onkel,von,emma], writeln(Q), question(Q,[]).
processInput(['ex3']) :- Q = [wer,ist,der,sohn,von,mary], writeln(Q), question(Q,[]).
processInput(['ex4']) :- Q = [ist,der,onkel,von,emma,der,sohn,von,mary], writeln(Q), question(Q,[]).
processInput(['ex5']) :- Q = [wer,sind,die,eltern,von,emma], writeln(Q), question(Q,[]).

processInput(['ex6']) :- Q = [ist,heinrich,die,_,von,_], writeln(Q), question(Q,[]).
processInput(['ex7']) :- Q = [sind,die,mutter,von|_], writeln(Q), question(Q,[]).

processInput(Q) :- question(Q,[]).

/*
Positivbeispiele:
ist heinrich der onkel von emma? ~ question([ist,heinrich,der,onkel,von,emma],[]).
wer ist der onkel von emma? ~ question([wer,ist,der,onkel,von,emma],[]).
wer ist ein sohn von mary? ~ question([wer,ist,der,sohn,von,mary],[]).
ist der onkel von emma der sohn von mary? ~ question([ist,der,onkel,von,emma,der,sohn,von,mary],[]).
wer sind die eltern von emma? ~ question([wer,sind,die,eltern,von,emma],[]).
*/

/* Negativbeispiele
question([ist,heinrich,die,_,von,_],[]). % Falsches Geschlecht wird erkannt.
question([sind,die,mutter,von|_],[]). % Falscher Numerus wird erkannt.

*/

% Entfernt das ? . oder ! am ende
trim(Q,In) :- append(Q,['.'],In).
trim(Q,In) :- append(Q,['?'],In).
trim(Q,In) :- append(Q,['!'],In).

% Uncomment this line to disable verbose
verbose(_) :- !.    % Notiz *1
verbose(X) :- writeln(X).


question --> open_q.
question --> closed_q.

% wer ist der onkel von emma?
% wer sind die eltern von emma?
open_q -->
		    ip(N,G),
		    v(N,G),
		    rel(N,G, [Rel, Name2]),
		    {
			    Ques =.. [Rel, Wer, Name2],
			    verbose(Ques),
			    Ques,
			    write('Es ist '), writeln(Wer)
		    }.
% open_q --> { writeln('Niemand :(') }.

% ist heinrich der onkel von emma?
closed_q -->
		    is_w(N,G),
		    np(N,G,Name1),
		    rel(N,G, [Rel, Name2]),
		    {
		        Ques =.. [Rel, Name1, Name2],
    		    verbose(Ques),
    		    Ques,
    		    writeln('Ja :)')
		    }.

% ist der onkel von emma der sohn von mary?
closed_q -->
		    is_w(N,G),
		    np(N,G,[Rel1, Name1]),
		    rel(N,G, [Rel2, Name2]),
		    {
		        Q1 =.. [Rel1, X, Name1],
		        Q2 =.. [Rel2, X, Name2],
		        verbose(Q1),
		        verbose(Q2),
		        Q1,
		        Q2,
    		    writeln('Ja :)')
		    }.
% closed_q --> { writeln('Nein :(') }.

rel(N,G, [Rel, Name]) --> det(N,G), n(N,G, Rel), pp(Name).

np(N,G,Name) --> name(N,G, Name).
np(N,G,Rel) --> rel(N,G, Rel).

pp(Name) --> prep, np(_,_,Name).

vp --> v.
vp --> v, np.

det(N,G) --> det_b(N,G).
det(N,G) --> det_u(N,G).

% Lexikazugriff
ip(N,G) --> 	 [X], {lex(X,_,ip,  	N,G)}.
is_w(N,G) -->  [X], {lex(X,_,is_w,	N,G)}.
n(N,G, Sem)  --> 	 [X], {lex(X,Sem,n,N,G)}.
v(N,G)  --> 	 [X], {lex(X,_,v,   	N,G)}.
prep    --> 	 [X], {lex(X,_,prep,	_,_)}.
name(N,G, Sem)  --> [X], {lex(X,Sem,name,  N,G)}.
det_b(N,G) --> [X], {lex(X,_,det_b, N,G)}.
det_u(N,G) --> [X], {lex(X,_,det_u, N,G)}.


% *1: Das cut verhindert, dass beim Backtraking einer falschen Aussage nicht die verbose/1 auch gebacktrakt wird.
% Sonst würden verbose-meldungen von falschen Aussagen erscheinen. (z.B. son([uncle,emma], mary))

