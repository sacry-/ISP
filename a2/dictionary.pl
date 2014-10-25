
% lex(word, semantics, type, numerus, sex)

lex(wer, 			newvar, ip, _, _).

lex(ist, 			_, is_w, s, _).

lex(ist, 			_, v, s, _).
lex(sind,			_, v,p,_).
lex(hat,			_, v,s,_).

lex(der, 			_, det_b, s, m).
lex(die, 			_, det_b, s, f).

lex(ein, 			_, det_u, s, m).
lex(ein, 			_, det_u, s, n).
lex(eine, 		_, det_u, s, f).
lex(einen, 		_, det_u, s, m).

lex(mutter, 	mother, n, s, f).
lex(tochter, 	daughter,n,s, f).
lex(sohn, 		son, n,    s, m).
lex(vater, 		father, n, s, m).
lex(eltern, 	parent, n, p, f).
lex(oma, 			grandmother, n, s, f).
lex(opa, 			grandfather, n, s, m).
lex(tante, 		aunt, n,    s, f).
lex(onkel,		uncle, n,  s, m).
lex(kind, 		child, n,  s, n).
lex(kinder, 	child, n,  p, f).
lex(neffe, 		nephew, n, s, m).
lex(nichte, 	niece, n,  s, f).
lex(cousin, 	cousin, n, s, m).
lex(cousine, 	cousin, n, s, f).


lex(von, 			_, prep, _, _).

lex(Name, 		Name, name, s, m) :- male(Name).
lex(Name, 		Name, name, s, f) :- female(Name).

% lex(hat, 			_, has_w, s, _).

