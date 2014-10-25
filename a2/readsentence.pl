/* Convert bytestream into a List */

read_sentence(List) :- !, put(62), put(32), read_in(List).

read_in([W|Ws]) :-
  get0(C), read_word(C,W,C1), rest_sentence(W,C1,Ws).

rest_sentence(W,_,[]) :- terminal_symbol(W), !.
rest_sentence(_,C,[W1|Ws]) :-
  read_word(C,W1,C1), rest_sentence(W1,C1,Ws).

read_word(C,W,C1) :-
  single_character(C), !, name(W,[C]), get0(C1).
read_word(C,W,C2) :-
  in_word(C,NewC), !,
  get0(C1),
  rest_word(C1,Cs,C2),
  name(W,[NewC|Cs]).
read_word(_,W,C2) :- get0(C1), read_word(C1,W,C2).

rest_word(C,[NewC|Cs],C2) :-
  in_word(C,NewC), !, 
  get0(C1), rest_word(C1,Cs,C2).
rest_word(C,[],C).

terminal_symbol('.').
terminal_symbol('!').
terminal_symbol('?').

single_character(44).  /* , */
single_character(59).  /* ; */
single_character(58).  /* : */
single_character(63).  /* ? */
single_character(33).  /* ! */
single_character(46).  /* . */

in_word(C,C) :- C>96, C<123.           /* a b ... z */
in_word(C,L) :- C>64, C<91, L is C+32. /* A B ... Z */
in_word(C,C) :- C>47, C<58.            /* 1 2 ... 0 */
in_word(223,223).                      /* ß */
in_word(228,228).                      /* ä */
in_word(246,246).                      /* ö */
in_word(252,252).                      /* ü */
in_word(196,228).                      /* Ä */
in_word(214,246).                      /* Ö */
in_word(220,252).                      /* Ü */
in_word(39,39).                        /* ' */
in_word(45,45).                        /* - */
