:- ensure_loaded('checker.pl').

%test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un literal, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de literali reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un literal)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
%
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
intrebari(integ(_, _, [], _), []).

intrebari(integ(H, W, [((R, C), [(Text, Dir, Id) | Qs]) | T], Vocab), [((R, C), Text, Dir, Id) | L_qs]) :- 
                                              intrebari(integ(H, W, [((R, C), Qs) | T], Vocab), L_qs), !.

intrebari(integ(H, W, [((_, _), []) | T], Vocab), L_qs) :- 
                                                intrebari(integ(H, W, T, Vocab), L_qs), !.

intrebari(integ(H, W, [((_, _), _) | T], Vocab), L_qs) :- intrebari(integ(H, W, T, Vocab), L_qs), !.

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_in_intrebari([], _, _) :- fail, !.
id_in_intrebari([((_, _), Q, _, Id) | _], Q, Id).
id_in_intrebari([_ | L_qs], Q, Id) :- id_in_intrebari(L_qs, Q, Id).

id_intrebare(integ(H, W, L, Vocab), 
            Q, 
            Id) :- intrebari(integ(H, W, L, Vocab), L_qs), id_in_intrebari(L_qs, Q, Id).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt atomi (literali).
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
gaseste_intrebare([], _, _) :- fail, !.
gaseste_intrebare([((R, C), Q, Dir, Id) | _], Q, ((R, C), Q, Dir, Id)).
gaseste_intrebare([_ | L_qs], Q, ((R, C), Q, Dir, Id)) :- gaseste_intrebare(L_qs, Q, ((R, C), Q, Dir, Id)).

adauga_raspuns(Integ, [], _, Integ).

adauga_raspuns(Integ, [Car | Cs], ((R, C), Q, j, Id), Sol_Integ) :- Rj is R + 1, 
                                                                    adauga_raspuns(Integ, Cs, ((Rj, C), Q, j, Id), integ(H, W, L, Vocab)),
                                                                    \+member(((Rj, C), Car), L),
                                                                    Sol_Integ = integ(H, W, [((Rj, C), Car) | L], Vocab).
adauga_raspuns(Integ, [_ | Cs], ((R, C), Q, j, Id), Sol_Integ) :-   Rj is R + 1, 
                                                                    adauga_raspuns(Integ, Cs, ((Rj, C), Q, j, Id), Curr_Integ),
                                                                    Sol_Integ = Curr_Integ.

adauga_raspuns(Integ, [Car | Cs], ((R, C), Q, d, Id), Sol_Integ) :- Cd is C + 1, 
                                                                    adauga_raspuns(Integ, Cs, ((R, Cd), Q, d, Id), integ(H, W, L, Vocab)),
                                                                    \+member(((R, Cd), Car), L),
                                                                    Sol_Integ = integ(H, W, [((R, Cd), Car) | L], Vocab).
adauga_raspuns(Integ, [_ | Cs], ((R, C), Q, d, Id), Sol_Integ) :- Cd is C + 1, 
                                                                    adauga_raspuns(Integ, Cs, ((R, Cd), Q, d, Id), Curr_Integ),
                                                                    Sol_Integ = Curr_Integ.

completare(Integ, [], Integ).

completare(integ(H, W, L, Vocab),
           [(Intrebare, Rasp) | Sol],
           S_Integ) :- atom_chars(Rasp , R_list),
                       intrebari(integ(H, W, L, Vocab), L_qs),

                       gaseste_intrebare(L_qs, 
                                         Intrebare,
                                         ((R, C), Q, Dir, Id)),
                       completare(integ(H, W, L, Vocab), Sol, Curr_Integ),

                       adauga_raspuns(Curr_Integ, R_list, ((R, C), Q, Dir, Id), S_Integ), !.

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% pentru Bonus:
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), ?Intrebare, ?Lungime)
%
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).
parcurge_spatiu(integ(_, _, L, _),
                ((R, C), _, j, _),
                0) :- Rj is R + 1, (member(((Rj, C), x), L); member(((Rj, C), [_|_]), L)), !. % am ajuns la capat

parcurge_spatiu(integ(_, _, L, _),
                ((R, C), _, d, _),
                0) :- Cd is C + 1, (member(((R, Cd), x), L); member(((R, Cd), [_|_]), L)), !. % am ajuns la capat

parcurge_spatiu(Integ,
                ((R, C), Q, j, Id),
                Sol_lg) :- Rj is R + 1,
                           parcurge_spatiu(Integ, 
                                           ((Rj, C), Q, j, Id),
                                           Curr_lg),
                            Sol_lg is 1 + Curr_lg, !.

parcurge_spatiu(Integ,
                ((R, C), Q, d, Id),
                Sol_lg) :- Cd is C + 1,
                           parcurge_spatiu(Integ, 
                                           ((R, Cd), Q, d, Id),
                                           Curr_lg),
                            Sol_lg is 1 + Curr_lg, !.

lungime_spatiu(integ(H, W, L, Vocab), 
               Intrebare, 
               Lung) :- intrebari(integ(H, W, L, Vocab), L_qs),
                        gaseste_intrebare(L_qs, 
                                         Intrebare,
                                         ((R, C), Q, Dir, Id)),
                        parcurge_spatiu(integ(H, W, L, Vocab), ((R, C), Q, Dir, Id), Curr_Lung),
                        Lung is Curr_Lung.

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% pentru Bonus:
% intersectie(integ(+H, +W, +Lista, +Voc), ?I1, ?Poz1, ?I2, ?Poz2)
%
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersecteaza_intrebari(((R1, C1), d),
                        L1,

                        ((R2, C2), j),
                        L2,

                        (R1, C2)) :- (C2 - C1 =< L1),
                                     (C2 - C1 > 0),

                                     (R1 - R2 =< L2),
                                     (R1 - R2 > 0).

intersecteaza_intrebari(((R1, C1), j),
                        L1,

                        ((R2, C2), d),
                        L2,

                        (R2, C1)) :- (C1 - C2 =< L2),
                                     (C1 - C2 > 0),

                                     (R2 - R1 =< L1),
                                     (R2 - R1 > 0).

gaseste_index(((R, _), j), (Rx, _), Pos) :- Pos is Rx - R - 1.
gaseste_index(((_, C), d), (_, Cy), Pos) :- Pos is Cy - C - 1.

intersectie(integ(H, W, L, Vocab), 
            I1, 
            P1, 
            I2, 
            P2) :- intrebari(integ(H, W, L, Vocab), L_qs),
                   gaseste_intrebare(L_qs, 
                                     I1,
                                     ((R1, C1), Q1, Dir1, Id1)),
                   gaseste_intrebare(L_qs, 
                                     I2,
                                     ((R2, C2), Q2, Dir2, Id2)),

                  parcurge_spatiu(integ(H, W, L, Vocab), ((R1, C1), Q1, Dir1, Id1), L1),
                  parcurge_spatiu(integ(H, W, L, Vocab), ((R2, C2), Q2, Dir2, Id2), L2),

                  intersecteaza_intrebari(((R1, C1), Dir1), L1,
                                          ((R2, C2), Dir2), L2,
                                          (Rx, Cy)),

                  gaseste_index(((R1, C1), Dir1), (Rx, Cy), P11),
                  gaseste_index(((R2, C2), Dir2), (Rx, Cy), P22),

                  P1 is P11,
                  P2 is P22.

my_intersect(Integ,
            L_qs,
            I1, 
            P1, 
            I2, 
            P2) :-

                  member(((R1, C1), I1, Dir1, Id1), L_qs),
                  member(((R2, C2), I2, Dir2, Id2), L_qs),

                  parcurge_spatiu(Integ, ((R1, C1), I1, Dir1, Id1), L1),
                  parcurge_spatiu(Integ, ((R2, C2), I2, Dir2, Id2), L2),

                  intersecteaza_intrebari(((R1, C1), Dir1), L1,
                                          ((R2, C2), Dir2), L2,
                                          (Rx, Cy)),

                  gaseste_index(((R1, C1), Dir1), (Rx, Cy), P11),
                  gaseste_index(((R2, C2), Dir2), (Rx, Cy), P22),

                  P1 is P11,
                  P2 is P22.

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de atomi, fiecare atom
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
verif_posibil_rasp(_, [], []).
verif_posibil_rasp(LQ, [Cuv | Vocab], [R | Sol]) :- atom_chars(Cuv, R), length(R, LQ), !, verif_posibil_rasp(LQ, Vocab, Sol).
verif_posibil_rasp(LQ, [_ | Vocab], Sol) :- verif_posibil_rasp(LQ, Vocab, Sol).


cauta_cuvinte(integ(_, _, _, _), [], []).
cauta_cuvinte(integ(H, W, L, Vocab), [((R, C), Q, Dir, Id) | L_qs], [(Q, Rasp) | Sols]) :- parcurge_spatiu(integ(H, W, L, Vocab),
                                                                                                          ((R, C), Q, Dir, Id),
                                                                                                          Lung),
                                                                                           verif_posibil_rasp(Lung, Vocab, Rasp),
                                                                                           cauta_cuvinte(integ(H, W, L, Vocab), L_qs, Sols), !.

solutii_posibile(integ(H, W, L, Vocab), Sols) :- intrebari(integ(H, W, L, Vocab), L_qs),
                                                 cauta_cuvinte(integ(H, W, L, Vocab), L_qs, Sols).

my_sols(integ(H, W, L, Vocab), Sols, SL_qs) :- intrebari(integ(H, W, L, Vocab), L_qs),
                                              my_q_sort(L_qs, SL_qs),
                                              cauta_cuvinte(integ(H, W, L, Vocab), SL_qs, Sols).

my_q_sort(L, S) :- q_sort(L, [], S).

pivoting(_, [], [], []).
pivoting(((R, C), Q, Dir, Id), [((Rx, Cx), Qx, Dirx, Idx) | T], [((Rx, Cx), Qx, Dirx, Idx) | L], G) :- atom_chars(Q, CQ),
                                                                                                       atom_chars(Qx, CQx),

                                                                                                       length(CQ, LQ),
                                                                                                       length(CQx, LQx),

                                                                                                       LQ =< LQx, !,
                                                                                                       pivoting(((R, C), Q, Dir, Id), T, L, G).

pivoting(((R, C), Q, Dir, Id), [((Rx, Cx), Qx, Dirx, Idx) | T], L, [((Rx, Cx), Qx, Dirx, Idx) | G]) :- atom_chars(Q, CQ),
                                                                                                       atom_chars(Qx, CQx),

                                                                                                       length(CQ, LQ),
                                                                                                       length(CQx, LQx),

                                                                                                       LQ > LQx, !,
                                                                                                   
                                                                                                       pivoting(((R, C), Q, Dir, Id), T, L, G).
% Ref : http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
q_sort([], Acc, Acc).
q_sort([((R, C), Q, Dir, Id) | T], Acc, Sorted):- pivoting(((R, C), Q, Dir, Id), T, L1, L2),
                                                  q_sort(L1, Acc, Sorted1),
                                                  q_sort(L2, [((R, C), Q, Dir, Id) | Sorted1], Sorted).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.

verif_valid_sol(_, _, _, []).
% verif_valid_sol(Q, X, L_int, [(Q2, X2) | Sol]) :- X == X2, !, fail.

verif_valid_sol(Q, X, L_int, [(Q2, X2) | Sol]) :- member((Q, P1, Q2, P2), L_int), !,

                                                  nth0(P1, X, L1),
                                                  nth0(P2, X2, L2),

                                                  L1 == L2, verif_valid_sol(Q, X, L_int, Sol).

% verif_valid_sol(Q, X, L_int, [(Q2, X2) | Sol]) :- member((Q2, P2, Q, P1), L_int), !,

%                                                   nth0(P1, X, L1),
%                                                   nth0(P2, X2, L2),

%                                                   L1 == L2, verif_valid_sol(Q, X, L_int, Sol).

verif_valid_sol(Q, X, L_int, [_ | Sol]) :- verif_valid_sol(Q, X, L_int, Sol).

cauta_solutii(_,[], _, []).
cauta_solutii(W, [(Q, Rasp) | Solutii], L_qs, [(Q, X) | Curr_Sol]) :- findall((Q, P1, Q2, P2), my_intersect(W, L_qs, Q, P1, Q2, P2), L_int),
                                                                    cauta_solutii(W, Solutii, L_qs, Curr_Sol),
                                                                    member(X, Rasp), \+member((_, X), Curr_Sol),
                                                                    verif_valid_sol(Q, X, L_int, Curr_Sol).

rezolvare(W, Solutie) :- my_sols(W, Solutii, L_qs),

                        %  findall((Q1, P1, Q2, P2), ( % member((_, Q1, d, _), L_qs),
                        %                             my_intersect(W, L_qs, Q1, P1, Q2, P2)), L_int), % se pot intersecta doar intrebari 
                        %                                                                             % avand directii diferite
                                                 
                         cauta_solutii(W, Solutii, L_qs, Sol),
                         findall((Q, R), (member((Q, Rasp), Sol), atom_chars(R, Rasp)), Solutie).
                                                    

%% Helpers
% integrama(0, W), intrebari(W, L_qs), member((_, Q1, d, _), L_qs), intersectie(W, Q1, P1, Q2, P2), Sol = (Q1, P1, Q2, P2).
% integrama(0, W), intrebari(W, L_qs), findall((Q1, P1, Q2, P2), (member((_, Q1, d, _), L_qs), intersectie(W, Q1, P1, Q2, P2)), L).
