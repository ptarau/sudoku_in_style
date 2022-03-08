/* 
    solves or generates all possible
    square sudoku problems of size 9 specified by a grid
    input: grid as  alist lists with _ marking free spots
    yield: one or more filled out grids as answers
*/

% examples of use

go:-solve(escargot).

go1:-solve(problem1).

% generates all 6,670,903,752,021,072,936,960 problems
gox:-solve(problem).

solve(Problem):-
  call(Problem,Rows), % state problem
  maplist(ppp, Rows),nl,% show a problem
  sudoku(Rows), % solve it
  maplist(ppp, Rows),nl,% show a solution
  fail.

sudoku(Rows) :-
   build(Rows,Cols, Blocks),
   app([Rows,Cols,Blocks],Lists),
   constrain(Lists).

constrain(Lists):-
   lists2edges(Lists,Edges),
   keygroups(Edges,KGs),
   mpp(KGs),nl,
   maplist(pick,KGs).

% describe problem
build(Rows,Cols,Blocks):-
  Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
  mtranspose(Rows, Cols),
  blocks(As, Bs, Cs,Bss1),
  blocks(Ds, Es, Fs,Bss2),
  blocks(Gs, Hs, Is,Bss3),
  app([Bss1,Bss2,Bss3],Blocks).

blocks([], [], [],[]).
blocks([N1,N2,N3|Ns1],[N4,N5,N6|Ns2], [N7,N8,N9|Ns3],
                      [[N1,N2,N3,N4,N5,N6,N7,N8,N9]|Bs]):-
         blocks(Ns1, Ns2, Ns3,Bs).

mtranspose([],[]):- !.
mtranspose([Xs],Css):- !,to_columns(Xs,Css).
mtranspose([Xs|Xss],Css2):- !,
  mtranspose(Xss,Css1),
  to_columns(Xs,Css1,Css2).

to_columns([], []).
to_columns([X|Xs],[[X]|Zs]):-to_columns(Xs,Zs).

to_columns([],Css,Css).
to_columns([X|Xs],[Cs|Css1],[[X|Cs]|Css2]) :- to_columns(Xs,Css1,Css2).

% constrainer components

pick(X-Xs):- between(1,9,X),\+ member_const(X,Xs).

member_const(X,[Y|_]):-nonvar(Y),X=:=Y,!.
member_const(X,[_|Xs]):-member_const(X,Xs).

lists2edges(Xss,Ys):-
   maplist(difpairs,Xss,Dss),
   app(Dss,Ds),
   sort(Ds,Ys).

difpairs(Xs,Zs):-
  bagof(X-Y,X^Y^(member(X,Xs),member(Y,Xs),X\==Y),Zs).

keygroups(Ps,KXs):-
   keysort(Ps,Ss),
   group_pairs_by_key(Ss,KXs).


% helpers

app([], []).
app([L|Ls], As) :- append(L, Ws, As), app(Ls, Ws).

ppp(X):-portray_clause(X).

mpp(Xs):-numbervars(Xs,0,_),member(X,Xs),write(X),nl,fail;true.

problem(Rows):-
   length(Rows,9),
   maplist(same_length(Rows), Rows).

problem1([
     [2, 5, _, _, 3, _, 9, _, 1],
     [_, 1, _, _, _, 4, _, _, _],
     [4, _, 7, _, _, _, 2, _, 8],
     [_, _, 5, 2, _, _, _, _, _],
     [_, _, _, _, 9, 8, 1, _, _],
     [_, 4, _, _, _, 3, _, _, _],
     [_, _, _, 3, 6, _, _, 7, 2],
     [_, 7, _, _, _, _, _, _, 3],
     [9, _, 3, _, _, _, 6, _, 4]
   ]).

escargot([
      [1, _, _, _, _, 7, _, 9, _],
      [_, 3, _, _, 2, _, _, _, 8],
      [_, _, 9, 6, _, _, 5, _, _],
      [_, _, 5, 3, _, _, 9, _, _],
      [_, 1, _, _, 8, _, _, _, 2],
      [6, _, _, _, _, 4, _, _, _],
      [3, _, _, _, _, _, _, 1, _],
      [_, 4, _, _, _, _, _, _, 7],
      [_, _, 7, _, _, _, 3, _, _]
  ]).

to_board(Atom,Xss):-
  atom_chars(Atom,Cs),
  maplist(to_vars,Cs,Vs),
  reshape(Xss,Vs,[]).

to_vars('.',_):-!.
to_vars(S,N):-atom_number(S,N).

reshape([])-->[].
reshape([[A, B, C, D, E, F, G, H, I]|Xs])-->
   [A, B, C, D, E, F, G, H, I],
   reshape(Xs).

escargot_(Xss):-
    E='1....7.9..3..2...8..96..5....53..9...1..8...26....4...3......1..4......7..7...3..',
    to_board(E,Xss).

% alternative simpler solver

all_dif(Xs,Es):-all_dif(Xs,Es,[]).

all_dif([])-->[].
all_dif([X|Xs])-->
   difs_of(Xs,X),
   all_dif(Xs).


difs_of([],_)-->[].
difs_of([Y|Xs],X)-->
   [X-Y,Y-X],
   difs_of(Xs,X).

alt_sudoku(Prob) :-
   call(Prob,Rows),
   maplist(ppp,Rows),nl,
   build(Rows,Cols, Blocks),
   app([Rows,Cols,Blocks],Difs),
   maplist(all_dif,Difs,Ess),
   app(Ess,Es),
   keygroups(Es,XXs),
   maplist(pick,XXs),
   maplist(ppp,Rows),nl,
   fail.

%/*
% for Prolog that do not implement these

group_pairs_by_key([], []).
group_pairs_by_key([M-N|T0],
                         [M-[N|TN]|T]) :-
    same_key(M, T0, TN, T1),
    group_pairs_by_key(T1, T).

same_key(M0,
               [M-N|T0],
               [N|TN],
               T) :-
    M0==M,
    !,
    same_key(M, T0, TN, T).
same_key(_, L, [], L).

portray_clause(X):-write(X),write('.'),nl.
%*/
