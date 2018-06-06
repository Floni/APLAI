:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).

:- op(700,xfx,'in').
:- op(700,xfx,'not_in').
:- op(700,xfx,'not_eq').
:- op(700,xfx,'not_box_eq').

:- chr_constraint enum_helper/1, in/2, not_eq/2, not_box_eq/2, indexes/3.

% adds row contraints for the given rows
solve_rows([]).
solve_rows([Row|R]) :- all_dif(Row), solve_rows(R).

% generates the first column of the given rows
col_generator([], Heads, Heads, Rests, Rests).
col_generator([[H|R]| L], HeadsIn, HeadsOut, RestsIn, RestsOut) :-
    col_generator(L, [H|HeadsIn], HeadsOut, [R|RestsIn], RestsOut).

% adds column contraints for the given sudoku
solve_cols([[]| _]).
solve_cols(P) :- 
    col_generator(P, [], Col, [], Rest),
    all_dif(Col),
    solve_cols(Rest).

% adds all different contraint for the given list
all_dif([]).
all_dif([El|NextElems]) :-
    El not_in NextElems,
    all_dif(NextElems).

% checks whether an element is not in a list
_ not_in [].
X not_in [E|L] :- X not_eq E, X not_in L. 

% not equal contraint
% if both variables are grounded they should not be equal
X not_eq N          <=> number(X), number(N) | X =\= N.
% change domain of L:
X not_eq N \ N in L <=> number(X), delete(L, X, L1), L \== L1 | N in L1.
% change domain of X:
X not_eq N \ X in L <=> number(N), delete(L, N, L1), L \== L1 | X in L1.

% box contraint
% two variables with the same value should be in different boxes
X not_box_eq N          <=> number(X), number(N) | 
                            X1 is X - 1, N1 is N - 1,
                            BX is X1 // 3, BN is N1 // 3,
                            BX =\= BN.
% change domain of L:
X not_box_eq N \ N in L <=> number(X), X1 is X - 1, BX is X1 // 3,
                            XS is BX * 3 + 1, XS1 is XS + 1, XS2 is XS + 2,
                            delete(L, XS, L1), delete(L1, XS1, L2), delete(L2, XS2, L3),
                            L \== L3 | N in L3.
% change domain of X:
N not_box_eq X \ N in L <=> number(X), X1 is X - 1, BX is X1 // 3,
                            XS is BX * 3 + 1, XS1 is XS + 1, XS2 is XS + 2,
                            delete(L, XS, L1), delete(L1, XS1, L2), delete(L2, XS2, L3),
                            L \== L3 | N in L3.

% indexes contraint of value, index and list
indexes(N, X, L)  <=> number(X), number(N) | nth1(X, L, N).

% enumerate variable values from their domain
enum([]).
enum([X|R]) :- enum_helper(X), enum(R).

enum_helper([])             <=> true.
enum_helper([X|R])          <=> number(X) | enum_helper(R).
enum_helper([X|R]), X in L  <=> member(X, L), enum_helper(R).

% X in L
_ in []             <=> fail.
X in [X1]           <=> X=X1.
X in L              <=> number(X) | member(X, L).

upto(0, []).
upto(N, [N|L]) :- N > 0, N1 is N - 1, upto(N1, L).

% generate the sudoku variables domains
makedomains(P) :- upto(9, D), domain(P, D).

domain_helper([], _).
domain_helper([X|L], D) :- X in D, domain_helper(L, D).

domain([], _).
domain([SL|L], D) :- domain_helper(SL, D), domain(L, D).
