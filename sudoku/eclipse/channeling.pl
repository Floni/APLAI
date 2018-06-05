:- lib(ic).
%:- import alldifferent/1 from ic_global.
:- lib(matrix_util).

:- ensure_loaded('common.pl').
:- ensure_loaded('../sudex_toledo.pl').

:- ensure_loaded('original_viewpoint.pl').
:- ensure_loaded('new_viewpoint.pl').

% channeling between the original and the new viewpoint
% the viewpoints have their columns and values swapped
channeling(Porig, Pnew) :-
    ( for(Row, 1, 9),
      param(Porig, Pnew)
    do
        ( for(Col, 1, 9),
          param(Porig, Pnew, Row)
        do
            ( for(Val, 1, 9),
              param(Porig, Pnew, Row, Col)
            do
                Vorig is Porig[Row, Col],
                Cnew is Pnew[Row, Val],
                #=(Vorig, Val, B),
                #=(Col, Cnew, B)
            )
        )
    ).

% solves the given sudoku using both channeled viewpoints
chan_solve(P, B) :-
    P :: 1..9,
    to_matrix(P, Parray),

    to_new_viewpoint(Parray, Pnew),
    Pnew :: 1..9,

    new_constraints(Pnew),
    orig_constraints(Parray),
    channeling(Parray, Pnew),

    array_flat(2, Parray, Pflat),
    search(Pflat, 0, first_fail, indomain, complete, [backtrack(B)]).

% solves all sudokus using the channeled viewpoints
solutions_chan :-
    findall(P-Name, puzzles(P, Name), L),
    ( foreach(P-Name, L),
      foreach(Time, Times),
      foreach(Back, Backs)
    do
        writeln(Name),
        cputime(Start),
        chan_solve(P, Back),
        cputime(End),
        Time is End - Start,
        print_sudoku(P),
        write('backtracks: '),
         writeln(Back),
        write('time elapsed: '),
         writeln(Time),
        nl
    ),
    sum(Times, S),
    sum(Backs, B),
    write('total backs: '), writeln(B),
    write('total time: '), writeln(S).