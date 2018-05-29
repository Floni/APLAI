:- lib(ic).
:- lib(matrix_util).

:- ensure_loaded('common.pl').
:- ensure_loaded('../sudex_toledo.pl').

:- ensure_loaded('original_viewpoint.pl').
:- ensure_loaded('new_viewpoint.pl').


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

chan_solve(P) :-
    P :: 1..9,
    to_matrix(P, Parray),
    %dim(Pnew, [9, 9]),
    to_new_viewpoint(Parray, Pnew),
    Pnew :: 1..9,

    new_constraints(Pnew),
    orig_constraints(Parray),
    channeling(Parray, Pnew),

    array_flat(2, Parray, Pflat),
    labeling(Pflat).

solutions_chan :-
    findall(P-Name, puzzles(P, Name), L),
    ( foreach(P-Name, L),
      foreach(Time, Times)
    do
        writeln(Name),
        cputime(Start),
        chan_solve(P),
        cputime(End),
        Time is End - Start,
        print_sudoku(P),
        write('time elapsed: '),
        writeln(Time),
        write('\n')
    ),
    sum(Times, S),
    writeln(Times),
    writeln(S).