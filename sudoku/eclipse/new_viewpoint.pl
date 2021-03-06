:- lib(ic).
%:- import alldifferent/1 from ic_global.
:- lib(matrix_util).

:- ensure_loaded('common.pl').
:- ensure_loaded('../sudex_toledo.pl').

% constraint that asserts: A == B // N
div_constraint(A, B, N) :-
    B #> A * N,
    B #=< A * N + N.

% check the box constraint in the new viewpoint
check_box_new(P) :-
    ( foreach(BR, [1, 4, 7]),
      param(P)
    do
        ( for(BV, 1, 9),
          param(P, BR)
        do
            Box is P[BR..BR+2, BV],
            dim(BoxCols, [3]),
            ( foreachelem(Col, Box),
              foreachelem(BC, BoxCols)
            do
                div_constraint(BC, Col, 3)
            ),
            alldifferent(BoxCols)
        )
    ).

% converts a sudoku matrix to the new viewpoint
to_new_viewpoint(P, Pout) :-
    dim(Pout, [9, 9]),
    ( for(Row, 1, 9),
      param(P, Pout)
    do
        ( for(Col, 1, 9),
          param(P, Pout, Row)
        do
            Val is P[Row, Col],
            (ground(Val) ->
                Col is Pout[Row, Val]
            ;
                true
            )
        )
    ).

% checks the contraints of the new viewpoint
new_constraints(Pout) :-
    check_rows(Pout),
    check_cols(Pout),
    check_box_new(Pout).

% solves the given sudoku using the new viewpoint
% (first converts the list to a matrix and then solves)
solve_new(P, Pout, B) :-
    to_matrix(P, Parray),
    to_new_viewpoint(Parray, Pout),

    Pout :: 1..9,
    new_constraints(Pout),

    array_flat(2, Pout, Pflat),
    search(Pflat, 0, input_order, indomain, complete, [backtrack(B)]).

% solvse all sudokus using the new viewpoint
solutions_new :-
    findall(P-Name, puzzles(P, Name), L),
    ( foreach(P-Name, L),
      foreach(Time, Times),
      foreach(Back, Backs)
    do
        writeln(Name),
        cputime(Start),
        solve_new(P, Pout, Back),
        cputime(End),
        writeln(Pout),

        Time is End - Start,
        write('backtracks: '), writeln(Back),
        write('time elapsed: '), writeln(Time),
        write('\n')
    ),
    sum(Times, S),
    sum(Backs, B),
    write('total backs: '), writeln(B),
    write('total time: '), writeln(S).