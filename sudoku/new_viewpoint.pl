:- lib(ic).
:- lib(matrix_util).

:- ['utils.pl'].
:- ['sudex_toledo.pl'].

% constraint that asserts: A == B // N
div_constraint(A, B, N) :-
    B #> A * N,
    B #=< A * N + N.

check_box_new(P) :-
    ( foreach(BR, [1, 4, 7]),
      param(P)
    do
        ( for(BV, 1, 9),
          param(P, BR)
        do
            Box is P[BR..BR+2, BV],
            %writeln(Box),
            dim(BoxCols, [3]),
            ( foreachelem(Col, Box),
              foreachelem(BC, BoxCols)
            do
                %writeln(Col-BC),
                div_constraint(BC, Col, 3)
            ),
            %writeln(BoxCols),
            alldifferent(BoxCols)
        )
    ).

% converts a sudoku matrix to the new viewpoit
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

% solves the given sudoku using the new viewpoint
% (first converts the list to a matrix and then solves)
solve_new(P) :-
    to_matrix(P, Parray),
    to_new_viewpoint(Parray, Pout),
    Pout :: 1..9,

    check_rows(Pout),
    check_cols(Pout),
    check_box_new(Pout),

    array_flat(2, Pout, Pflat),
    labeling(Pflat),

    writeln(Pout).

solutions_new :-
    findall(P-Name, puzzles(P, Name), L),
    ( foreach(P-Name, L)
    do
        writeln(Name),
        cputime(Start),
        solve_new(P),
        cputime(End),
        Diff is End - Start,
        %print_sudoku(P),
        write('time elapsed: '),
        writeln(Diff),
        write('\n')
    ).