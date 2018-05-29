% check if all rows in P contain different values.
check_rows(P) :-
    ( for(I, 1, 9),
      param(P)
    do
        Row is P[I],
        alldifferent(Row)
    ).

% check if all columns in P contain different values.
check_cols(P) :-
    ( for(I, 1, 9),
      param(P)
    do
        Col is P[1..9, I],
        alldifferent(Col)
    ).

% converts a list of lists to an array of arrays
to_matrix(Lists, Matrix) :-
    flatten(Lists, Flats),
    dim(Matrix, [9, 9]),
    ( foreach(El, Flats),
      foreachelem(El, Matrix)
    do
        true
    ).

% pretty prints a sudoku
print_sudoku(P) :-
    ( foreach(El, P),
      for(I, 1, 9)
    do
        ( foreach(Val, El),
          for(J, 1, 9)
        do
            write(Val),
            M is J mod 3,
            (M =:= 0 -> write('  ') ; write(' '))
        ),
        write('\n'),
        M is I mod 3,
        (M =:= 0 -> writeln(' ') ; true)
    ).