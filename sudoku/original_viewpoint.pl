:- lib(ic).
:- lib(matrix_util).

:- ['sudex_toledo.pl'].

% check if all rows in P contain different values.
checkRows(P) :-
    ( for(I, 1, 9),
      param(P)
    do
        Row is P[I],
        alldifferent(Row)
    ).

% check if all columns in P contain different values.
checkCols(P) :-
    ( for(I, 1, 9),
      param(P)
    do
        Col is P[1..9, I],
        alldifferent(Col)
    ).

% check if all 3x3 boxes in P contain different values.
checkBoxes(P) :-
    ( foreach(BR, [1, 4, 7]),
      param(P)
    do
        ( foreach(BC, [1, 4, 7]),
          param(P, BR)
        do
            Box is P[BR..BR+2, BC..BC+2],
            array_flat(2, Box, BoxFlat),
            alldifferent(BoxFlat)
      )
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

% solves the puzzle P
solve(P) :-
    P :: 1..9,
    to_matrix(P, Parray),
    checkRows(Parray),
    checkCols(Parray),
    checkBoxes(Parray),
    array_flat(2, Parray, Pflat),
    labeling(Pflat).

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

% solves all sudoku's while printing their name and runtime
solutions :-
    findall(P-Name, puzzles(P, Name), L),
    ( foreach(P-Name, L)
    do
        writeln(Name),
        cputime(Start),
        solve(P),
        cputime(End),
        Diff is End - Start,
        print_sudoku(P),
        write('time elapsed: '),
        writeln(Diff),
        write('\n')
    ).