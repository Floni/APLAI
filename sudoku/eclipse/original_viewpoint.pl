:- lib(ic).
%:- import alldifferent/1 from ic_global.
:- lib(matrix_util).

:- ensure_loaded('common.pl').
:- ensure_loaded('../sudex_toledo.pl').


% check if all 3x3 boxes in P contain all different values.
check_boxes(P) :-
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

% check all constraints
orig_constraints(Parray) :-
    check_rows(Parray),
    check_cols(Parray),
    check_boxes(Parray).

% solves the puzzle P
solve(P, B) :-
    P :: 1..9,
    to_matrix(P, Parray),
    orig_constraints(Parray),
    array_flat(2, Parray, Pflat),
    search(Pflat, 0, first_fail, indomain, complete, [backtrack(B)]).

% solves all sudokus while printing their name, solution and runtime
solutions :-
    findall(P-Name, puzzles(P, Name), L),
    ( foreach(P-Name, L),
      foreach(Time, Times),
      foreach(Back, Backs)
    do
        writeln(Name),
        cputime(Start),
        solve(P, Back),
        cputime(End),
        Time is End - Start,
        print_sudoku(P),
        write('backtracks: '),
        write(Back),
        write('time elapsed: '),
        writeln(Time), nl
    ),
    sum(Times, S),
    sum(Backs, B),
    write('total backs: '), writeln(B),
    write('total time: '), writeln(S).