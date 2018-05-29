:- ensure_loaded('common.pl').
:- ensure_loaded('../sudex_toledo.pl').

solve(P) :- makedomains(P), solve_sudoku(P), enum(P).

solve_sudoku(P) :- solve_rows(P), solve_cols(P), solve_boxes(P).


box_cols_helper([], BL, BL, R, R).
box_cols_helper([[H1, H2, H3|R]|L], BoxListIn, BoxListOut, RestIn, RestOut) :-
    box_cols_helper(L, [H1, H2, H3|BoxListIn], BoxListOut, [R|RestIn], RestOut).

box_cols([[]|_]).
box_cols(L) :-
    box_cols_helper(L, [], BoxList, [], Rest),
    all_dif(BoxList),
    box_cols(Rest).


solve_boxes([]).
solve_boxes([R1, R2, R3|Rest]) :-
    box_cols([R1, R2, R3]),
    solve_boxes(Rest).


solve_all([], Times, Times).
solve_all([P-Name|R], TimesIn, TimesOut) :-
    writeln(Name),
    Start is cputime,
    solve(P),
    End is cputime,
    writeln(P),
    Time is End - Start,
    write('time elapsed: '),
    writeln(Time),
    write('\n'),
    solve_all(R, [Time|TimesIn], TimesOut).

% solvse all sudokus
solutions :-
    findall(P-Name, puzzles(P, Name), L),
    solve_all(L, [], Times),
    sumlist(Times, S),
    writeln(Times),
    writeln(S).