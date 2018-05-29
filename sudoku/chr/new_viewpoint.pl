:- use_module(library(lists)).

:- ensure_loaded('common.pl').
:- ensure_loaded('../sudex_toledo.pl').

solve_new(P) :- 
    to_new_viewpoint(P, Pnew), 
    makedomains(Pnew), 
    solve_sudoku_new(Pnew), 
    enum(Pnew),
    writeln(Pnew).

solve_sudoku_new(P) :- solve_rows(P), solve_cols(P), solve_new_boxes(P).

boxes_get_heads([], Heads, Heads, Rest, Rest).
boxes_get_heads([[H|HRest]|L], HeadsIn, HeadsOut, RestIn, RestOut) :-
    boxes_get_heads(L, [H|HeadsIn], HeadsOut, [HRest|RestIn], RestOut).

boxes_new_cols([[]|_]).
boxes_new_cols(L) :-
    boxes_get_heads(L, [], [H1, H2, H3], [], Rest),
    H1 not_box_eq H2, H1 not_box_eq H3,
    H2 not_box_eq H3,
    boxes_new_cols(Rest).

solve_new_boxes([]).
solve_new_boxes([R1, R2, R3|Rest]) :-
    boxes_new_cols([R1, R2, R3]),
    solve_new_boxes(Rest).

to_new_viewpoint([], []).
to_new_viewpoint([Row|Rest], [RowOut|RestOut]) :- 
    viewpoint_row(Row, RowOut), !,
    to_new_viewpoint(Rest, RestOut).

viewpoint_row(Row, RowOut) :-
    length(RowOut, 9),
    viewpoint_row(1, Row, RowOut).

viewpoint_row(_, [], _).
viewpoint_row(C, [V|Rest], Rout) :-
    number(V), !,
    nth1(V, Rout, C),
    C1 is C + 1,
    viewpoint_row(C1, Rest, Rout).

viewpoint_row(C, [_|Rest], Rout) :-
    C1 is C + 1,
    viewpoint_row(C1, Rest, Rout).


solve_all_new([], Times, Times).
solve_all_new([P-Name|R], TimesIn, TimesOut) :-
    writeln(Name),
    Start is cputime,
    solve_new(P),
    End is cputime,
    %writeln(P),
    Time is End - Start,
    write('time elapsed: '),
    writeln(Time),
    write('\n'),
    solve_all_new(R, [Time|TimesIn], TimesOut).

% solvse all sudokus
solutions_new :-
    findall(P-Name, puzzles(P, Name), L),
    solve_all_new(L, [], Times),
    sumlist(Times, S),
    writeln(Times),
    writeln(S).