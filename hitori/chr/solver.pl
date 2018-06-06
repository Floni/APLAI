:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).

:- ensure_loaded('../puzzles.pl').
:- op(700,xfx,'can_be').

:- chr_constraint possible_val/3, is_black/2, has_val/3, enum_cols/1, can_be/2, marked/2, not_marked/2, start/0, end/0, size/1.

:- include('common.pl').

% mark one white cell and unmark all other white cells
has_val(Row, Col, _) \ start            <=> marked(Row, Col).
has_val(Row, Col, _)                    ==> not_marked(Row, Col).
% marking a cell removes the fact that it is not marked
marked(Row, Col) \ not_marked(Row, Col) <=> true.

% mark a cell if it is not black and is adjacent to a marked cell
marked(Row1, Col1), has_val(Row2, Col2, _) \ not_marked(Row2, Col2) <=> adjacent(Row1, Col1, Row2, Col2)
                                                                        | marked(Row2, Col2).

% If some white cell is not marked at the end the solution does not respect the connected whites constraint
not_marked(_, _), end <=> fail.
% remove all remaining contraints at the end so they do not interfere with the next puzzle
end \ possible_val(_, _, _) <=> true.
end \ is_black(_, _) <=> true.
end \ has_val(_, _, _) <=> true.
end \ marked(_, _) <=> true.
end <=> true.

% solves the given hitori puzzle
solve(Size, P, Pout) :-
    size(Size),
    start,
    make_domains(1, P, Pout),
    enum_rows(Pout),
    end.

% solves the list of hitori puzzles and returns the time each one took
solve_all([], Times, Times).
solve_all([Id-Size-P|R], TimesIn, TimesOut) :-
    writeln(Id),
    Start is cputime,
    solve(Size, P, Pout),
    End is cputime,
    print_hitori(Pout),
    Time is End - Start,
    write('time elapsed: '),
    writeln(Time), nl,
    solve_all(R, [Time|TimesIn], TimesOut).

% solves all hitori puzzles and prints total elapsed time
solutions :-
    findall(Id-Size-P, puzzle(Id, Size, P), L),
    solve_all(L, [], Times),
    sumlist(Times, S),
    writeln(Times),
    writeln(S).