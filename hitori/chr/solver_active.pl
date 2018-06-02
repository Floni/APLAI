:- use_module(library(chr)).
%:- chr_option(debug,off).
%:- chr_option(optimize,full).

:- ensure_loaded('../puzzles.pl').
:- op(700, xfx, 'can_be').

:- chr_constraint
    possible_val/3, % row, col, Pval  -> indicates possible values in grid
    is_black/2,     % row, col        -> indicates black in grid
    has_val/3,      % row, col, val   -> indicates fixed value in grid
    enum_cols/1,    % [row]           -> for enumerating domain
    can_be/2,       % Pval can_be val -> sets domain
    size/1,         % size            -> size of grid
    chain/2,        % [row-col], id   -> identifies connected chain of blacks
    end/0.

% make the domain of the variables within a row
make_domains_row(_, _, [], []).
make_domains_row(Nrow, Ncol, [V|Rest], [Val|OutRest]) :-
    possible_val(Nrow, Ncol, Val),
    Val can_be V,
    Ncol1 is Ncol + 1,
    make_domains_row(Nrow, Ncol1, Rest, OutRest).

% make the domain of the hitori puzzle variables
make_domains(_, [], []).
make_domains(Nrow, [Row|Rest], [OutRow|OutRest]) :-
    make_domains_row(Nrow, 1, Row, OutRow),
    Nrow1 is Nrow + 1,
    make_domains(Nrow1, Rest, OutRest).



% the two cells are adjacent
adjacent(Row1, Col1, Row2, Col2) :-
    RowDist is abs(Row2 - Row1),
    ColDist is abs(Col2 - Col1),
    TotalDist is RowDist + ColDist,
    TotalDist =:= 1.

% the two cells are not adjacent
not_adjacent(Row1, Col1, Row2, Col2) :-
    \+ adjacent(Row1, Col1, Row2, Col2).

% the two cell are cornering
corner(Row1, Col1, Row2, Col2) :-
    RowDist is abs(Row2 - Row1),
    ColDist is abs(Col2 - Col1),
    RowDist =:= ColDist, RowDist =:= 1.

% checks wether the two chains corner eachother
chain_corners(L1, L2) :-
    member(R1-C1, L1),
    member(R2-C2, L2),
    corner(R1, C1, R2, C2), !.

% checks if Row-Col would create a loop in the chain
creates_loop(edge, Size, L, Row, Col) :-
    (Row =:= 1; Row =:= Size; Col =:= 1; Col =:= Size),
    member(R1-C1, L),
    corner(R1, C1, Row, Col), !.

creates_loop(_, _, L, Row, Col) :-
    member(R1-C1, L),
    member(R2-C2, L),
    R1-C1 \== R2-C2,
    corner(R1, C1, Row, Col),
    corner(R2, C2, Row, Col), !.

% prefer edge when merging id's of chains:
merge_id(edge, _, edge) :- !.
merge_id(_, edge, edge) :- !.
merge_id(Id1, _, Id1).

% black cell forms singleton chain:
is_black(1, Col)                ==> chain([1-Col], edge).
is_black(Row, 1)                ==> Row \== 1 | chain([Row-1], edge).
is_black(Size, Col), size(Size) ==> Col \== 1 | chain([Size-Col], edge).
is_black(Row, Size), size(Size) ==> Row \== 1, Row \== Size | chain([Row-Size], edge).
is_black(Row, Col), size(Size)  ==> Row \== 1, Row \== Size, Col \== Size, Col \== 1
                                    | Id is Row * Size + Col, chain([Row-Col], Id).

% merge chains:
chain(L1, edge), chain(L2, edge) <=> append(L1, L2, L), chain(L, edge).
chain(L1, Id1), chain(L2, Id2)   <=> chain_corners(L1, L2) | merge_id(Id1, Id2, Id), append(L1, L2, L), chain(L, Id).

% prevent creating 'loops' in chains:
possible_val(Row, Col, Pval), size(Size) \ chain(L, Id), Pval can_be N  <=> creates_loop(Id, Size, L, Row, Col) | chain(L, Id), Pval = N.

% sets the grid cells
possible_val(Row, Col, Val) <=> number(Val), Val =:= 0 | is_black(Row, Col).
possible_val(Row, Col, Val) <=> number(Val), Val > 0   | has_val(Row, Col, Val).

% no same values in row/col
has_val(Row1, _, Val), possible_val(Row1, _, Pval) \ Pval can_be Val <=> Pval = 0.
has_val(_, Col1, Val), possible_val(_, Col1, Pval) \ Pval can_be Val <=> Pval = 0.
has_val(Row1, Col1, Val), has_val(Row2, Col2, Val) ==> Row1 =\= Row2, Col1 =\= Col2.

% no black cells next to eachother
is_black(Row1, Col1), possible_val(Row2, Col2, Pval) \ Pval can_be N <=> adjacent(Row1, Col1, Row2, Col2)
                                                                       | Pval = N.
% two black cells are not adjacent
is_black(Row1, Col1), is_black(Row2, Col2) ==> not_adjacent(Row1, Col1, Row2, Col2).

% enumerates the elements within a row
enum_cols([])                   <=> true.
enum_cols([V|Rest])             <=> number(V) | enum_cols(Rest).
enum_cols([V|Rest]), V can_be N <=> member(V, [N, 0]), enum_cols(Rest).

% enumerates the rows
enum_rows([]).
enum_rows([Row|Rest]) :-
    enum_cols(Row),
    enum_rows(Rest).

% removes all constraints from stores.
% needed for solutions to work correctly.
end \ possible_val(_, _, _) <=> true.
end \ is_black(_, _) <=> true.
end \ has_val(_, _, _) <=> true.
end \ enum_cols(_) <=> true.
end \ can_be(_, _) <=> true.
end \ size(_) <=> true.
end \ chain(_, _) <=> true.
end <=> true.

% solves the given hitori puzzle
solve(Size, P, Pout) :-
    size(Size),
    make_domains(1, P, Pout),
    enum_rows(Pout),
    end.

print_val(0) :- write('x'), !.
print_val(V) :- write(V).

print_row([]).
print_row([V|Rest]) :-
    print_val(V), write(' '),
    print_row(Rest).

print_hitori([]).
print_hitori([Row|Rest]) :-
    print_row(Row), nl,
    print_hitori(Rest).

% solves the list of hitori puzzles and returns the time each one took
solve_all([], Times, Times).
solve_all([Id-Size-P|R], TimesIn, TimesOut) :-
    writeln(Id),
    Start is cputime,
    solve(Size, P, Pout),
    End is cputime,
    writeln(Pout),
    Time is End - Start,
    write('time elapsed: '),
    writeln(Time),
    write('\n'),
    solve_all(R, [Time|TimesIn], TimesOut).

% solves all hitori puzzles and prints total elapsed time
solutions :-
    findall(Id-Size-P, puzzle(Id, Size, P), L),
    solve_all(L, [], Times),
    sumlist(Times, S),
    writeln(Times),
    writeln(S).