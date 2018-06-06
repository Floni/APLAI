:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).

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

% checks if Row-Col would create a loop with the edge
creates_loop(edge, Size, L, Row, Col) :-
    (Row =:= 1; Row =:= Size; Col =:= 1; Col =:= Size),
    member(R1-C1, L),
    corner(R1, C1, Row, Col), !.

% checks if the given cell would create a loop in the chain
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
% not an edge:
is_black(Row, Col), size(Size)  ==> Row \== 1, Row \== Size, Col \== Size, Col \== 1
                                    | Id is Row * Size + Col, chain([Row-Col], Id).

% merge chains:
chain(L1, edge), chain(L2, edge) <=> append(L1, L2, L), chain(L, edge).
chain(L1, Id1), chain(L2, Id2)   <=> chain_corners(L1, L2) | merge_id(Id1, Id2, Id), append(L1, L2, L), chain(L, Id).

% prevent creating 'loops' in chains:
possible_val(Row, Col, Pval), 
size(Size) \ chain(L, Id), Pval can_be N  <=> creates_loop(Id, Size, L, Row, Col) | chain(L, Id), Pval = N.

:- include('common.pl').

% removes all constraints from store,
% needed for solve_all to work correctly.
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