:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).

:- ensure_loaded('../puzzles.pl').
:- op(700,xfx,'can_be').

:- chr_constraint possible_val/3, is_black/2, has_val/3, enum_cols/1, can_be/2, marked/2, not_marked/2, start/0, end/0, size/1.

make_domains_row(_, _, [], []).
make_domains_row(Nrow, Ncol, [V|Rest], [Val|OutRest]) :-
    possible_val(Nrow, Ncol, Val),
    Val can_be V,
    Ncol1 is Ncol + 1,
    make_domains_row(Nrow, Ncol1, Rest, OutRest).

make_domains(_, [], []).
make_domains(Nrow, [Row|Rest], [OutRow|OutRest]) :-
    make_domains_row(Nrow, 1, Row, OutRow),
    Nrow1 is Nrow + 1,
    make_domains(Nrow1, Rest, OutRest).

possible_val(Row, Col, Val) <=> number(Val), Val =:= 0 | is_black(Row, Col).
possible_val(Row, Col, Val) <=> number(Val), Val > 0   | has_val(Row, Col, Val).

adjacent(Row1, Col1, Row2, Col2) :-
    RowDist is abs(Row2 - Row1),
    ColDist is abs(Col2 - Col1),
    TotalDist is RowDist + ColDist,
    TotalDist =:= 1.

not_adjacent(Row1, Col1, Row2, Col2) :-
    RowDist is abs(Row2 - Row1),
    ColDist is abs(Col2 - Col1),
    TotalDist is RowDist + ColDist,
    TotalDist > 1.

% no same values in row/col
has_val(Row1, _, Val), possible_val(Row1, _, Pval) \ Pval can_be Val <=> Pval = 0.
has_val(_, Col1, Val), possible_val(_, Col1, Pval) \ Pval can_be Val <=> Pval = 0.
has_val(Row1, Col1, Val), has_val(Row2, Col2, Val) ==> Row1 =\= Row2, Col1 =\= Col2.

% no black next to eachother
is_black(Row1, Col1), possible_val(Row2, Col2, Pval) \ Pval can_be N <=> adjacent(Row1, Col1, Row2, Col2) 
                                                                       | Pval = N.%, has_val(Row2, Col2, N). 

is_black(Row1, Col1), is_black(Row2, Col2) ==> not_adjacent(Row1, Col1, Row2, Col2).


is_black(Row1, Col1), is_black(Row2, Col2), is_black(Row1, Col3), possible_val(Row4, Col2, Pval) \ Pval can_be N <=>
                                                                     Vdiff is Row1 - Row2, 1 is abs(Vdiff),
                                                                     Col2 is Col1 + 1, Col3 is Col2 + 1, Row4 is Row1 - Vdiff
                                                                    | Pval = N.

is_black(Row1, Col1), is_black(Row2, Col2), is_black(Row3, Col1), possible_val(Row2, Col4, Pval) \ Pval can_be N <=>
                                                                     Hdiff is Col1 - Col2, 1 is abs(Hdiff),
                                                                     Row2 is Row1 + 1, Row3 is Row2 + 1, Col4 is Col1 - Hdiff
                                                                    | Pval = N.

% sandwich
possible_val(Row1, Col1, Val1), Val1 can_be N, 
possible_val(Row1, Col2, Val2), Val2 can_be N,
possible_val(Row1, Col3, Val3) \ Val3 can_be N1 <=> Col1 is Col3 - 1, Col2 is Col3 + 1 | Val3 = N1. 

possible_val(Row1, Col1, Val1), Val1 can_be N,
possible_val(Row2, Col1, Val2), Val2 can_be N,
possible_val(Row3, Col1, Val3) \ Val3 can_be N1 <=> Row1 is Row3 - 1, Row2 is Row3 + 1 | Val3 = N1. 

%is_black(Row1, Col1), possible_val(Row2, Col2, Pval) \ Pval can_be N <=> is_corner(Row1, Col1, Row2, Col2), ...
%                                                                         | Pval = N.
has_val(Row, Col, _) \ start            <=> marked(Row, Col).
has_val(Row, Col, _)                    ==> not_marked(Row, Col).
marked(Row, Col) \ not_marked(Row, Col) <=> true.

marked(Row1, Col1), has_val(Row2, Col2, _) \ not_marked(Row2, Col2) <=> adjacent(Row1, Col1, Row2, Col2) 
                                                                        | marked(Row2, Col2).

not_marked(_, _), end <=> fail.
end \ possible_val(_, _, _) <=> true.
end \ is_black(_, _) <=> true.
end \ has_val(_, _, _) <=> true.
end \ marked(_, _) <=> true.
end <=> true.

enum_cols([])                   <=> true.
enum_cols([V|Rest])             <=> number(V) | enum_cols(Rest).
enum_cols([V|Rest]), V can_be N <=> member(V, [N, 0]), enum_cols(Rest).

enum_rows([]).
enum_rows([Row|Rest]) :-
    enum_cols(Row),
    enum_rows(Rest).

solve(Size, P, Pout) :-
    size(Size),
    start,
    make_domains(1, P, Pout),
    enum_rows(Pout),
    end.

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

% solvse all sudokus
solutions :-
    findall(Id-Size-P, puzzle(Id, Size, P), L),
    solve_all(L, [], Times),
    sumlist(Times, S),
    writeln(Times),
    writeln(S).