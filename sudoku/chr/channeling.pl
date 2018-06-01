:- ensure_loaded('common.pl').
:- ensure_loaded('original_viewpoint.pl').
:- ensure_loaded('new_viewpoint.pl').
:- ensure_loaded('../sudex_toledo.pl').

% channeling between both viewpoints
channeling(Pa, Pb) :-
    generate_channel(Pa, Pb),
    generate_channel(Pb, Pa).

% generates the channeling for the given row
channel_helper(_, [], _).
channel_helper(ColN, [El|Rest], Rb) :-
    indexes(ColN, El, Rb),
    ColN1 is ColN + 1,
    channel_helper(ColN1, Rest, Rb).

% generates the channeling between the given viewpoints
generate_channel([], []).
generate_channel([Ra|RestA], [Rb|RestB]) :-
    channel_helper(1, Ra, Rb),
    generate_channel(RestA, RestB).

% solve the given sudoku using the channeling of both viewpoints
solve_chan(P) :-
    to_new_viewpoint(P, Pnew),
    makedomains(P),
    makedomains(Pnew),
    solve_sudoku(P),
    solve_sudoku_new(Pnew),
    channeling(P, Pnew),    
    enum(P),
    writeln(P),
    writeln(Pnew).

% solves the given sudokus and returns how long each sudoku took
solve_all_chan([], Times, Times).
solve_all_chan([P-Name|R], TimesIn, TimesOut) :-
    writeln(Name),
    Start is cputime,
    solve_chan(P),
    End is cputime,
    writeln(P),
    Time is End - Start,
    write('time elapsed: '),
    writeln(Time),
    write('\n'),
    solve_all_chan(R, [Time|TimesIn], TimesOut).

% solves all sudokus using the channeled viewpoints and prints total elapsed time
solutions_chan :-
    findall(P-Name, puzzles(P, Name), L),
    solve_all_chan(L, [], Times),
    sumlist(Times, S),
    writeln(Times),
    writeln(S).