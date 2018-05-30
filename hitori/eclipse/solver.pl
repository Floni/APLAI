:- lib(ic).

:- ensure_loaded('../puzzles.pl').

build_domains(Size, P, Pmat) :-
    dim(Pmat, [Size, Size]),
    ( for(I, 1, Size),
      foreach(Prow, P),
      param(Size, Pmat)
    do
        ( for(J, 1, Size),
          foreach(Pelem, Prow),
          param(Size, I, Pmat)
        do
            Elem is Pmat[I, J],
            BlackN is -((I-1) * Size + J),
            Elem :: [Pelem, BlackN]
        )
    ).

row_constraint(Size, Pmat) :-
    ( for(I, 1, Size),
      param(Pmat)
    do
        Row is Pmat[I],
        alldifferent(Row)
    ).

col_constraint(Size, Pmat) :-
    ( for(I, 1, Size),
      param(Pmat, Size)
    do
        Col is Pmat[1..Size, I],
        alldifferent(Col)
    ).

adj_constraint(Size, Pmat) :-
    ( for(I, 1, Size),
      param(Size, Pmat)
    do
        ( for(J, 1, Size),
          param(I, Size, Pmat)
        do
            Elem is Pmat[I, J],
            (I < Size ->
                Belem is Pmat[I+1, J],
                #<(Belem, 0, B1),
                #<(Elem, 0, B2),
                B1 + B2 #\= 2
            ; true),
            (J < Size ->
                Relem is Pmat[I, J+1],
                #<(Relem, 0, B3),
                #<(Elem, 0, B4),
                B3 + B4 #\= 2 
            ; true)
        )
    ).

close_constraint(Size, Pmat) :-
    ( for(I, 1, Size),
      param(Size, Pmat)
    do
        ( for(J, 1, Size),
          param(I, Size, Pmat)
        do
            (I > 1 ->
                Elem1 is Pmat[I-1, J],
                #<(Elem1, 0, B1)
            ;
                B1 = 1
            ),

            (I < Size ->
                Elem2 is Pmat[I+1, J],
                #<(Elem2, 0, B2)
            ;
                B2 = 1
            ),

            (J > 1 ->
                Elem3 is Pmat[I, J-1],
                #<(Elem3, 0, B3)
            ;
                B3 = 1
            ),
            
            (J < Size ->
                Elem4 is Pmat[I, J+1],
                #<(Elem4, 0, B4)
            ;
                B4 = 1
            ),

            B1 + B2 + B3 + B4 #\= 4
        )
    ).

sandwich_double_constraint(Size, Pmat) :-
    ( for(I, 1, Size),
      param(Size, Pmat)
    do
        ( for(J, 1, Size),
          param(I, Size, Pmat)
        do
            Elem is Pmat[I, J],
            ((I < Size, I > 1) ->
                Belem1 is Pmat[I+1, J],
                Belem2 is Pmat[I-1, J],
                #=(Belem1, Belem2, B1),
                #>(Elem, 0, B2),
                B1 => B2
            ; true),
            ((J < Size, J > 1) ->
                Relem1 is Pmat[I, J+1],
                Relem2 is Pmat[I, J-1],
                #=(Relem1, Relem2, B3),
                #>(Elem, 0, B4),
                B3 => B4
            ; true)
        )
    ).

sandwich_triple_constraint(Size, Pmat) :-
    ( for(I, 1, Size),
      param(Size, Pmat)
    do
        ( for(J, 1, Size),
          param(I, Size, Pmat)
        do
            Elem is Pmat[I, J],
            ((I < Size, I > 1) ->
                Belem1 is Pmat[I+1, J],
                Belem2 is Pmat[I-1, J],
                #=(Belem1, Belem2, B1),
                #=(Belem1, Elem, B2),
                #<(Belem1, 0, B3),
                #<(Belem2, 0, B4),
                B1 and B2 => B3 and B4
            ; true),
            ((J < Size, J > 1) ->
                Relem1 is Pmat[I, J+1],
                Relem2 is Pmat[I, J-1],
                #=(Relem1, Relem2, B5),
                #=(Relem1, Elem, B6),
                #<(Relem1, 0, B7),
                #<(Relem2, 0, B8),
                B5 and B6 => B7 and B8
            ; true)
        )
    ).

set_black_constraint(Size, Pmat) :-
    ( for(I, 1, Size),
      param(Size, Pmat)
    do
        ( for(J, 1, Size),
          param(I, Size, Pmat)
        do
            Elem is Pmat[I, J],
            #<(Elem, 0, B),
            (I > 1 ->
                Elem1 is Pmat[I-1, J],
                #>(Elem1, 0, B1),
                B => B1
            ; true ),

            (I < Size ->
                Elem2 is Pmat[I+1, J],
                #>(Elem2, 0, B2),
                B => B2
            ; true ),

            (J > 1 ->
                Elem3 is Pmat[I, J-1],
                #>(Elem3, 0, B3),
                B => B3
            ; true ),
            
            (J < Size ->
                Elem4 is Pmat[I, J+1],
                #>(Elem4, 0, B4),
                B => B4
            ; true )
        )
    ).

white_squares(Size, Pmat, Sum) :-
    ( for(I, 1, Size),
      fromto(0, Bin, Bout, Btot),
      param(Size, Pmat)
    do
        ( for(J, 1, Size),
          fromto(Bin, Bin2, Bout2, Bout),
          param(I, Pmat)
        do
            Elem is Pmat[I, J],
            #>(Elem, 0, B),
            Bout2 = Bin2 + B
        )
    ),
    Sum #= eval(Btot).

find_white_square(_Size, Pmat, X, Y, X, Y) :-
    Elem is Pmat[X, Y],
    Elem > 0, !.
find_white_square(Size, Pmat, X, Y, Xo, Yo) :-
    X < Size, !,
    X1 is X + 1,
    find_white_square(Size, Pmat, X1, Y, Xo, Yo).
find_white_square(Size, Pmat, _X, Y, Xo, Yo) :-
    Y < Size,
    Y1 is Y + 1,
    find_white_square(Size, Pmat, 1, Y1, Xo, Yo).


check_neighbour(Size, Pmat, X, Y, Visited) :-
    X > 0, Y > 0, X =< Size, Y =< Size, % bounds
    Elem is Pmat[X, Y], Elem > 0,       % not blacked
    \+ member(X-Y, Visited).            % not visited

all_neighbours(X, Y, L) :-
    X1 is X - 1, X2 is X + 1,
    Y1 is Y - 1, Y2 is Y + 1,
    L = [X1-Y, X2-Y, X-Y1, X-Y2].

count_connected(_Size, _Pmat, [], Visited, Visited).
count_connected(Size, Pmat, [X-Y|ToVisited], VisitedIn, VisitedOut) :-
    all_neighbours(X, Y, AllNeighs),
    findall(A-B, (member(A-B, AllNeighs), check_neighbour(Size, Pmat, A, B, VisitedIn)), Neighs),
    append(ToVisited, Neighs, ToVisit),
    append(Neighs, VisitedIn, VisitedIn1),
    count_connected(Size, Pmat, ToVisit, VisitedIn1, VisitedOut).

print_hitori(Size, Pmat) :-
    ( for(I, 1, Size),
      param(Size, Pmat)
    do
        ( for(J, 1, Size),
          param(Pmat, I)
        do
            Elem is Pmat[I, J],
            (Elem > 0 ->
                write(Elem),
                write(' ')
            ;
                write('X ') 
            )
        ),
        nl
    ).

solve(Size, P, Pmat) :-
    build_domains(Size, P, Pmat),
    row_constraint(Size, Pmat),
    col_constraint(Size, Pmat),
    adj_constraint(Size, Pmat),

    close_constraint(Size, Pmat),
    sandwich_double_constraint(Size, Pmat),
    sandwich_triple_constraint(Size, Pmat),
    set_black_constraint(Size, Pmat),
    
    white_squares(Size, Pmat, Tot),

    array_flat(2, Pmat, Pflat),
    labeling(Pflat),

    find_white_square(Size, Pmat, 1, 1, X, Y),
    count_connected(Size, Pmat, [X-Y], [X-Y], Connected),
    length(Connected, Tot).

% solves all hitori while printing their name and runtime
solutions :-
    findall(Id-Size-P, puzzle(Id, Size, P), L),
    ( foreach(Id-Size-P, L),
      foreach(Time, Times)
    do
        writeln(Id),
        cputime(Start),
        solve(Size, P, Pmat),
        cputime(End),
        Time is End - Start,
        print_hitori(Size, Pmat),
        write('time elapsed: '),
        writeln(Time),
        write('\n')
    ),
    sum(Times, S),
    writeln(Times),
    writeln(S).