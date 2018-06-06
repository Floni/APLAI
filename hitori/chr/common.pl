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
