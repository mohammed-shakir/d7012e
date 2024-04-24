% Tails with indexes
tails_with_indexes([], _, []).
tails_with_indexes([Head|Tail], Index, [[[Head|Tail], Index]|Result]) :-
    NewIndex is Index + 1,
    tails_with_indexes(Tail, NewIndex, Result).

% All contiguous subarrays of a list, sorted, with indexes
subarrays(List, SortedResult) :-
    tails_with_indexes(List, 0, IndexedTails),
    findall(
        (Sum, StartIndex, EndIndex),
        (member([Tail, StartIndex], IndexedTails),
        prefix(Sub, Tail, StartIndex, EndIndex),
        sum(Sub, Sum)),
        Result),
        insertion_sort(Result, SortedResult).

prefix(Prefix, List, StartIndex, EndIndex) :-
    append(Prefix, _, List),
    length(Prefix, Length),
    Prefix \= [],
    EndIndex is StartIndex + Length - 1.

% Sum of subarray
sum([], 0).
sum([Head|Tail], Sum) :-
    sum(Tail, TailSum),
    Sum is Head + TailSum.

% Insertion sort
insertion_sort([], []).
insertion_sort([(Sum, Start, End) | T], Sorted) :-
    insertion_sort(T, SortedTail),
    insert((Sum, Start, End), SortedTail, Sorted).

insert((Sum, Start, End), [], [(Sum, Start, End)]).
insert((Sum, Start, End), [(Sum1, Start1, End1) | T], [(Sum, Start, End), (Sum1, Start1, End1) | T]) :-
    Sum =< Sum1.
insert((Sum, Start, End), [(Sum1, Start1, End1) | T], [(Sum1, Start1, End1) | Sorted]) :-
    Sum > Sum1,
    insert((Sum, Start, End), T, Sorted).

% Extract a sublist based on indices
extract_sublist(List, Start, End, SubList) :-
    findall(Elem, (between(Start, End, Index), nth0(Index, List, Elem)), SubList).

% Print header
print_header :-
    format('~` tsize~4+~` ti~10+~` tj~10+~` tsublist~17+~n`', []).

% Print
print_each_tuple([], _).
print_each_tuple([(Sum, StartIndex, EndIndex)|T], List) :-
    extract_sublist(List, StartIndex, EndIndex, SubList),
    S is StartIndex + 1,
    E is EndIndex + 1,
    format('~` t~d~4+~` t~d~10+~` t~d~10+~` |  ~w~17+~n', [Sum, S, E, SubList]),
    print_each_tuple(T, List).

% Main
subarrays_and_print(List, K) :-
    subarrays(List, Result),
    take_first_k(Result, K, FilteredResult),
    print_header,
    print_each_tuple(FilteredResult, List).

% Take the first K elements of a list
take_first_k([], _, []).
take_first_k(_, 0, []).
take_first_k([H|T], K, [H|R]) :-
    K > 0,
    NewK is K - 1,
    take_first_k(T, NewK, R).

test(3, [-1, 2, -3, 4, -5]).
test(15, [-1, 2, -3, 4, -5, 6, -7, 8, -9, 10, -11, 12, -13,
    14, -15, 16, -17, 18, -19, 20, -21, 22, -23, 24, -25, 26,
    -27, 28, -29, 30, -31, 32, -33, 34, -35, 36, -37, 38, -39,
    40, -41, 42, -43, 44, -45, 46, -47, 48, -49, 50, -51, 52,
    -53, 54, -55, 56, -57, 58, -59, 60, -61, 62, -63, 64, -65,
    66, -67, 68, -69, 70, -71, 72, -73, 74, -75, 76, -77, 78,
    -79, 80, -81, 82, -83, 84, -85, 86, -87, 88, -89, 90, -91,
    92, -93, 94, -95, 96, -97, 98, -99, 100]).
test(6, [24, -11, -34, 42, -24, 7, -19, 21]).
test(8, [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3]).

testing :-
    forall(test(K, List), (subarrays_and_print(List, K), nl)).
