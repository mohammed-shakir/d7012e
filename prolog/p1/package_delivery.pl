% state(Room, RobotCarrying, Room1Items, Room2Items, Room3Items)
state(r1, [], [steel_key], [brass_key], [package]).

% Moveing between rooms
% Room 1 to Room 2
move(state(R1, Carrying, R1Items, R2Items, R3Items),
    walk(R1, R2),
    state(R2, Carrying, R1Items, R2Items, R3Items)) :-
        member(steel_key, Carrying),
        R1 = r1,
        R2 = r2.

% Room 2 to Room 1
move(state(R2, Carrying, R1Items, R2Items, R3Items),
    walk(R2, R1),
    state(R1, Carrying, R1Items, R2Items, R3Items)) :-
        member(steel_key, Carrying),
        R1 = r1,
        R2 = r2.

% Room 1 to 3
move(state(R1, Carrying, R1Items, R2Items, R3Items),
    walk(R1, R3),
    state(R3, Carrying, R1Items, R2Items, R3Items)) :-
        member(brass_key, Carrying),
        R1 = r1,
        R3 = r3.

% Room 3 to 1
move(state(R3, Carrying, R1Items, R2Items, R3Items),
    walk(R3, R1),
    state(R1, Carrying, R1Items, R2Items, R3Items)) :-
        member(brass_key, Carrying),
        R1 = r1,
        R3 = r3.

% Pick up an item
% Pick up an item from room 1
move(state(R1, Carrying, R1Items, R2Items, R3Items),
    pickup(Item),
    state(R1, NewCarrying, NewR1Items, R2Items, R3Items)) :-
        member(Item, R1Items),
        R1 = r1,
        length(Carrying, Length),
        Length < 2,
        append(Carrying, [Item], NewCarrying),
        delete(R1Items, Item, NewR1Items).

% Pick up an item from room 2
move(state(R2, Carrying, R1Items, R2Items, R3Items),
    pickup(Item),
    state(R2, NewCarrying, R1Items, NewR2Items, R3Items)) :-
        member(Item, R2Items),
        R2 = r2,
        length(Carrying, Length),
        Length < 2,
        append(Carrying, [Item], NewCarrying),
        delete(R2Items, Item, NewR2Items).

% Pick up an item from room 3
move(state(R3, Carrying, R1Items, R2Items, R3Items),
    pickup(Item),
    state(R3, NewCarrying, R1Items, R2Items, NewR3Items)) :-
        member(Item, R3Items),
        R3 = r3,
        length(Carrying, Length),
        Length < 2,
        append(Carrying, [Item], NewCarrying),
        delete(R3Items, Item, NewR3Items).

% Drop an item
% Drop an item in room 1
move(state(R1, Carrying, R1Items, R2Items, R3Items),
    drop(Item),
    state(R1, NewCarrying, NewR1Items, R2Items, R3Items)) :-
        member(Item, Carrying),
        R1 = r1,
        delete(Carrying, Item, NewCarrying),
        append(R1Items, [Item], NewR1Items).

% Drop an item in room 2
move(state(R2, Carrying, R1Items, R2Items, R3Items),
    drop(Item),
    state(R2, NewCarrying, R1Items, NewR2Items, R3Items)) :-
        member(Item, Carrying),
        R2 = r2,
        delete(Carrying, Item, NewCarrying),
        append(R2Items, [Item], NewR2Items).

% Drop an item in room 3
move(state(R3, Carrying, R1Items, R2Items, R3Items),
    drop(Item),
    state(R3, NewCarrying, R1Items, R2Items, NewR3Items)) :-
        member(Item, Carrying),
        R3 = r3,
        delete(Carrying, Item, NewCarrying),
        append(R3Items, [Item], NewR3Items).

% Goal state
goal(state(r2, _, _, R2Items, _)) :-
    member(package, R2Items).

% solveR: Depth-first search
% Initial state, N: maximum number of moves, Trace: list of moves
solveR(State, N, Trace) :-
    solveR_helper(State, N, [State], Trace).

solveR_helper(State, _, _, []) :-
    goal(State),
    !. % Prevent backtracking once the goal state is found

% State: current state, N: maximum number of moves, Visited: list of visited states, Trace: list of moves
solveR_helper(State, N, Visited, [Move | Trace]) :-
    N > 0,
    move(State, Move, NewState), % Attempts a move from the current State resulting in NewState.
    \+ member(NewState, Visited), % \+ is the negation operator. Checks so that NewState is not in the list of visited states to prevent cycles.
    N1 is N - 1,
    solveR_helper(NewState, N1, [NewState | Visited], Trace).

% solveR(state(r1, [], [steel_key], [brass_key], [package]), 10, Trace).

% set_prolog_flag(answer_write_options, [max_depth(0)])