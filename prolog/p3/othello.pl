/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Mohammed Shakir
%    Student user id  : mohsha-0
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([	[.,.,.,.,.,.], 
        	[.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

% A board where player 1 has fewer stones than player 2
testBoard1([
    [2, 2, 2, 2, 2, 2],
    [2, 1, 1, 1, 1, 2],
    [2, 1, ., ., 1, 2],
    [2, 1, ., ., 1, 2],
    [2, 1, 1, 1, 1, 2],
    [2, 2, 2, 2, 2, 2]
]).

% A board where player 2 has fewer stones than player 1
testBoard2([
    [1, 1, 1, 1, 1, 1],
    [1, 2, 2, 2, 2, 1],
    [1, 2, ., ., 2, 1],
    [1, 2, ., ., 2, 1],
    [1, 2, 2, 2, 2, 1],
    [1, 1, 1, 1, 1, 1]
]).

% Tie / Terminal
testBoard3([
    [1, 2, 1, 2, 1, 2],
    [2, 1, 2, 1, 2, 1],
    [1, 2, 1, 2, 1, 2],
    [2, 1, 2, 1, 2, 1],
    [1, 2, 1, 2, 1, 2],
    [2, 1, 2, 1, 2, 1]
]).

% Terminal state but player 1 wins
testBoard4([
    [2, 2, 1, 2, 1, 2],
    [2, 1, 2, 1, 2, 1],
    [1, 2, 1, 2, 1, 2],
    [2, 1, 2, 1, 2, 1],
    [1, 2, 1, 2, 1, 2],
    [2, 1, 2, 1, 2, 1]
]).

% Terminal state but player 2 wins
testBoard5([
    [1, 1, 2, 1, 2, 1],
    [1, 2, 1, 2, 1, 2],
    [2, 1, 2, 1, 2, 1],
    [1, 2, 1, 2, 1, 2],
    [2, 1, 2, 1, 2, 1],
    [1, 2, 1, 2, 1, 2]
]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState,InitialPlyr) :- initBoard(InitialState), InitialPlyr = 1.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

% Helper to count stones
count_stones([], _, 0).
count_stones([Row|Rest], Player, Count) :-
    count_stones(Rest, Player, RestCount),
    count_row(Row, Player, RowCount),
    Count is RestCount + RowCount.

count_row([], _, 0).
% If we find the player, we increment the count.
count_row([P|T], P, Count) :-
    !, % Once we find the player, we can stop.
    count_row(T, P, NewCount),
    Count is NewCount + 1.
% If we don't find the player, we continue.
count_row([_|T], P, Count) :- 
    count_row(T, P, Count).

winner(State, Plyr) :-
    terminal(State),
    count_stones(State, 1, Count1),
    count_stones(State, 2, Count2),
    (
        (Count1 < Count2 -> Plyr = 1);
        (Count2 < Count1 -> Plyr = 2)
    ).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :-
    terminal(State),
    count_stones(State, 1, Count1),
    count_stones(State, 2, Count2),
    Count1 == Count2.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :-
    moves(1, State, Moves1),
    moves(2, State, Moves2),
    Moves1 == [],
    Moves2 == [].

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(Plyr, State, MvList) :-
    findall(
		[X,Y],
		(between(0, 5, X), between(0, 5, Y), validmove(Plyr, State, [X,Y])),
		MvList
	).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(Plyr, [X,Y], State, NewState, NextPlyr) :-
    set(State, StateAfterMove, [X,Y], Plyr), % Set the new stone
    flip_all_directions(Plyr, [X,Y], StateAfterMove, NewState), % Flip opponent stones
    other_player(Plyr, NextPlyr). % Switch player

other_player(1, 2).
other_player(2, 1).

directions([
    [0, 1],     % Right
    [1, 1],     % Down-right
    [1, 0],     % Down
    [1, -1],    % Down-left
    [0, -1],    % Left
    [-1, -1],   % Up-left
    [-1, 0],    % Up
    [-1, 1]     % Up-right
]).

in_bounds(X, Y) :-
    X >= 0, X < 6, Y >= 0, Y < 6.

% Flip all directions
flip_all_directions(Plyr, Pos, State, NewState) :-
    directions(Dirs),
    flip_directions(Plyr, Pos, Dirs, State, NewState).

% Recursive flipping in all directions
flip_directions(_, _, [], State, State).
flip_directions(Plyr, Pos, [Dir|Dirs], State, NewState) :-
    flip_in_direction(Plyr, Pos, Dir, State, StateAfterFlip),
    flip_directions(Plyr, Pos, Dirs, StateAfterFlip, NewState).

% Flip in a specific direction
flip_in_direction(Plyr, [X, Y], [DX, DY], State, NewState) :-
    step([X, Y], [DX, DY], NextPos),
    try_flip(Plyr, NextPos, [DX, DY], State, NewState).

% Calculate next position
step([X, Y], [DX, DY], [NX, NY]) :-
    NX is X + DX,
    NY is Y + DY.

% Attempt to flip stones starting from a position in a given direction
try_flip(Plyr, [X, Y], [DX, DY], State, NewState) :-
    in_bounds(X, Y),
    get(State, [X, Y], Opponent),
    other_player(Plyr, Opponent),  % Ensuring the next stone is opponent's
    continue_flipping(Plyr, [X, Y], [DX, DY], State, NewState).

% Continue flipping until a player's stone is found
continue_flipping(Plyr, [X, Y], [DX, DY], State, NewState) :-
    step([X, Y], [DX, DY], NextPos),
    get(State, NextPos, Val),
    Val == Plyr,  % Stop if we reach a player's stone
    set(State, NewState, [X, Y], Plyr).
continue_flipping(Plyr, [X, Y], [DX, DY], State, NewState) :-
    step([X, Y], [DX, DY], NextPos),
    get(State, NextPos, Opponent),
    other_player(Plyr, Opponent),
    continue_flipping(Plyr, NextPos, [DX, DY], State, TempState),
    set(TempState, NewState, [X, Y], Plyr).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State, [X,Y]) :-
    get(State, [X,Y], '.'), % Ensure the cell is empty.
    member(Dx, [-1,0,1]), % Direction deltas for X
    member(Dy, [-1,0,1]), % Direction deltas for Y
    (Dx \= 0; Dy \= 0), % Avoid checking the zero direction (no movement)
    can_flip(Plyr, State, [X,Y], [Dx,Dy], false).

% Helper to find at least one flanking line.
can_flip(Plyr, State, [X,Y], [Dx,Dy], Found) :-
    NewX is X + Dx, % New X position
    NewY is Y + Dy, % New Y position
	NewX >= 0, NewX < 6, % Check bounds
	NewY >= 0, NewY < 6, % Check bounds
    get(State, [NewX,NewY], Cell),
    players(Plyr, Opponent),
    (Found -> Cell = Plyr ; Cell = Opponent),
    (Cell = Plyr ; can_flip(Plyr, State, [NewX,NewY], [Dx,Dy], true)).

% Define the other player.
players(1, 2).
players(2, 1).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

% Heuristic function h(State, Val)
h(State, Val) :-
    terminal(State), !,
    (
        winner(State, 1) -> Val = 100  ; % Player 1 wins
        winner(State, 2) -> Val = -100 ; % Player 2 wins
        tie(State) -> Val = 0          ; % Tie
        true -> Val = 0                  % Default case if no terminal state checks triggered
    ).

h(State, Val) :-
    count_stones(State, 1, Count1),
    count_stones(State, 2, Count2),
    Val is Count2 - Count1.  % Favor states with fewer stones for Player 1

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-100).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(100).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 