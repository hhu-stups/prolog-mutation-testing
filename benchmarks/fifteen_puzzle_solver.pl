:- module(eight_puzzle_solver,[solve_astar/1,solve_bfs/1,solve_iterative/2]).

:- use_module('mutation_testing').

:- use_module(library(plunit)).
:- use_module(library(lists)).


% Solver for the 15 puzzle using breadth-first-search, iterative deepening and A*.

solved(c([x, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15])).

%%% A-Star
solve_astar(Puzzle) :- 
    all_s(Puzzle,NextPuzzles) ,
    cost_heuristic(1,NextPuzzles,NextPuzzlesWithCosts) ,
    solve_astar([(Puzzle,_,_)],NextPuzzlesWithCosts).

solve_astar(_,Puzzles) :-
    contains_solved_puzzle(Puzzles) , !.
solve_astar(ClosedList,Puzzles) :-
    get_smallest_costs(ClosedList,Puzzles,(Puzzle,_,SCosts),Rest) ,
    all_s(Puzzle,NextPuzzles) ,
    NextCosts is SCosts + 1 ,
    cost_heuristic(NextCosts,NextPuzzles,NextPuzzlesWithCosts) ,
    append(NextPuzzlesWithCosts,Rest,NewPuzzles) ,                 
    solve_astar([(Puzzle,_,SCosts)|ClosedList],NewPuzzles).

contains_solved_puzzle([(Puzzle,_,_)|_]) :-
    Puzzle = c([x, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]) , !.
contains_solved_puzzle([_|T]) :-
    contains_solved_puzzle(T).

get_smallest_costs(ClosedList,[Puzzle|T],SmallestCosts,Rest) :- 
    \+ member(Puzzle,ClosedList) , ! ,                          
    get_smallest_costs(ClosedList,T,Puzzle,[],SmallestCosts,Rest).
get_smallest_costs(ClosedList,[Puzzle,Puzzle2|T],SmallestCosts,[Puzzle|Rest]) :-
    get_smallest_costs(ClosedList,T,Puzzle2,[],SmallestCosts,Rest).

get_smallest_costs(_,[],SmallestCosts,Rest,SmallestCosts,Rest).
get_smallest_costs(ClosedList,[(Puzzle,HC,AC)|T],(AccPuzzle,AccHC,AccAC),RAcc,SmallestCosts,Rest) :-
    HC + AC < AccHC + AccAC ,
    \+ member((Puzzle,HC,AC),ClosedList) , ! ,
    get_smallest_costs(ClosedList,T,(Puzzle,HC,AC),[(AccPuzzle,AccHC,AccAC)|RAcc],SmallestCosts,Rest).
get_smallest_costs(ClosedList,[GreaterCosts|T],SAcc,RAcc,SmallestCosts,Rest) :-
    get_smallest_costs(ClosedList,T,SAcc,[GreaterCosts|RAcc],SmallestCosts,Rest).

cost_heuristic(ActualCosts,NewPuzzles,NewPuzzlesWithCosts) :-
    solved(Solved) ,
    cost_heuristic(Solved,ActualCosts,NewPuzzles,NewPuzzlesWithCosts).

cost_heuristic(_,_,[],[]).
cost_heuristic(Solved,ActualCosts,[Puzzle|T],[(Puzzle,ManhattanDistance,ActualCosts)|NT]) :-
    puzzles_manhattan(Puzzle,Solved,ManhattanDistance) ,
    cost_heuristic(Solved,ActualCosts,T,NT).

puzzles_manhattan(Puzzle1,Puzzle2,Distance) :-
    puzzles_manhattan(1,Puzzle1,Puzzle2,0,Distance).

puzzles_manhattan(Symbol,Puzzle1,Puzzle2,Acc,Distance) :-
    Symbol =< 15 ,
    get_coords(Symbol,Puzzle1,Coords1) ,
    get_coords(Symbol,Puzzle2,Coords2) ,
    manhattan_coord(Coords1,Coords2,CoordDistance) ,
    NewAcc is Acc + CoordDistance ,
    Symbol1 is Symbol + 1 ,
    puzzles_manhattan(Symbol1,Puzzle1,Puzzle2,NewAcc,Distance).
puzzles_manhattan(Symbol,Puzzle1,Puzzle2,Acc,Distance) :-
    Symbol > 15 ,
    get_coords(x,Puzzle1,Coords1) ,
    get_coords(x,Puzzle2,Coords2) ,
    manhattan_coord(Coords1,Coords2,CoordDistance) ,
    Distance is Acc + CoordDistance.

manhattan_coord((X1,Y1),(X2,Y2),Distance) :-
    Distance is abs(X2 - X1) + abs(Y2 - Y1).
%%%

solve_iterative(Puzzle,Pfad) :-
    lst(Pfad) ,
    dfs(Puzzle,Pfad).

lst([]).
lst([_|T]) :-
    lst(T).

dfs(Start,Pfad) :-
    dfs1(Start,Pfad,[]).

dfs1(A,[A],_) :-
    solved(A).
dfs1(A,[A|Rest],Seen) :-
    \+ member(A,Seen) ,
    s(A,B) ,
    dfs1(B,Rest,[A|Seen]).
%%%

%%% Breadth-First-Search
solve_bfs(Puzzle) :-
    solve_bfs([],[Puzzle]).
solve_bfs(Previous,Puzzles) :-
    maplist(all_s,Puzzles,TempNextPuzzles) ,
    flatten(TempNextPuzzles,TempNextPuzzles2) ,
    findall(NextPuzzle,(member(NextPuzzle,TempNextPuzzles2) , \+member(NextPuzzle,Previous)),NextPuzzles) ,
    (contains_solved_puzzle(NextPuzzles)
    ->  ! % solved
    ;   solve_bfs(Puzzles,NextPuzzles)).
%%%

all_s(Puzzle,NewPuzzles) :-
    get_coords(x,Puzzle,Coords) ,
    findall(NextCoord,get_next_coords(Coords,NextCoord),NextCoords) ,
    maplist(move(Puzzle,Coords),NextCoords,NewPuzzles).

s(Puzzle,NewPuzzle) :-
    get_coords(x,Puzzle,Coords) ,
    get_next_coords(Coords,NextCoords) ,
    move(Puzzle,Coords,NextCoords,NewPuzzle).

move(Puzzle,Coords,NextCoords,NewPuzzle) :-
    get_coords(TempSymbol,Puzzle,NextCoords) ,
    set_field(Puzzle,Coords,TempSymbol,TempNewPuzzle) ,
    set_field(TempNewPuzzle,NextCoords,x,NewPuzzle).

replace_at([],Elm,_,[Elm]).
replace_at([_|T],Elm,0,[Elm|T]).
replace_at([H|T],Elm,Index,[H|NT]) :-
    Index > 0 ,
    NewIndex is Index - 1 ,
    replace_at(T,Elm,NewIndex,NT).

set_field(c(R1,R2,R3,R4),(X,0),Symbol,c(NewR1,R2,R3,R4)) :-
    replace_at(R1,Symbol,X,NewR1).
set_field(c(R1,R2,R3,R4),(X,1),Symbol,c(R1,NewR2,R3,R4)) :-
    replace_at(R2,Symbol,X,NewR2).
set_field(c(R1,R2,R3,R4),(X,2),Symbol,c(R1,R2,NewR3,R4)) :-
    replace_at(R3,Symbol,X,NewR3).
set_field(c(R1,R2,R3,R4),(X,3),Symbol,c(R1,R2,R3,NewR4)) :-
    replace_at(R4,Symbol,X,NewR4).

get_coords(Symbol,c(R1,_,_,_),(X,0)) :-
    nth0(X,R1,Symbol).
get_coords(Symbol,c(_,R2,_,_),(X,1)) :-
    nth0(X,R2,Symbol).
get_coords(Symbol,c(_,_,R3,_),(X,2)) :-
    nth0(X,R3,Symbol).
get_coords(Symbol,c(_,_,_,R4),(X,3)) :-
    nth0(X,R4,Symbol).

get_next_coords((CurrX,CurrY),(NextX,CurrY)) :-
    get_next_coords_aux(CurrX,NextX).
get_next_coords((CurrX,CurrY),(CurrX,NextY)) :-
    get_next_coords_aux(CurrY,NextY).

get_next_coords_aux(Pos,NextPos) :-
    Pos > 0 ,
    NextPos is Pos - 1.
get_next_coords_aux(Pos,NextPos) :-
    Pos < 3 ,
    NextPos is Pos + 1.

:- begin_tests(move).

test(move) :-
    move(c([x, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]),(0,0),(1,0),NewPuzzle) , ! ,
    NewPuzzle = c([1, x, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]).

test(move1) :-
    move(c([1, 2, x, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]),(2,0),(1,0),NewState) , ! ,
    NewState = c([1, x, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]).

test(move2) :-
    move(c([1, 2, 3, 7], [4, 5, 6, x], [8, 9, 10, 11], [12, 13, 14, 15]),(3,1),(3,2),NewState) , ! ,
    NewState = c([1, 2, 3, 7], [4, 5, 6, 11], [8, 9, 10, x], [12, 13, 14, 15]).

test(move3) :-
    move(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 9, 10, 11], [12, 13, 14, 15]),(0,2),(1,2),NewState) , ! ,
    NewState = c([4, 1, 2, 3], [8, 5, 6, 7], [9, x, 10, 11], [12, 13, 14, 15]).

test(move4) :-
    move(c([4, 1, 2, 3], [8, 5, 6, 7], [9, x, 10, 11], [12, 13, 14, 15]),(1,2),(2,2),NewState) , ! ,
    NewState = c([4, 1, 2, 3], [8, 5, 6, 7], [9, 10, x, 11], [12, 13, 14, 15]).

test(move5) :-
    move(c([4, 1, 2, 3], [8, 5, 6, 7], [9, 10, x, 11], [12, 13, 14, 15]),(2,2),(1,2),NewState) , ! ,
    NewState = c([4, 1, 2, 3], [8, 5, 6, 7], [9, x, 10, 11], [12, 13, 14, 15]).

:- end_tests(move).


:- begin_tests(solve_astar).

test(simple) :-
    format("~nA*:~n~n",[]) , 
    solve_astar(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 9, 10, 11], [12, 13, 14, 15])) , !.

test(simple2,[nondet]) :-
    solve_astar(c([4, 1, 2, 3], [x, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15])).

test(simple3,[nondet]) :-
    solve_astar(c([4, 1, 2, 3], [8, 5, 6, 7], [9, x, 10, 11], [12, 13, 14, 15])).

test(simple4,[nondet]) :-
    solve_astar(c([4, 1, 2, 3], [8, 5, 6, 7], [9, 10, 14, 11], [12, 13, x, 15])).

test(simple5,[nondet]) :-
    solve_astar(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 10, 14, 11], [9, 12, 13, 15])).

test(simple6,[nondet]) :-
    solve_astar(c([4, 1, 2, 3], [8, 5, 6, 7], [10, 14, 13, 11], [9, x, 12, 15])).

/*test(complex1,[nondet]) :-
    solve_astar(c([4, 1, 2, 3], [8, 5, 6, 7], [14, x, 13, 11], [10, 9, 12, 15])).

test(complex2,[nondet]) :-
    solve_astar(c([4, 1, 2, 3], [8, x, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15])).

test(complex3,[nondet]) :-
    solve_astar(c([4, 2, x, 3], [8, 1, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15])).

test(complex4,[nondet]) :-
    solve_astar(c([13, 2, 3, 12], [9, 11, 1, 10], [x, 6, 4, 14], [15, 8, 7, 5])))./

:- end_tests(solve_astar).

:- begin_tests(solve_iterative_deepening).

test(simple) :-
    format("~nIterative Deepening:~n~n",[]) ,
    solve_iterative(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 9, 10, 11], [12, 13, 14, 15]),_Pfad).

test(simple2,[nondet]) :-
    solve_iterative(c([4, 1, 2, 3], [x, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]),_Pfad).

test(simple3,[nondet]) :-
    solve_iterative(c([4, 1, 2, 3], [8, 5, 6, 7], [9, x, 10, 11], [12, 13, 14, 15]),_Pfad).

test(simple4,[nondet]) :-
    solve_iterative(c([4, 1, 2, 3], [8, 5, 6, 7], [9, 10, 14, 11], [12, 13, x, 15]),_Pfad).

test(simple5,[nondet]) :-
    solve_iterative(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 10, 14, 11], [9, 12, 13, 15]),_Pfad).

test(simple6,[nondet]) :-
    solve_iterative(c([4, 1, 2, 3], [8, 5, 6, 7], [10, 14, 13, 11], [9, x, 12, 15]),_Pfad).

/*test(complex1,[nondet]) :-
    solve_iterative(c([4, 1, 2, 3], [8, 5, 6, 7], [14, x, 13, 11], [10, 9, 12, 15]),_Pfad).

test(complex2,[nondet]) :-
    solve_iterative(c([4, 1, 2, 3], [8, x, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15]),_Pfad).

test(complex3,[nondet]) :-
    solve_iterative(c([4, 2, x, 3], [8, 1, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15]),_Pfad).

test(complex4,[nondet]) :-
    solve_iterative(c([13, 2, 3, 12], [9, 11, 1, 10], [x, 6, 4, 14], [15, 8, 7, 5]),_Pfad))./

:- end_tests(solve_iterative_deepening).

/* breadth-first-search is very slow
:- begin_tests(solve_bfs).

test(simple) :-
    format("~nBreadth-First-Search:~n~n",[]) ,
    solve_bfs(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 9, 10, 11], [12, 13, 14, 15])).

test(simple2,[nondet]) :-
    solve_bfs(c([4, 1, 2, 3], [x, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15])).

test(simple3,[nondet]) :-
    solve_bfs(c([4, 1, 2, 3], [8, 5, 6, 7], [9, x, 10, 11], [12, 13, 14, 15])).

test(simple4,[nondet]) :-
    solve_bfs(c([4, 1, 2, 3], [8, 5, 6, 7], [9, 10, 14, 11], [12, 13, x, 15])).

test(simple5,[nondet]) :-
    solve_bfs(c([4, 1, 2, 3], [8, 5, 6, 7], [x, 10, 14, 11], [9, 12, 13, 15])).

test(simple6,[nondet]) :-
    solve_bfs(c([4, 1, 2, 3], [8, 5, 6, 7], [10, 14, 13, 11], [9, x, 12, 15])).

test(complex1,[nondet]) :-
    solve_bfs(c([4, 1, 2, 3], [8, 5, 6, 7], [14, x, 13, 11], [10, 9, 12, 15])).

test(complex2,[nondet]) :-
    solve_bfs(c([4, 1, 2, 3], [8, x, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15])).

test(complex3,[nondet]) :-
    solve_bfs(c([4, 2, x, 3], [8, 1, 5, 7], [14, 13, 6, 11], [10, 9, 12, 15])).

test(complex4,[nondet]) :-
    solve_bfs(c([13, 2, 3, 12], [9, 11, 1, 10], [x, 6, 4, 14], [15, 8, 7, 5])).

:- end_tests(solve_bfs).*/
