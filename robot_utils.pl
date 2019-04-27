:- module(utils_robot, [
        path_to_closest_target_cell/4
    ]).
:- use_module(world, [
        walkeable_cell_list/2
    ])


path_to_closest_target_cell(Board, Pos, TargetCells, Path) :-
    walkeable_cell_list(Board, WalkeableCellList),
    bfs(Board, [[Pos]], TargetCells, WalkeableCellList, Path).

target_cell(Cell, TargetCells) :-
    member(Cell,TargetCell).

add_to_stack(A, B, [B|A]).

bfs(Board, [Visited|Rest], TargetCells, WalkeableCellList, Path) :-
    Visited = [Start|_],
    not(target_cell(Start, TargetCells)),
    list_difference(WalkeableList, Visited, WalkeableNotVisited),
    findall((X,Y), (member((X,Y), WalkeableNotVisited), robot_neighbor_cells(Start,(X,Y))), ToVisit),
    maplist(add_to_stack(Visited), ToVisit, VisitedExtended), 
    append(Rest, VisitedExtended, UpdatedQueue),
    bfs( Board, UpdatedQueue,TargetCells, WalkeableCellList, Path).

bfs(Board,[[Goal|Visited]|_], TargetCells, WalkeableCellList, Path) :- 
    target_cell(Goal, TargetCells),
    reverse([Goal|Visited], Path).


% Removes elements in Substract from List
% list_difference(+List, +Substract, -Rest)
list_difference([], _, []) :- !.
list_difference([H|T], Substract, Rest) :-
    member(H, Substract), !,
    list_difference(T,Substract, Rest).
list_difference([H|T], Substract, [H|Rest]) :-
    list_difference(T,Substract, Rest).


robot_neighbor_cells((X1,Y1), (X2, Y2)) :- X2 is X1 - 1, Y2 is Y1 - 1.
robot_neighbor_cells((X1,Y1), (X2, Y2)) :- X2 is X1 + 1, Y2 is Y1 + 1.
robot_neighbor_cells((X1,Y1), (X2, Y2)) :- X2 is X1 - 1, Y2 is Y1.
robot_neighbor_cells((X1,Y1), (X2, Y2)) :- X2 is X1, Y2 is Y1 - 1.
robot_neighbor_cells((X1,Y1), (X2, Y2)) :- X2 is X1, Y2 is Y1 + 1.
robot_neighbor_cells((X1,Y1), (X2, Y2)) :- X2 is X1 + 1, Y2 is Y1.
robot_neighbor_cells((X1,Y1), (X2, Y2)) :- X2 is X1 + 1, Y2 is Y1 - 1.
robot_neighbor_cells((X1,Y1), (X2, Y2)) :- X2 is X1 - 1, Y2 is Y1 + 1.
