:- use_module(world, [playpen_cell/2, child_being_carried/2, child_cells/2]).

% SW, W, NW, N, NE, E, SE, S
directions([[-1, -1], [-1, 0], [-1, 1], [0, 1], [1, 1], [1, 0], [1, -1], [0, -1]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Robot Interface %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    robot_init(Board, Ammount, NewBoard) :-
        clean_cell_list(Board,CleanCells),
        select_k_items_randomly(Ammount, CleanCells, ChildListOut), 
        add_childs_to_map(Board,[], ChildListOut, NewBoard).

    make_robot_actions(Board, NewBoard) :-
        ChildList = Board.child_cells,
        writeln(ChildList),
        get_results(Board, ChildList, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_results(Board, Robot, UpdatedBoard) :-
    robot_behavior(Board, Robot, NewRobotPosition, OldChildPosition, NewChildPosition, OldDirtPosition),
    move_robot(Board, Robot, NewRobotPosition, NewBoard),
    move_child(NewBoard, NewChildPosition, OldDirtPosition, UpdatedBoard ).



% Selects k items randomly from a list L
select_k_items_randomly(0, _, []) :- !.
select_k_items_randomly(K, L, [X|Ans]) :-
    random_member(X, L), !,
    delete(L, X, New_L),
    New_K is K-1,
    select_k_items_randomly(New_K, New_L, Ans).

robot_behavior(Board,  (R, C)) :-
    playpen_cell(Board,  (R, C)),
    child_being_carried(Board,  (R, C)),
    robot_drop_child(Board,  (R, C)).
robot_behavior(Board,  (R, C)) :-
    length(Board.child_cells, C1),
    C is 0,
    robot_clean(Board, (R, C)).
robot_behavior(Board,  (R, C)) :-
    child_being_carried(Board,  (R, C)),!,
    robot_go_to_playpen(Board,  (R, C)).
robot_behavior(Board,  (R, C)) :-
    robot_go_pickup_child(Board,  (R, C)).




