
:- module(child, [child_init/3, make_children_actions/2]).
:- use_module(world, [
        %update(Board, RemoveDirt, AddDirt, RemoveBox, AddBox, RemoveChild, AddChild, RemoveRobot, AddRobot, NewBoard) :-
        update/10,
        % estos se usan de la forma method(Board, (X,Y))
        board_cell/1,
        playpen_cell/2,
        child_cell/2,
        robot_cell/2,
        box_cell/2,
        clean_cell_list/2,
        clean_cell/2,
        child_being_carried/2,
        dirtiable_cell_list/3,
        move_child/4,
        walkeable_cell/2,
        update_boxes/4,
        add_dirt/3
   ]).

%%%%%%%%%%%%%%%%%%% Child Utils %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Given an ammount of childs generates positions,
    % initializes them and updates the map.
    % child_init(+Board,+Ammount,-ChildListOut,-NewBoard)
    child_init(Board, Ammount, NewBoard) :-
        clean_cell_list(Board,CleanCells),
        select_k_items_randomly(Ammount, CleanCells, ChildListOut), % <- ChildPositions
        add_childs_to_map(Board,[], ChildListOut, NewBoard).

    make_children_actions(Board, NewBoard) :-
        ChildList = Board.child_cells,
        writeln(ChildList),
        get_results(Board, ChildList, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_results(Board, [], Board).

get_results(Board, [Child|ChildList], UpdatedBoard) :-
    child_behavior(Board, Child, NewDirtied, NewPosition, MovedBoxes, BoxesNewPos),
    move_child(Board, [Child], NewPosition, NewBoard),
    update_boxes(NewBoard, MovedBoxes, BoxesNewPos, BoardWithBoxes),
    add_dirt(BoardWithBoxes, NewDirtied, NewerBoard),
    get_results(NewerBoard, ChildList, UpdatedBoard).


add_childs_to_map(Board,RemoveChilds, NewChildren, NewBoard) :-
    update(Board, [],[],[],[], RemoveChilds, NewChildren,[],[], NewBoard).


child_behavior(Board,(R, C), [], [], [], []) :-
    child_being_carried(Board, (R, C)), !.
child_behavior(Board, (R, C), [], [], [], []) :-
    playpen_cell(Board, (R, C)), !.
child_behavior(Board, (R, C), Dirtied, [NewPos], MovedBoxes, Boxes_New_Pos) :-
    dirty_cells(Board, (R, C), Dirtied),
    choose_direction((R, C), ChosenPos),
    move_child(Board, (R, C),ChosenPos, NewPos, MovedBoxes, Boxes_New_Pos).

% Finds how many cells will the chils dirty and dirties them.
dirty_cells(Board, (R, C), Dirtied) :-
    get_3x3_neightbourhood((R, C), Neightbourhood),
    children_in_3x3(Board, Neightbourhood, Ammount),
    get_dirty_cells(6, Ammount, Ammount>=3, Dirty),
    select_k_items_randomly(Dirty, Neightbourhood, ChosenDirt),
    dirtiable_cell_list(Board, ChosenDirt, Dirtied).


% Gets the 3x3 neightbourhood of a cell in the matrix.
get_3x3_neightbourhood((X, Y), Ans) :-
    Z1 is X-1,
    Z2 is X + 1,
    Z3 is Y - 1,
    Z4 is Y + 1,
    Ans=[(X, Y), (Z1, Y), (Z2, Y), (X, Z3), (X, Z4 ), (Z1, Z3), (Z2, Z4), (Z1, Z4), (Z2, Z3)].

% Finds the ammount of children in the 3x3 neighbourhood.
children_in_3x3( _, [], K) :- K is 0, !.
children_in_3x3(Board, [(R, C)|T], Ammount) :-
    child_cell(Board, (R,C)), !,
    children_in_3x3(Board, T, Tail_Ammount),
    Ammount is Tail_Ammount + 1.
%? ESTO HACE FALTA?
children_in_3x3(Board, [_|T], Ammount) :-
    children_in_3x3(Board, T, Ammount).



% If there are 3 or more children in the 3x3 neighbourhood,
% will be 6. Otherwise, X will be equal to the ammount of
% children in the child's 3x3 neighbourhood.
get_dirty_cells(X, _, Bool, X) :-
    Bool, !.
get_dirty_cells(_, X, _, X).

% SW, W, NW, N, NE, E, SE, S
directions([[-1, -1], [-1, 0], [-1, 1], [0, 1], [1, 1], [1, 0], [1, -1], [0, -1]]).

choose_direction((X,Y), NewPos) :-
    directions(Dirs),
    random_member([H, V], Dirs),
    X_New is X+H,
    Y_New is Y+V,
    NewPos = (X_New, Y_New).

move_child(Board, OldPos, NewPos, NewPos, [], []) :-
    walkeable_cell(Board, NewPos),!. %not box

move_child(Board, (X,Y), (NewX, NewY), (NewX,NewY), [(NewX,NewY)], [CleanCell]) :-
    box_cell(Board, (NewX, NewY)),
    H is NewX - X,
    V is NewY - Y,
    next_clean_cell_in_dir(Board, (NewX, NewY), (H,V), CleanCell),!.

move_child(_, OldPos, _, OldPos, [], []).

next_clean_cell_in_dir(Board, Origin , _ , Origin) :-
    clean_cell(Board, Origin), !.

next_clean_cell_in_dir(Board, Origin , _ , _) :-
    not(board_cell(Origin)), !,fail.

next_clean_cell_in_dir(Board, (O1, O2), (DirX, DirY) , CleanCell) :-
    NewX is O1 + DirX,
    NewY is O2 + DirY,
    next_clean_cell_in_dir(Board, (NewX, NewY), (DirX, DirY), CleanCell).


% Selects k items randomly from a list L
select_k_items_randomly(0, _, []) :- !.
select_k_items_randomly(K, L, [X|Ans]) :-
    random_member(X, L), !,
    delete(L, X, New_L),
    New_K is K-1,
    select_k_items_randomly(New_K, New_L, Ans).