%%%%%%%%%%%%%%%%
%  Game board  %
%%%%%%%%%%%%%%%%

% Definiendo el module para trabajo con mapa
:- module(world, [
    initialize/6,
    log/1,
    update/10,
    % estos se usan de la forma method(Board, (X,Y))
    board_cell/1,
    playpen_cell/2,
    child_cell/2,
    robot_cell/2,
    clean_cell_list/2,
    clean_cell/2,
    child_being_carried/2,
    dirtiable_cell_list/3,
    walkeable_cell_list/2,
    walkeable_cell/2,
    move_child/4,
    update_boxes/4,
    box_cell/2,
    percent/2,
    playpen_child_list/2
    ]).

% X1,Y1 needs to be instantiated
neighbor_cells((X1,Y1), (X2, Y2)) :- board_cell((X1,Y1)), X2 is X1 + 1, Y2 is Y1, board_cell((X2,Y2)).
neighbor_cells((X1,Y1), (X2, Y2)) :- board_cell((X1,Y1)), X2 is X1, Y2 is Y1 + 1, board_cell((X2,Y2)).
neighbor_cells((X1,Y1), (X2, Y2)) :- board_cell((X1,Y1)), X2 is X1 - 1, Y2 is Y1, board_cell((X2,Y2)).
neighbor_cells((X1,Y1), (X2, Y2)) :- board_cell((X1,Y1)), X2 is X1, Y2 is Y1 - 1, board_cell((X2,Y2)).


board_cell((X, Y)) :-
    nonvar(X),
    nonvar(Y), !,
    once(size((N,M))),
    X < N,
    X >= 0,
    Y < M,
    Y >= 0.

board_cell((X,Y)) :-
    size((N,M)),
    SizeX is N - 1,
    SizeY is M - 1,
    between(0, SizeX, X),
    between(0, SizeY, Y).

n_matrix(N, Rows) :-
    length(Rows, N),
    maplist(length_list, Rows).

length_list([]).

empty_list(L,N) :-
    length(L,N),
    maplist(zero,L).
zero(Element) :-
    Element is 0.

% linear id of point (X,Y)
% X,Y must be instantiated.
id_in_list((X,Y), Id) :-
    size((_,M)),
    Id is X*M + Y.


%%%%%%%%%%%%%%% Playpen Generation %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    dfs(Root, K ,Playpen) :-
        dfs([Root], [], K, Playpen).

    dfs( _, Visited, K, Visited) :- length(Visited,K).

    dfs([H|T], Visited, K, Playpen) :-
        member(H, Visited),
        dfs(T, Visited, K, Playpen).

    dfs([H|T], Visited, K, Playpen) :-
        not(member(H,Visited)),
        findall((X,Y), neighbor_cells(H,(X,Y)), Bag),
        append( T, Bag, ToVisit), 
        dfs(ToVisit, [H | Visited], K, Playpen).

    corral_cell_list(K, CorralCellsO):-
        board_cell((X,Y)),
        dfs((X,Y), K, CorralCellsO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% Dirt Generation %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % dirt_cell_list(+Ammount, -DirtCellList)
    dirt_cell_list(Restricted, Ammount, DirtCellList) :-
        not_restricted_cell_list(Restricted, CleanCells),
        select_k_items_randomly(Ammount, CleanCells, DirtCellList).


    % percent(Number, Percent)
    percent(Number, Percent) :-
        nonvar(Percent),!,
        size((N,M)),
        Dim is N*M,
        Number is (Percent*Dim) div 100.
    percent(Number, Percent) :-
        nonvar(Number),
        size((N,M)),
        Dim is N*M,
        Percent is (100 * Number) div Dim.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%% Box Generation %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    box_list(Restricted, Ammount, BoxList) :-
        findall((X,Y), board_cell((X,Y)), BoardCells),
        list_difference(BoardCells, Restricted, CleanCells),
        select_k_items_randomly(Ammount, CleanCells, BoxList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%% List Utils %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Replaces the nth element of the list. DOES NOT HANDLE IOR.
    replace_nth_element([_|T], K, NewElement, [NewElement|T]) :- 
        K is 0, !.
    replace_nth_element([H|T], N, NewElement, [H|R] ) :-
        N > 0,
        NewN is N - 1,
        replace_nth_element(T, NewN, NewElement, R).

    % Removes elements in Substract from List
    % list_difference(+List, +Substract, -Rest)
    list_difference([], _, []) :- !.
    list_difference([H|T], Substract, Rest) :-
        member(H, Substract), !,
        list_difference(T,Substract, Rest).
    list_difference([H|T], Substract, [H|Rest]) :-
        list_difference(T,Substract, Rest).

    all_ones_list(L, N) :-
        length(L,N),
        maplist(one ,L).
    one(Element) :-
        Element is 1.

    % sum_string_list(+List,-SumString) :-
    sum_string_list([], SumString, SumString).
    sum_string_list([H|Tail], SumString, OutSumString) :-
        string_concat(SumString, H, NewSum),
        sum_string_list(Tail, NewSum, OutSumString).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%% World Utils %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % True if (X,Y) is playpen cell
    % playpen_cell(+Board, ?X, ?Y)
    playpen_cell(Board, (X, Y)) :-
        member((X,Y), Board.playpen).

    dirt_cell(Board, (X, Y)) :-
        member((X,Y), Board.dirt_cells).

    box_cell(Board, (X,Y)) :-
        member((X,Y), Board.box_cells).

    child_cell(Board, (X,Y)) :-
        member((X,Y), Board.child_cells).

    robot_cell(Board, (X,Y)) :-
        member((X,Y), Board.robot_cells).

    child_being_carried(Board, (X,Y)) :-
        member((X,Y), Board.robot_cells),
        member((X,Y), Board.child_cells).


    playpen_child_list(Board, ChildsInPlaypen) :-
        list_difference(Board.playpen, Board.child_cells, NotChildren),
        list_difference(Board.playpen, NotChildren, ChildsInPlaypen).
        
        

    playpen_child(Board, (X,Y)) :-
        playpen_cell(X,Y),
        child_cell(X,Y).

    move_child(Board, OldPos, [], Board ) :- !.
    % move_child(Board, OldPos, NewPos, NewBoard)
    move_child(Board, OldPos, NewPos, NewBoard) :-
        update_attr(Board.child_cells, OldPos, NewPos, NewChildren),
        NewBoard = board{
            playpen: Board.playpen,
            dirt_cells: Board.dirt_cells,
            box_cells: Board.box_cells,
            child_cells: NewChildren,
            robot_cells: Board.robot_cells
        }.

    update_boxes(Board, OldBoxes, NewBoxes, MovedBoxesBoard) :-
        update_attr(Board.box_cells, OldBoxes, NewBoxes, NewBoxesCells),
        MovedBoxesBoard = board{
            playpen:Board.playpen,
            dirt_cells:Board.dirt_cells,
            box_cells:NewBoxesCells,
            child_cells:Board.child_cells,
            robot_cells:Board.robot_cells
        }.

        add_dirt(Board, NewDirt, NewBoard) :-
            append(Board.dirt_cells, NewDirt, NewDirtCells),
            NewBoard = board{
                playpen:Board.playpen,
                dirt_cells:NewDirtCells,
                box_cells:Board.box_cells,
                child_cells:Board.child_cells,
                robot_cells:Board.robot_cells
            }.

    not_restricted_cell_list(Restricted, CleanCells) :-
        findall((X,Y), board_cell((X,Y)), BoardCells),
        list_difference(BoardCells, Restricted, CleanCells).

    % playpen_cell(+Board, ?X, ?Y)
    clean_cell_list(Board, List) :-
        append(Board.playpen, Board.dirt_cells, Result1),
        append(Result1, Board.box_cells, Result2),
        append(Result2, Board.child_cells, Result3),
        append(Result3, Board.robot_cells, Result4),
        append(Result4, Board.dirt_cells, Result5),
        findall((X,Y), board_cell((X,Y)), BoardCells),
        list_difference(BoardCells, Result5, List).


    % Given a List of chosen dirt filters the non valid ones
    dirtiable_cell_list(Board, List, OList) :- % Si esta afuera cuenta como desicion pero no la ensucio
        append(Board.playpen, Board.dirt_cells, Result1),
        append(Result1, Board.box_cells, Result2),
        append(Result2, Board.dirt_cells, Result3),
        findall((X,Y), board_cell((X,Y)), BoardCells),
        list_difference(List, BoardCells, NotInBoard),
        append(Result3, NotInBoard, Result4),
        list_difference(List, Result4, OList).

    walkeable_by_child(Board, List, LO) :-
        findall((X,Y), board_cell((X,Y)), BoardCells),
        list_difference(List, BoardCells, NotInBoard),
        append(List, NotInBoard, LO).

    walkeable_cell(Board, (X,Y)) :-
        board_cell((X,Y)),
        not(box_cell(Board, (X,Y))),
        not(robot_cell(Board,(X,Y))),
        not(child_cell(Board,(X,Y))).

    walkeable_by_robot_list(Board, List) :-
        findall((X,Y), board_cell((X,Y)), BoardCells),
        list_difference(BoardCells, Board.box_cells, Result1),


    walkeable_cell_list(Board, OList) :- % Si esta afuera cuenta como decision pero no la ensucio
        findall((X,Y), board_cell((X,Y)), BoardCells),
        list_difference(BoardCells, Board.box_cells, Result1),
        list_difference(Board.child_cells, Board.playpen, NotInPlaypen),
        list_difference(Result1, NotInPlaypen, OList).

    clean_cell(Board, (X,Y)) :-
        board_cell((X,Y)),
        not(box_cell(Board, (X,Y))),
        not(robot_cell(Board,(X,Y))),
        not(child_cell(Board,(X,Y))),
        not(dirt_cell(Board,(X,Y))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    initialize(Board, N, M, DirtPercent, BoxPercent, ChildrenAmmount) :-
        assertz(size((N,M))),
        corral_cell_list(ChildrenAmmount, Playpen),
        percent(DirtAmmount, DirtPercent),
        dirt_cell_list(Playpen,DirtAmmount, DirtCells),
        append(Playpen, DirtCells, Restricted),
        percent(BoxAmmount, BoxPercent),
        box_list(Restricted, BoxAmmount, BoxCells),
        Board = board{
            playpen:Playpen,
            dirt_cells:DirtCells,
            box_cells:BoxCells,
            child_cells:[],
            robot_cells:[]
        },
        log(Board),
        writeln("Board Initialized").

    log(Board) :-
        writeln("-------------------------------"),
        write("Playground: "),
        writeln(Board.playpen),
        write("Dirt: "),
        writeln(Board.dirt_cells),
        write("Boxes: "),
        writeln(Board.box_cells),
        write("Children: "),
        writeln(Board.child_cells),
        write("Robots: "),
        writeln(Board.robot_cells),
        writeln("-------------------------------").


    update(Board, RemoveDirt, AddDirt, RemoveBox, AddBox, RemoveChild, AddChild, RemoveRobot, AddRobot, NewBoard) :-
        update_attr(Board.dirt_cells, RemoveDirt,AddDirt, NewDirtCells),
        update_attr(Board.box_cells, RemoveBox, AddBox, NewBoxCells),
        update_attr(Board.child_cells, RemoveChild, AddChild, NewChildCells),
        update_attr(Board.robot_cells, RemoveRobot,AddRobot, NewRobotCells),
        NewBoard = board{
            playpen:Board.playpen,
            dirt_cells:NewDirtCells,
            box_cells:NewBoxCells,
            child_cells:NewChildCells,
            robot_cells:NewRobotCells
        }.


update_attr(Attr, Remove, Add, Updated) :-
    list_difference(Attr, Remove, NewAttr),
    append(NewAttr, Add, UpdatedAttr),
    list_to_set(UpdatedAttr, Updated).


% True if Y is less than X
% less_than(+X,?Y)
less_than(X,Y) :-
    findall(Var, between(0, X, Var), Bag),
    random_permutation(Bag, Permutation),
    member(Y,Permutation).


% Selects k items randomly from a list L
select_k_items_randomly(0, _, []) :- !.
select_k_items_randomly(K, L, [X|Ans]) :-
    random_member(X, L), !,
    delete(L, X, New_L),
    New_K is K-1,
    select_k_items_randomly(New_K, New_L, Ans).