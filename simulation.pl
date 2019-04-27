:- use_module(world, [initialize/6, log/1, playpen_child_list/2, percent/2]).
 % child_init(+Board,+Ammount,-ChildListOut,-NewBoard)
 % make_children_actions(Board, NewBoard)
:- use_module(child, [child_init/3, make_children_actions/2]).
:- use_module(robot, [path_to_closest_child/3]).
:- initialization(main).

main :-
    N is 4,
    M is 4,
    DirtPercent is 10,
    BoxPercent is 10,
    ChildAmmount is 3,
    %% Guardo los parametros iniciales para no tener que pasarlos a la simulacion
    T is 100,
    asserta(n(N)),
    asserta(m(M)),
    asserta(dirt_percent(DirtPercent)),
    asserta(box_percent(BoxPercent)),
    asserta(child_ammount(ChildAmmount)),
    asserta(t(T)),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    simulate(T,N,M,DirtPercent,BoxPercent, ChildAmmount),
    halt(1).

% Makes a single simulation
simulate(T,N,M,DirtPercent,BoxPercent, ChildAmmount) :-
    initialize(Board,N,M,DirtPercent,BoxPercent,ChildAmmount),
    child_init(Board, ChildAmmount, NewBoard),
    log(NewBoard),
    simulation_step(T, NewBoard, Updated),
    SimulationData = [].


simulation_step(T, Board, NewBoard) :- 
    T is 0, !,
    n(N),m(M),dirt_percent(DirtPercent),t(NewT),
    box_percent(BoxPercent),child_ammount(ChildAmmount),
    initialize(BrandNewBoard,N,M,DirtPercent, BoxPercent, ChildAmmount),
    child_init(BrandNewBoard, ChildAmmount, TheBoard),
    simulation_step(NewT, TheBoard, NewBoard).

simulation_step( _ , Board, Board) :- % si todos los ninos estan en corral termina la simulacion
    playpen_child_list(Board, Bag),
    child_ammount(N),
    length(Bag,N).

simulation_step( _ , Board, Board) :- % si llega al 60% de suciedad termina la simulacion
    length(Board.dirt_cells, N),
    percent(N, Percent),
    Percent > 60.

simulation_step(T, Board, NewBoard) :-
    write(T),
    writeln(" Steps Left: __________"),
    make_children_actions(Board, ChildrenModifiedBoard),
    make_robot_actions(ChildrenModifiedBoard, RobotModifiedBoard),
    log(RobotModifiedBoard),
    NewT is T - 1,
    writeln("______________________"),
    simulation_step(NewT, RobotModifiedBoard, NewBoard).

