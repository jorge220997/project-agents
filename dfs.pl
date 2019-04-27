bfs(Root) :-
    dfs([Root], []).

bfs([], _ ).

bfs([H|T], Visited) :-
    member(H, Visited),
    dfs(T, Visited).

bfs([H|T], Visited) :-
    not(member(H,Visited)),
    findall(Point, neighbor_cells(H,Point), Bag),
    append(Bag, T, ToVisit),
    dfs(ToVisit, [H | Visited]).


