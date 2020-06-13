:- use_module(library(clpfd)).
:- use_module(library(lists)).

node(espana).
node(portugal).
node(andorra).
node(francia).
arc(espana,portugal). % espana != portugal
arc(espana,andorra). % espana != andorra
arc(espana,francia). % espana != francia
arc(andorra,francia). % andorra != francia

% node(1).
% node(2).
% node(3).
% arc(1, 2).
% arc(1, 3).

nodes(N) :-
  setof(X, node(X), N).
arcs(A) :-
  setof([X,Y], arc(X,Y), A).
arcs_of_node(N, L) :-
  setof(X, arc(N, X), L).

display([]) :- nl.
display([T|Rest]) :- write(T), write(' '), display(Rest).

assign_node(NumColors, N, R) :-
  C in 1..NumColors,
  labeling([], [C]),
  R = [N, C].

assign_all(Nodes, NumColors, R) :-
  maplist(assign_node(NumColors), Nodes, R).

check_color(_, [], _).
check_color(N, [A|Arcs], R) :-
  member([N, C1], R),
  member([A, C2], R),
  % display(['- Compare', [N, C1], [A, C2]]),
  not(C1 = C2),
  % display(['-- OK']),
  check_color(N, Arcs, R).

check(_, N) :- not(arcs_of_node(N, _)).
check(R, N) :-
  % display(['- Node:', N]),
  arcs_of_node(N, Arcs),
  % display(['- Arcs:', Arcs]),
  check_color(N, Arcs, R).

solve(NumColors, R) :-
  nodes(Nodes),
  assign_all(Nodes, NumColors, R),
  % display(['#', R]),
  maplist(check(R), Nodes).

solve(NumColors) :-
  setof(X, solve(NumColors, X), R),
  length(R, L),
  display(['# Soluciones:', L, '; Cant. colores:', NumColors]),
  maplist(writeln, R).

solve :-
  NumColors in 1..10,
  labeling([], [NumColors]),
  solve(NumColors).
