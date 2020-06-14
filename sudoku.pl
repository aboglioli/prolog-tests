:- use_module(library(clpfd)).
:- use_module(library(lists)).

s :-
  pregunta_heuristica(HeurNro),
  heuristica(HeurNro, Heuristica),

  pregunta_sudoku(SNro),
  test(SNro, Sudoku),

  time(sudoku(Sudoku, Heuristica)),
  maplist(writeln, Sudoku),

  writeln('----'),
  s.
  % continuar.

continuar :-
  writeln('Continuar? s/n : '),
  read(X),
  continue(X).

continue('s') :- s,!.
continue('S') :- s,!.
continue('n') :- continue('N').
continue('N') :- writeln('Alan Boglioli (legajo 38507)').

pregunta_sudoku(Selec) :-
 writeln('Puzzle:'),
 writeln('1. puzzle 1.'),
 writeln('2. puzzle 2.'),
 writeln('3. puzzle 3.'),
 writeln('4. puzzle 4.'),
 writeln('5. puzzle 5.'),
 read(Selec).

pregunta_heuristica(HeurNro) :-
  writeln('Heurística:'),
  writeln('1. ff'),
  writeln('2. leftmost'),
  writeln('3. min'),
  writeln('4. max'),
  writeln('5. ffc'),
  read(HeurNro).

heuristica(1, [ff]).
heuristica(2, [leftmost]).
heuristica(3, [min]).
heuristica(4, [max]).
heuristica(5, [ffc]).

/*
 * Solución 1
 */
sudoku(Rows) :- sudoku(Rows, []). % heurística por defecto
sudoku(Rows, Heuristic) :-
  % Comprueba que  matriz sea cuadrada de 9x9. Misma cantidad filas y columnas.
  length(Rows, 9),
  maplist(same_length(Rows), Rows),
  % Traduce la lista de listas en una lista simple, todos los elementos quedan en una misma lista unidimensional.
  % Similar a flatten(Lista, F).
  append(Rows, Vs), Vs ins 1..9,
  % Primera regla: todas las filas con valores distintos.
  maplist(all_distinct, Rows),
  % Transpone. Cambia filas por columnas.
  transpose(Rows, Columns),
  % Segunda regla: columnas con valores distintos.
  maplist(all_distinct, Columns),
  % Obtiene cada fila por separado.
  Rows = [A,B,C,D,E,F,G,H,I],
  % Tercera regla: comprueba que cada bloque de 3x3 sea distintos
  blocks(A, B, C), blocks(D, E, F), blocks(G, H, I), % primeras 3 filas, siguientes 3 filas y últimas 3 filas se pasan. Movimiento vertical.
  % Cuarta regla: diagonales con valores distintos.
  % diagonal1(Rows, D1),
  % all_distinct(D1),
  % diagonal2(Rows, D2),
  % all_distinct(D2),
  % Heurística: estrategia de selección y asignación de variables.
  flatten(Rows, Vs),
  labeling(Heuristic, Vs). % 

blocks([], [], []).
% Cada bloque de 3x3 debe ser distintos. Unifica con los 3 primeros valores de 3 filas continguas para obtener el bloque.
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
        all_distinct([A,B,C,D,E,F,G,H,I]),
        blocks(Bs1, Bs2, Bs3). % repite con siguiente bloque de 3x3. Movimiento horizontal.

% Obtener diagonales de matriz cuadrada
list_tail([_|Es], Es).
diagonal1([], []).
diagonal1([[E|_]|Ess], [E|D]) :-
    maplist(list_tail, Ess, Ess0),
    diagonal1(Ess0, D).

diagonal2(Ess,D) :-
    maplist(reverse, Ess, Fss),
    diagonal1(Fss, D).

/*
 * Solución 2
 * Otra solución que requiere menos cantidad de inferencias, exigue menos al motor de inferencias.
 */
sudoku_x(L, Heuristic):-  
  flatten(L,Lista),
  Lista ins 1..9,

  [R1,R2,R3,R4,R5,R6,R7,R8,R9] = L,

  [X11,X12,X13,X14,X15,X16,X17,X18,X19] = R1,
  [X21,X22,X23,X24,X25,X26,X27,X28,X29] = R2,
  [X31,X32,X33,X34,X35,X36,X37,X38,X39] = R3,
  [X41,X42,X43,X44,X45,X46,X47,X48,X49] = R4,
  [X51,X52,X53,X54,X55,X56,X57,X58,X59] = R5,
  [X61,X62,X63,X64,X65,X66,X67,X68,X69] = R6,
  [X71,X72,X73,X74,X75,X76,X77,X78,X79] = R7,
  [X81,X82,X83,X84,X85,X86,X87,X88,X89] = R8,
  [X91,X92,X93,X94,X95,X96,X97,X98,X99] = R9,

  all_different(R1), all_different(R2), all_different(R3),
  all_different(R4), all_different(R5), all_different(R6),
  all_different(R7), all_different(R8), all_different(R9),

  all_different([X11,X21,X31,X41,X51,X61,X71,X81,X91]),
  all_different([X12,X22,X32,X42,X52,X62,X72,X82,X92]),
  all_different([X13,X23,X33,X43,X53,X63,X73,X83,X93]),
  all_different([X14,X24,X34,X44,X54,X64,X74,X84,X94]),
  all_different([X15,X25,X35,X45,X55,X65,X75,X85,X95]),
  all_different([X16,X26,X36,X46,X56,X66,X76,X86,X96]),
  all_different([X17,X27,X37,X47,X57,X67,X77,X87,X97]),
  all_different([X18,X28,X38,X48,X58,X68,X78,X88,X98]),
  all_different([X19,X29,X39,X49,X59,X69,X79,X89,X99]),

  all_different([X11,X12,X13,X21,X22,X23,X31,X32,X33]),
  all_different([X41,X42,X43,X51,X52,X53,X61,X62,X63]),
  all_different([X71,X72,X73,X81,X82,X83,X91,X92,X93]),
  all_different([X14,X15,X16,X24,X25,X26,X34,X35,X36]),
  all_different([X44,X45,X46,X54,X55,X56,X64,X65,X66]),
  all_different([X74,X75,X76,X84,X85,X86,X94,X95,X96]),
  all_different([X17,X18,X19,X27,X28,X29,X37,X38,X39]),
  all_different([X47,X48,X49,X57,X58,X59,X67,X68,X69]),
  all_different([X77,X78,X79,X87,X88,X89,X97,X98,X99]),

  all_different([X11,X22,X33,X44,X55,X66,X77,X88,X99]),
  all_different([X19,X28,X37,X46,X55,X64,X73,X82,X91]),

  labeling(Heuristic,Lista).

/*
 * Runner: para obtener métricas.
 */
% Sudoku
run(0, _, _, _).
run(C, NroSudoku, NroHeuristica, Stream) :-
  C1 is C-1,
  test(NroSudoku, S),
  heuristica(NroHeuristica, H),
  % statistics(walltime, _),
  time(sudoku(S, H)),
  % statistics(walltime, [_, ExecutionTime]),
  % write(Stream, ExecutionTime),
  % write(Stream, ' - '),
  % write(Stream, C1),
  % write(Stream, S),
  % write(Stream, '\n'),
  run(C1, NroSudoku, NroHeuristica, Stream).

t(C, N, H) :-
  % open('results.csv', write, Stream),
  run(C, N, H, Stream).
  % close(Stream).

% Sudoku X
run_x(0, _, _, _).
run_x(C, NroSudoku, NroHeuristica, Stream) :-
  C1 is C-1,
  test(NroSudoku, S),
  heuristica(NroHeuristica, H),
  time(sudoku_x(S, H)),
  run_x(C1, NroSudoku, NroHeuristica, Stream).

t_x(C, N, H) :-
  run_x(C, N, H, Stream).


/*
 * Problemas de Sudoku
 */
test(1,[
  [_,6,_,1,_,4,_,5,_],
  [_,_,8,3,_,5,6,_,_],
  [2,_,_,_,_,_,_,_,1],
  [8,_,_,4,_,7,_,_,6],
  [_,_,6,_,_,_,3,_,_],
  [7,_,_,9,_,1,_,_,4],
  [5,_,_,_,_,_,_,_,2],
  [_,_,7,2,_,6,9,_,_],
  [_,4,_,5,_,8,_,7,_]
]).

test(2, [
  [_,_,4 ,_,_,3, _,7,_],
  [_,8,_ ,_,7,_, _,_,_],
  [_,7,_ ,_,_,8, 2,_,5],
  [4,_,_ ,_,_,_, 3,1,_],
  [9,_,_ ,_,_,_, _,_,8],
  [_,1,5 ,_,_,_, _,_,4],
  [1,_,6 ,9,_,_, _,3,_],
  [_,_,_ ,_,2,_, _,6,_],
  [_,2,_ ,4,_,_, 5,_,_]
]).

test(3, [
  [_,4,3,_,8,_,2,5,_],
  [6,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,1,_,9,4],
  [9,_,_,_,_,4,_,7,_],
  [_,_,_,6,_,8,_,_,_],
  [_,1,_,2,_,_,_,_,3],
  [8,2,_,5,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,5],
  [_,3,4,_,9,_,7,1,_]
]).

test(4, [
  [_,1,4,_,_,8,2,_,_],
  [2,8,5,4,9,3,6,7,1],
  [_,3,7,_,_,_,4,_,8],
  [8,2,_,6,_,_,_,4,_],
  [_,4,_,_,_,_,_,6,_],
  [_,_,6,_,_,4,_,_,2],
  [1,_,2,_,4,6,3,8,_],
  [_,6,3,8,1,_,_,2,4],
  [4,9,8,5,3,2,7,1,6]
]).

/* el siguiente es el sudoku escargot presentado por Arto Inkala, su creador en 2006, como el más difícil del mundo */
test(5, [
  [1,_,_,_,_,7,_,9,_],
  [_,3,_,_,2,_,_,_,8],
  [_,_,9,6,_,_,5,_,_],
  [_,_,5,3,_,_,9,_,_],
  [_,1,_,_,8,_,_,_,2],
  [6,_,_,_,_,4,_,_,_],
  [3,_,_,_,_,_,_,1,_],
  [_,4,_,_,_,_,_,_,7],
  [_,_,7,_,_,_,3,_,_]
]).

