outputFile('./TestOutput/hitori_solved.txt').
inputFile('./TestInput/hitori_unsolved.txt').

/*
  0 - black
  1 - white
*/

/********************* dummy solution algorithms -> fill your correct algorithm here */
transpose([], []).
transpose([F|Fs], Ts) :- transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :- lists_firsts_rests(Ms, Ts, Ms1), transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :- lists_firsts_rests(Rest, Fs, Oss).

replace(L, X, Y, E, R):- replace0(L, Y, X, E, R).
replace0([L|Ls], 0, Y, E, [R|Ls]):- replace_column(L,Y,E,R), !.
replace0([L|Ls], X, Y, E, [L|Rs]):- X > 0, X1 is X-1, replace0(Ls, X1, Y, E, Rs).
replace_column([_|Cs], 0, E, [E|Cs]).
replace_column([C|Cs], Y, E, [C|Rs]):- Y > 0, Y1 is Y-1, replace_column(Cs, Y1, E, Rs).

/* puzzle(puzzle, values, colors) */
puzzle([],[],[]).
puzzle([V, C], V, C).

elemAt(Ess, X, Y, R):- nth0(Y, Ess, Es), nth0(X, Es, R).
valueAt(P, X, Y, R):- puzzle(P, Vals, _), nth0(Y, Vals, Rows), nth0(X, Rows, R).
colorAt(P, X, Y, R):- puzzle(P, _, Cols), nth0(Y, Cols, Rows), nth0(X, Rows, R).

findFirst0([E|_], E, 0):- !.
findFirst0([_|T], E, I):- findFirst0(T, E, I1), !, I is I1 + 1.
findFirst(L, E, I):- findFirst0(L, E, I) ; I = -1.

index(I, X, Y, S):- X is mod(I, S), Y is div(I, S).

pair([V, C], V, C).

pairedList([],[],[]).
pairedList([A|Ar], [B|Br], [[A,B]|R]):- pairedList(Ar, Br, R).

graph(P, Nodes):-
  puzzle(P, _, C),
  length(C, N)
  flattten(C, Cf),
  graph(Cf, 0, N, Nodes).

graph([H|T], I, N, Nodes):-
  I1 = I + 1,
  graph(T, I1, Ns).

blackList(P, B):-
  puzzle(P, _, C),
  length(C, N),
  flattten(C, Cf),
  blackList(Cf, 0, N, B).

blackList([C|Cs], I, N, B):-
  maplist(isBlack(I, N), C, R)
  I1 is I + 1,
  blackList(Cs, I1, [R|B]).

isBlack(C, I, N, [X, Y]):- C = 0, index(I, X, Y, N).

uniqueWhiteRow([]).
uniqueWhiteRow([Pl|Pls]):- pair(Pl, V, C), ( C = 1 -> not(member([V, 1], Pls)) ; true ), uniqueWhiteRow(Pls).
uniqueWhite([], []).
uniqueWhite([Vs|Vss], [Cs|Css]):- pairedList(Vs, Cs, Ps), uniqueWhiteRow(Ps), uniqueWhite(Vss, Css).
uniqueWhite(P):- puzzle(P, V, C), transpose(V, Vcol), transpose(C, Ccol), uniqueWhite(V, C), uniqueWhite(Vcol, Ccol).

abr([]).
abr([_]):-!.
abr([P1, P2|Ps]):- ( P1 = P2 -> P1 \= 0 ; true ), abr([P2|Ps]).
abs([]).
abs([Cs|Css]):- abr(Cs), abs(Css).
ab(P):- puzzle(P, _, C), transpose(C, Ccol), abs(C), abs(Ccol).

falseColor(C, R):- ( C = 0 -> R is 1 ; R is 0 ).

%falseColor(0, 1).
%falseColor(C, 0): C \= 0.

falseList(N, C, L) :- length(L, N), maplist(falseColor, C, L).
generateChecked(N, C, R) :- length(R, N), maplist(falseList(N), C, R).

floodFill(P):-
  puzzle(P, _, C),
  length(C, N),
  generateChecked(N, C, G),
  flatten(C, Cf),
  findFirst(Cf, 1, I),
  index(I, X, Y, N),
  ( I \= -1 -> !, floodFill(C, [X,Y], G) ; true).
floodFill(C, [X,Y|Is], Chl):-
  length(C, S),
  replace(Chl, X, Y, 1, Chl1),
  X0 is X - 1, X1 is X + 1, Y0 is Y - 1, Y1 is Y + 1,
  ( X0 >= 0 -> elemAt(Chl, X0, Y, C0), (C0 = 0 -> (floodFill(C, [X0, Y|Is], Chl1)->true) ; true) ; true),%flatten([X0, Y, Is], I0), floodFill(C, I0, Chl1) ; true) ; true),
  ( X1 <  S -> elemAt(Chl, X1, Y, C1), (C1 = 0 -> (floodFill(C, [X1, Y|Is], Chl1)->true) ; true) ; true),%flatten([X1, Y, Is], I1), floodFill(C, I1, Chl1) ; true) ; true),
  ( Y0 >= 0 -> elemAt(Chl, X, Y0, C2), (C2 = 0 -> (floodFill(C, [X, Y0|Is], Chl1)->true) ; true) ; true),%flatten([X, Y0, Is], I2), floodFill(C, I2, Chl1) ; true) ; true),
  ( Y1 <  S -> elemAt(Chl, X, Y1, C3), (C3 = 0 -> (floodFill(C, [X, Y1|Is], Chl1)->true) ; true) ; true),%flatten([X, Y1, Is], I3), floodFill(C, I3, Chl1) ; true) ; true),
  floodFill(C, Is, Chl1), !.
floodFill(_, [], Chl):- flatten(Chl, C), not(member(0, C)), !.

test0:-
  puzzle(P0, [[1,2],[3,4]],[[0,0],[0,0]]),  % true
  floodFill(P0).
test1:-
  puzzle(P1, [[1,2],[3,4]],[[1,0],[0,1]]),  % false
  floodFill(P1).
test2:-
  puzzle(P2, [[1,2],[3,4]],[[0,1],[0,0]]),  % true
  floodFill(P2).
test3:-
  puzzle(P3, [[1,2],[3,4]],[[0,1],[1,1]]),  % true
  floodFill(P3).
test4:-
  puzzle(P4, [[1,2],[3,4]],[[1,0],[0,0]]),  % true
  floodFill(P4).
test5:-
  puzzle(P5, [[1,2],[3,4]],[[0,1],[1,0]]),  % false
  floodFill(P5).
test6:-
  puzzle(P6, [[1,2],[3,4]],[[1,1],[1,1]]),  % true
  floodFill(P6).

test:-
  puzzle(_, [[3,5,4,1,3],[2,4,1,4,5],[2,2,4,5,3],[4,3,5,4,2],[3,4,3,2,1]],
            [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]]).
  %puzzle(P0, [[1,2],[3,4]],[[0,0],[0,0]]),  % true
  %puzzle(P1, [[1,2],[3,4]],[[1,0],[0,1]]),  % false
  %puzzle(P2, [[1,2],[3,4]],[[0,1],[0,0]]),  % true
  %puzzle(P3, [[1,2],[3,4]],[[0,1],[1,1]]),  % true
  %puzzle(P4, [[1,2],[3,4]],[[1,0],[0,0]]),  % true
  %puzzle(P5, [[1,2],[3,4]],[[0,1],[1,0]]),  % false

:- test0.
:- not(test1).
:- test2.
:- test3.
:- test4.
:- not(test5).
:- test6.

/* doSolve(SizeX,SizeY,Input,Output) */
doSolve(5,_,_,[[1, 2 ,3,'X',5],[4,1,5,3,2],[2,'X',1,'X',3],[5,3,'X',1,4],[3,'X',4,5,'X']]):-!.
doSolve(7,_,_,[['X',4,1,'X',6,5,'X'],[6,'X',3,5,'X',1,4],[5,3,'X',1,2,'X',6],['X',7,6,'X',1,2,5],[4,'X',7,2,'X',6,'X'],[1,6,2,7,5,4,3],[7,'X',5,'X',4,'X',2]]):-!.

doSolve(_,_,Solution,Solution).

/********************* writing the result */
writeFullOutput(S, X, Y):- write(X), write('x'), write(Y), nl, writeOutput(S).

writeOutput([]).
writeOutput([E|R]):- writeLine(E), writeOutput(R).

writeOutput(_, []).
writeOutput(S, P):- write(S), nl, writeOutput(P).

writeLine([]):- nl.
writeLine([E|R]):- write(' '), write(E), writeLine(R).

/********************** reading the input */
readProblem(N,M,Problem):- readInt(N), readInt(M), M=N, length(Problem, M), readProblemLines(N,Problem).

readProblemLines(_,[]).
readProblemLines(N,[H|T]):- length(H,N), readLine(H), readProblemLines(N,T).

readLine([]).
readLine([E|R]):- readInt(E), readLine(R).

readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- N>=48, N<58, N1 is N-48.

/*********************** global control: starting the algorithm and the reading */
run:- inputFile(IF), see(IF), outputFile(F), tell(F), readInt(N), write(N), nl, solveProblems(N), told, seen, !.
run:- told, seen. /* close the files */

solveProblems(0).
solveProblems(N):- N>0, readProblem(X, Y, I), doSolve(X, Y, I, S), writeFullOutput(S, X, Y), !, N1 is N-1, solveProblems(N1).

%:- nl,nl,write(' try running "?- run."'), nl,nl,nl.

%:- run.
%:- halt.
