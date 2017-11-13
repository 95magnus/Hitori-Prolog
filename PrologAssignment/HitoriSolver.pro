outputFile('./TestOutput/hitori_solved.txt').
inputFile('./TestInput/hitori_unsolved.txt').

/********************* dummy solution algorithms -> fill your correct algorithm here */
transpose([], []).
transpose([F|Fs], Ts) :- transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :- lists_firsts_rests(Ms, Ts, Ms1), transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :- lists_firsts_rests(Rest, Fs, Oss).

/* puzzle(puzzle, values, colors) */
puzzle([],[],[]).
puzzle([V, C], V, C).

/* checkWhites(values, colors) */
uniqueWhiteRow([]).
uniqueWhiteRow([P|Ps]):- pair(P, V, C), ( C = 1 -> not(member([V, 1], Ps)) ; true ), uniqueWhiteRow(Ps).
uniqueWhite([], []).
uniqueWhite([Vs|Vss], [Cs|Css]):- pairedList(Vs, Cs, Ps), uniqueWhiteRow(Ps), uniqueWhite(Vss, Css).
uniqueWhite(P):- puzzle(P, V, C), transpose(V, Vcol), transpose(C, Ccol), uniqueWhite(V, C), uniqueWhite(Vcol, Ccol).

% Generate list of false(0), C is list of colors
falseColor(C, R):- ( C = 0 -> R is 1 ; R is 0 ).
falseList(N, C, L) :- length(L, N), maplist(falseColor, C, L).
generateChecked(N, C, R) :- length(R, N), maplist(falseList(N), C, R).  % Genereate 2D list of false

pairedList([],[],[]).
pairedList([A|Ar], [B|Br], [[A,B]|R]):- pairedList(Ar, Br, R).

pair([V, C], V, C).

valueAt(P, X, Y, R):- puzzle(P, Vals, _), nth0(Y, Vals, Rows), nth0(X, Rows, R).
colorAt(P, X, Y, R):- puzzle(P, _, Cols), nth0(Y, Cols, Rows), nth0(X, Rows, R).

/************ DANGER ZONE - Predicates whitin may or may not work as intended */

/* blacksHorRow(puzzle) */
blacksHorRow([]).
blacksHorRow([_]):- !.
blacksHorRow([X, Y | R]):- ( X = Y ->  number(X), number(Y) ; ! ), blacksHorRow([Y|R]).


/* checkedFilled(array) */
allChecked([]).
allChecked([H|T]):- not(member(0, H)), allChecked(T).

/* floodFill(puzzle) */
floodFill([]).
floodFill(P):- puzzle(P, _, C), length(N, P), generateChecked(N, C, G), floodFill(C, G, []).
/* floodFill(puzzle, checked(2D)) */
floodFill([], G, G).
%floodFill([C|Cr], [Checked|CheckedRest], R):- ( C = 0  -> floodFill(Cr, CheckedRest, R) ;  ),

/************ DANGER ZONE end */

test:-
  puzzle(P, [[1,1],[1,1]], [[1,0],[0,1]]),
  %valueAt(P, X, Y, 3),
  %write(X), nl, write(Y).
  puzzle(P, V, C),
  writeOutput('Orignial V', V),
  writeOutput('Orignial C', C),
  transpose(V, V1),
  transpose(C, C1),
  writeOutput('Transposed V', V1),
  writeOutput('Transposed C', C1),

  uniqueWhite(P).
  %uniqueWhiteRow([1,2,3,1],[1,1,1,1]).

  %generateChecked(2, [[0,0],[0,0]], L),
  %writeOutput('Genereated checked list:', L),
  %allChecked(L).


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
