inputFile('./hitori_unsolved.txt').
outputFile('./hitori_solved.txt').
%inputFile('./TestInput/hitori_unsolved.txt').
%outputFile('./TestOutput/hitori_solved.txt').

% transpose(In, Out)
transpose([], []).
transpose([F|Fs], Ts) :- transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :- firstRestList(Ms, Ts, Ms1), transpose(Rs, Ms1, Tss).
firstRestList([], [], []).
firstRestList([[F|Os]|Rest], [F|Fs], [Os|Oss]) :- firstRestList(Rest, Fs, Oss).

% Replaces a element E at coordinates X, Y in 2D list L, new list R
replace(L, X, Y, E, R):- replace0(L, Y, X, E, R).
replace0([L|Ls], 0, Y, E, [R|Ls]):- replaceLine(L,Y,E,R), !.
replace0([L|Ls], X, Y, E, [L|Rs]):- X > 0, X1 is X-1, replace0(Ls, X1, Y, E, Rs).
replaceLine([_|Cs], 0, E, [E|Cs]).
replaceLine([C|Cs], Y, E, [C|Rs]):- Y > 0, Y1 is Y-1, replaceLine(Cs, Y1, E, Rs).

/* puzzle(puzzle, values, colors) */
puzzle([],[],[]).
puzzle([V, C], V, C).

index(I, X, Y, S):- X is I mod S, Y is I div S.
pair([V, C], V, C).

elementAt(Ess, X, Y, R):- nth0(Y, Ess, Es), nth0(X, Es, R).
valueAt(P, X, Y, R):- puzzle(P, Vals, _), nth0(Y, Vals, Rows), nth0(X, Rows, R).
colorAt(P, X, Y, R):- puzzle(P, _, Cols), nth0(Y, Cols, Rows), nth0(X, Rows, R).

% Finds the index of first occurence of a element, index I is -1 if element is not in list
findFirst0([E|_], E, 0):- !.
findFirst0([_|T], E, I):- findFirst0(T, E, I1), !, I is I1 + 1.
findFirst(L, E, I):- findFirst0(L, E, I) ; I = -1.

% Joins two list together in a list
pairedList([], [], []).
pairedList([A|Ar], [B|Br], [[A,B]|R]):- pairedList(Ar, Br, R).

% List of 0 (blacks) elements by coordinates
% e.g. [0,0,0,1] => [[0,0],[1,0],[0,1]]
blackList(C, N, B):- flatten(C, Cf), filterBlacks(Cf, 0, N, B).
filterBlacks([], _, _, []).
filterBlacks([C|Cs], I, N, [[X, Y]|B]):- C = 0, index(I, X, Y, N), I1 is I + 1, filterBlacks(Cs, I1, N, B), !.
filterBlacks([_|Cs], I, N, B):- I1 is I + 1, filterBlacks(Cs, I1, N, B), !.

% Starting technique - Finds cells with same value on both sides and makes middle white
middlePair(Pin, Pout):- puzzle(Pin, V, C),
  middlePair(V, C, C1), !, transpose(V, Vcol),transpose(C1, Ccol),
  middlePair(Vcol, Ccol, C2), !, transpose(C2, C3),
  puzzle(Pout, V, C3).
middlePair(V, C, Out):- middlePair(V, C, [], Out).
middlePair([], [], C, C):- !.
middlePair([Vs|Vss], [Cs|Css], Cacc, [Line|Cout]):- middlePairLine(Vs, Cs, [], Line), !, middlePair(Vss, Css, Cacc, Cout).
middlePairLine([], [], C, C):- !.
middlePairLine([V1, V2, V1|Vs], [_, C|Cin], Cs, [C|Cout]):- middlePairLine([V2,V1|Vs], [1|Cin], Cs, Cout).
middlePairLine([_|Vs], [C|Cin], Cs, [C|Cout]):- middlePairLine(Vs, Cin, Cs, Cout).

solve(V, R):-
  length(V, N),
  generateBlank(N, C),
  puzzle(P, V, C),
  %time(middlePair(P, P1)),
  I is (N * N) - 1,
  solve(P, I, N, R), !.

solve(P, -1, _, P):- valid(P), !.
solve(Pin, I, N, Pout):-
  I >= 0, index(I, X, Y, N),
  colorAt(Pin, X, Y, C0),
  changeColor(Pin, X, Y, C0, 0, P1),
  setWhiteUp(P1, X, Y, P2),
  setWhiteLeft(P2, X, Y, P3),
  I1 is I - 1,
  solve(P3, I1, N, Pout).

solve(Pin, I, N, Pout):- I >= 0, index(I, X, Y, N),
  colorAt(Pin, X, Y, C0), changeColor(Pin, X, Y, C0, 1, P1),
  I1 is I - 1,
  solve(P1, I1, N, Pout).

changeColor(Pin, X, Y, -1, Col, Pout):- puzzle(Pin, V, C), replace(C, X, Y, Col, C1), puzzle(Pout, V, C1), valid(Pout).
changeColor(P, _, _, C, _, P):- C \= -1.

setWhiteUp(Pin, X, Y, Pout):- Y1 is Y - 1, replace(Pin, X, Y1, 1, Pout).
setWhiteUp(P, _, _, P).
setWhiteLeft(Pin, X, Y, Pout):- X1 is X - 1, replace(Pin, X1, Y, 1, Pout).
setWhiteLeft(P, _, _, P).

valid(P):- ab(P), uniqueWhite(P), blackWall(P).

blank(-1).
blankList(N, R):- length(R, N), maplist(blank, R).
generateBlank(N, R) :- length(R, N), maplist(blankList(N), R).

uniqueWhiteRow([]).
uniqueWhiteRow([[V, 1]|Pls]):- not(member([V, 1], Pls)), !, uniqueWhiteRow(Pls).
uniqueWhiteRow([[_, C]|Pls]):- C \= 1, uniqueWhiteRow(Pls).
uniqueWhite([], []).
uniqueWhite([Vs|Vss], [Cs|Css]):- pairedList(Vs, Cs, Ps), uniqueWhiteRow(Ps), uniqueWhite(Vss, Css).
uniqueWhite(P):- puzzle(P, V, C), transpose(V, Vcol), transpose(C, Ccol), uniqueWhite(V, C), uniqueWhite(Vcol, Ccol).

abr([]).
abr([_]):-!.
abr([C, C|Cs]):- C \= 0, abr([C|Cs]).
abr([C1, C2|Cs]):- C1 \= C2, abr([C2|Cs]).
abs([]):- !.
abs([Cs|Css]):- abr(Cs), !, abs(Css).
ab(P):- puzzle(P, _, C), transpose(C, Ccol), abs(C), abs(Ccol).

blackWall(P):- puzzle(P, _, C), length(C, N), blackList(C, N, Bl), chains(Bl, Ch), cyclic(Ch), onEdge(Ch, N).
cyclic([]).
cyclic([[L|C]|Chains]):- not(member(L, C)), cyclic(Chains).

/* Unifies if there are no chains which contains at least two blacks on the puzzle edge */
onEdge(Chains, N):- E is N - 1, onEdge(Chains, E, []).
onEdge([], _, []):- !.
onEdge([], _, [H|T]):- H < 2, onEdge([], _, T). % If a chain has two or more blacks on edge, the chain seperates whites
onEdge([Ch|Chains], E, N):- chainEdges(Ch, E, 0, N1), !, onEdge(Chains, E, [N1|N]).
onEdge([_|Chains], E, C):- onEdge(Chains, E, C).

% Find number of blacks on the edge
chainEdges([], _, N, N).
chainEdges([H|T], E, N, C):- edgy(H, E), !, N1 is N + 1, chainEdges(T, E, N1, C).
chainEdges([_|T], E, N, C):- chainEdges(T, E, N, C).

edgy([0,_], _).
edgy([_, 0], _).
edgy([E, _], E).
edgy([_, E], E).

% Adds a black twice if the chain is cyclic
chains(Blacks, Chains):- chains(Blacks, [], Chains).
chains([], C, C):- !.
chains([B|Bs], Chs, [Visits|Chain]):-
  blackFill(B, [], [B|Bs], [], Visits),
  subtract([B|Bs], Visits, BlackRest),  % Remove already visited blacks and attempt to find new chains
  chains(BlackRest, Chs, Chain).

blackFill(_, _, [], V, V):- !.
blackFill(Next, Prev, _, Visited, [Next|Visits]):-
  Next \= Prev, member(Next, Visited), !, % Check if chain is cyclic
  blackFill(_, _, [], Visited, Visits).   % If so, ignore rest of chains and output chains found so far

blackFill(Next, Prev, Blacks, PreVisit, PostVisit):- % Find neighbours
  blackFillNW(Next, Prev, Blacks, [Next|PreVisit], V1),
  blackFillNE(Next, Prev, Blacks, V1, V2),
  blackFillSE(Next, Prev, Blacks, V2, V3),
  blackFillSW(Next, Prev, Blacks, V3, PostVisit).

blackFillNW([X, Y], Prev, Blacks, PreVisit, PostVisit):- % North west neightbour
  X1 is X - 1, Y1 is Y - 1, [X1,Y1] \= Prev, member([X1, Y1], Blacks), !,
  blackFill([X1,Y1], [X,Y], Blacks, PreVisit, PostVisit).
blackFillNW(_, _, _, V, V).

blackFillNE([X, Y], Prev, Blacks, PreVisit, PostVisit):- % North east neighbour
  X1 is X + 1, Y1 is Y - 1, [X1,Y1] \= Prev, member([X1, Y1], Blacks), !,
  blackFill([X1,Y1], [X,Y], Blacks, PreVisit, PostVisit).
blackFillNE(_, _, _, V, V).

blackFillSE([X, Y], Prev, Blacks, PreVisit, PostVisit):- % South east neighbour
  X1 is X + 1, Y1 is Y + 1, [X1,Y1] \= Prev, member([X1, Y1], Blacks), !,
  blackFill([X1,Y1], [X,Y], Blacks, PreVisit, PostVisit).
blackFillSE(_, _, _, V, V).

blackFillSW([X, Y], Prev, Blacks, PreVisit, PostVisit):- % South west neighbour
  X1 is X - 1, Y1 is Y + 1, [X1,Y1] \= Prev, member([X1, Y1], Blacks), !,
  blackFill([X1,Y1], [X,Y], Blacks, PreVisit, PostVisit).
blackFillSW(_, _, _, V, V).

formatSolution(P, S):- puzzle(P, V, C), formatPuzzle(V, C, [], S).
formatPuzzle([], [], S, S).
formatPuzzle([V|Vs], [C|Cs], S, [L|R]):- maplist(format, V, C, L), formatPuzzle(Vs, Cs, S, R).
format(_, 0, 'X'):- !.
format(V, C, V):- C \= 0.

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
solveProblems(N):- N > 0, readProblem(X, Y, P), solve(P, S), formatSolution(S, R), writeFullOutput(R, X, Y), !, N1 is N-1, solveProblems(N1).

%:- nl,nl,write(' try running "?- run."'), nl,nl,nl.

:- run.
:- halt.

/*
test:-
  V = %[[1, 4, 1, 5, 6, 5, 4],[6, 6, 3, 5, 4, 1, 4],[5, 3, 3, 1, 2, 1, 6],[6, 7, 6, 7, 1, 2, 5],[4, 4, 7, 2, 2, 6, 6],[1, 6, 2, 7, 5, 4, 3],[7, 7, 5, 2, 4, 5, 2]],
    [[1, 7, 5, 6, 7, 4, 4, 8, 2],
    [7, 1, 1, 3, 7, 9, 8, 6, 5],
    [2, 4, 7, 4, 1, 4, 3, 5, 6],
    [4, 3, 4, 1, 8, 2, 8, 7, 9],
    [6, 9, 1, 7, 4, 3, 2, 3, 1],
    [9, 8, 3, 8, 5, 6, 5, 1, 4],
    [6, 2, 2, 5, 6, 7, 1, 4, 1],
    [7, 6, 2, 4, 2, 3, 9, 9, 1],
    [3, 4, 1, 8, 9, 1, 4, 6, 5]],
     solve(V, R), formatSolution(R, S), writeOutput(S).

test1:-
  P =  [[[1, 4, 1, 5, 6, 5, 4],[6, 6, 3, 5, 4, 1, 4],[5, 3, 3, 1, 2, 1, 6],[6, 7, 6, 7, 1, 2, 5],[4, 4, 7, 2, 2, 6, 6],[1, 6, 2, 7, 5, 4, 3],[7, 7, 5, 2, 4, 5, 2]],
        [[-1, -1, -1, -1, -1, -1, -1],
         [-1, -1, -1, -1, -1, -1, -1],
         [-1, -1, -1, -1, -1, -1, -1],
         [-1, -1, -1, -1, -1, -1, -1],
         [-1, -1, -1, -1, -1, -1, -1],
         [-1, -1, -1, -1, -1, -1, -1],
         [-1, -1, -1, -1, -1, -1, -1]]],
        middlePair(P, R),
        %middlePair([[3,2,3,2], [5,6,6,6]], [[-1,-1,-1,-1],[-1,-1,-1,-1]], R),
        write(R).

intersectsAtLeast(_, _, N):- N =< 0, !.
intersectsAtLeast([H|T], L, N):- member(H, L), !, N1 is N-1, intersectsAtLeast(T, L, N1).
intersectsAtLeast([_|T], L, N):- intersectsAtLeast(T, L, N).

blackChains(P, Bch):- puzzle(P, _, C), length(C, N), blackList(C, N, Bl), blackLinks(Bl, N, Bls), linkChains(Bls, Bch).

linkChains(Ls, Chs):- linkChains([], Ls, Chs).
linkChains([],[],[]).     % Base clause
%linkChains(Ch, [], Ch).   % No linked blacks base clause
linkChains([], [L|Links], [Ch1|R]):- % Create new chain
  chainAppend(Links, [L], Ch1),
  linkChains(Ch1, Links, R).
linkChains([Ch|Chains], Links, [Ch1|R]):- % Append to existing chains
  chainAppend(Links, Ch, Ch1),
  linkChains(Chains, Links, R).
linkChains(Chains, [L|Links], [Ch1|R]):- % No chains to build upon, create new chain
  chainAppend(Links, [L], Ch1),
  linkChains(Chains, Links, R).

  chainAppend([], Ch, Ch).
  chainAppend([[C1, C2]|Links], Ch, [C2|R]):- member(C1, Ch), !, chainAppend(Links, Ch, R).
  chainAppend([[C1, C2]|Links], Ch, [C1|R]):- member(C2, Ch), !, chainAppend(Links, Ch, R).
  chainAppend([_|Links], Ch, R):- chainAppend(Links, Ch, R).


  blackLinks(P, Bcs):- puzzle(P, _, C), length(C, N), blackList(C, N, Bl), blackLinks(Bl, N, Bcs).
  blackLinks([], _, []).
  blackLinks([[X, Y]|Bs], N, [ [[X,Y],[X0,Y0]], [[X,Y],[X1,Y1]] |Ls]):-
    X0 is X - 1, Y0 is Y + 1, X0 >= 0, Y0 < N, member([X0, Y0], Bs),
    X1 is X + 1, Y1 is Y + 1, X1 < N, Y1 < N, member([X1, Y1], Bs), !,
    blackLinks(Bs, N, Ls).
  blackLinks([[X, Y]|Bs], N, [[[X,Y],[X0,Y0]]|Ls]):-
    X0 is X - 1, Y0 is Y + 1, X0 >= 0, Y0 < N, member([X0, Y0], Bs), !,
    blackLinks(Bs, N, Ls).
  blackLinks([[X, Y]|Bs], N, [[[X,Y],[X1,Y1]]|Ls]):-
    X1 is X + 1, Y1 is Y + 1, X1 < N, Y1 < N, member([X1, Y1], Bs), !,
    blackLinks(Bs, N, Ls).
  blackLinks([_|Bs], N, Chs):- blackLinks(Bs, N, Chs).

  isValid(V, Cf):-
    length(Cf, Lc),
    length(V, Lv),
    L is Lv*Lv - Lc,
    fillWithBlanks(Cf, L, R),
    append(Cf, R, Filled),
    create2D(Filled, Lv, C),!,
    puzzle(P, V, C),
    valid(P).

  fillWithBlanks(_, 0, _).
  fillWithBlanks(R, L, Result):-
    Next is L - 1,
    fillWithBlanks(R, Next, R2),
    append(R2, [-1], Result).

  solverLaunchpad(V, R):-
    length(V, N),
    generateBlank(N, C),
    puzzle(P, V, C),
    flatten(C, Cf),
    badSolve(P, Cf, R).
    % replace 0 vith x in values

  badSolve(P, [], R):- length(R,L), Length is round(sqrt(L)), create2D(R, Length, A), !,
    puzzle(P, V, _), puzzle(P1, V, A), valid(P1), lastCheck(P1, S), !,
    %write("\nDING! DING! DING! Solution found!\n"),
    formatSolution(S, Sf), writeFullOutput(Sf, Length, Length).

  badSolve(P, [E|Rest], R):-
    puzzle(P, V, _),
    isValid(V, R),
    (E = -1 -> append(R, [0], Rw),
    (badSolve(P, Rest, Rw)-> true ; append(R, [1], Rb),
    (isValid(V, Rb) -> badSolve(P, Rest, Rb))); append(R, [E], Rc), badSolve(P, Rest, Rc)).

  lastCheck(P, R):-
    not(valid(P)),
    puzzle(P,V,C),
    length(C, L),
    L1 is L - 1,
    replace(C, L1, L1, 1 , C1),
    puzzle(R, V, C1).
  lastCheck(P, P).

  create2D([], _, []).
  create2D(List, L, [Row|Board]):- create2Drow(List, L, Row, Tail), create2D(Tail, L, Board).
  create2Drow(Tail, 0, [], Tail).
  create2Drow([Item|List], L, [Item|Row], Tail):- L1 is L-1, create2Drow(List, L1, Row, Tail).


wob([_],[]):-!.
wob([C1,C2|C], R):-
	wob([C2|C], R1),
	C2 = 0,
	append([1], R1, R),!;
	wob([C2|C], R1),
    append([C1],R1,R)
    .

wobUse(C, R):-
    wob(C, R1),
    last(C, L),
    append(R1,[L],R).

wobBoard([],_):-!.
wobBoard([Row|Board], R):-
    wobUse(Row, R2),
    wobBoard(Board, R1),
    append(R1, R2, R).

setRules(C, R):-
    length(C, L),
    wobBoard(C, E),
	create2D(E, L, R),!.

applyLineRules(P, R):-
    puzzle(P,V,C),
    setRules(C, R1),
    transpose(R1,R2),
    setRules(R2,R3),
    reverseNested(R3,R4),
    reverse(R4,R5),
    setRules(R5,R6),
    transpose(R6, R7),
    reverseNested(R7,R8),
    reverse(R8,R9),
    setRules(R9,R10),
    reverseNested(R10,R11),
    puzzle(R, V, R11).

mirror(L,R):-reverseNested(L, R1),
    		reverse(R1,R2),reverseNested(R2,R).

reverseNested(L,R) :- rev(L,[],R).

rev([],A,A).
rev([H|T],A,R) :-
    ( is_list(H) ->        % If H is a list
      rev(H,[],X),         %   then reverse H as well
      rev(T,[X|A],R)
    ;
      rev(T,[H|A],R)
    ).
*/
