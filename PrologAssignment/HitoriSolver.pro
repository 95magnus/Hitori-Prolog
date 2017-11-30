outputFile('./TestOutput/hitori_solved.txt').
inputFile('./TestInput/hitori_unsolved.txt').

/*
  0 - black
  1 - white
*/

/********************* dummy solution algorithms -> fill your correct algorithm here */

isValid(V, Cf):-
    	length(Cf, Lc),
		length(V, Lv),
    	L is Lv*Lv - Lc,
    	fillWithBlanks(Cf, L, R),
		append(Cf, R, Filled),
    	create2D(Filled, Lv, C),!,
		puzzle(P, V, C),
		ab(P),
		uniqueWhite(P).

fillWithBlanks(_, 0, _).
fillWithBlanks(R, L, Result):-
    			Next is L - 1,
    			fillWithBlanks(R, Next, R2),
    			append(R2, [-1], Result).


solverLaunchpad(P, R):-
    puzzle(P, _, C),
	flatten(C, Cf),
    badSolve(P, Cf, R).

badSolve(P, [], R):-	length(R,L),
					Length is round(sqrt(L)),
    				create2D(R, Length, A),!,
					puzzle(P, V, _),
    				puzzle(P1, V, A),
					write("\nDING! DING! DING! Solution found!\n"),
					write(P1), true.

badSolve(P, [E|Rest], R):-
		puzzle(P, V, _),
		isValid(V, R),
		(E = -1 -> append(R, [0], Rw),
		(badSolve(P, Rest, Rw)->true;append(R, [1], Rb),
		(isValid(V, Rb) -> badSolve(P, Rest, Rb))); append(R, [E], Rc),badSolve(P, Rest, Rc)).
		
		

solve(P):-ab(P),uniqueWhite(P).

create2D([], _, []).
create2D(List, L, [Row|Board]):-
  create2Drow(List, L, Row, Tail),
  create2D(Tail, L, Board).

create2Drow(Tail, 0, [], Tail).
create2Drow([Item|List], L, [Item|Row], Tail):-
  L1 is L-1,
  create2Drow(List, L1, Row, Tail).

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

% List of 0 (blacks) elements by coordinates
% e.g. [0,0,0,1] => [[0,0],[1,0],[0,1]]
blackList(C, N, B):- flatten(C, Cf), filterBlacks(Cf, 0, N, B).
filterBlacks([], _, _, []).
filterBlacks([C|Cs], I, N, [[X, Y]|B]):- C = 0, index(I, X, Y, N), I1 is I + 1, filterBlacks(Cs, I1, N, B), !.
filterBlacks([_|Cs], I, N, B):- I1 is I + 1, filterBlacks(Cs, I1, N, B), !.


blackChains(P, Bch):-
  puzzle(P, _, C),
  length(C, N),
  blackList(C, N, Bl),
  blackLinks(Bl, N, Bls),
  linkChains(Bls, Bch).

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

/*
addToChain(_, [], []):- !.

% Signle link, chain found
addToChain(Link, [Ch|Chains], [Ch1|R]):-
  intersection(Link, Ch, Li),
  Li \= [], !,
  union(Link, Ch, Ch1), !,
  addToChain(Link, Chains, R).


addToChain(Link, [Ch|Chains], [Ch|R]):-
  intersection(Link, Ch, Li),
  Li = [], !,
  addToChain(Link, Chains, R).

linkChains(Ls, Chs):- linkChains([], Ls, Chs).

% links
% if intersection(A, B) \= [], move both to chain, if either is in a chain, append it

% Base condition
% Sinle link left, add to chain if possible
% Sinle link left, add to new chain

linkChains([], _, _).

% Links are connected, add existing chain
linkChains([L1, L2|Links], [Ch1|Chains], R):-
  addToChain(L1, Chains, Ch1),
  addToChain(L2, Ch1, Ch2),
  linkChains([L2|Links], Chains).

% Links dont match, add to seperate chains
*/
/*
[
  [ [1,0],[0,1] ],
  [ [1,0],[2,1] ],
  [ [0,1],[1,2] ],
  [ [2,1],[1,2] ],
  [ [3,4],[4,5] ],
  [ [3,4],[2,5] ]
] => [
  [ [1,0],[0,1],[2,1],[1,2],[1,2],[2,1] ],
  [ [3,4],[4,5],[2,5] ]
]
*/

listContains([], _).
listContains([H|T], L):-
  member(H, L), !,
  listContains(T, L).

intersectsAtLeast(_, _, N):- N =< 0, !.
intersectsAtLeast([H|T], L, N):-
  member(H, L), !,
  N1 is N-1,
  intersectsAtLeast(T, L, N1).
intersectsAtLeast([_|T], L, N):-
  intersectsAtLeast(T, L, N).


%linkChains(Ls, Chs):- linkChains([], Ls, Chs).
%linkChains([], [], _).     % Base clause
%linkChains(Ch, [], Ch).   % No linked blacks base clause

% Create new chain from link
% Append current links to chains
% Merge chains

%linkChains(Link, Links, [Ch|Chains], [R|Chains]):- % Append current link to chains
%  chainAppend()
%  linkChains(Link, Links, Chains, R)

% blacks, vistited, next, prev, R


% while blacks, pop head of blacks
% add to visited
% find next neighbour
% if not prev and member of blacks, add to visited, but don't do recursive call
% if not in visited, recall with as next and current as prev

/*
blackFill([X,Y], Prev, Blacks, Visited, _, [Visited|Visits]):-
  X0 is X - 1, Y0 is Y - 1, X1 is X + 1, Y1 is Y + 1,
  ([X0,Y0] \= Prev, member([X0,Y0], Blacks) -> blackFill([X0,Y0], [X,Y], Blacks, [[X,Y]|Visited], _, Visits));
  ([X1,Y0] \= Prev, member([X0,Y0], Blacks) -> blackFill([X1,Y0], [X,Y], Blacks, [[X,Y]|Visited], _, Visits));
  ([X1,Y1] \= Prev, member([X0,Y0], Blacks) -> blackFill([X1,Y1], [X,Y], Blacks, [[X,Y]|Visited], _, Visits));
  ([X0,Y1] \= Prev, member([X0,Y0], Blacks) -> blackFill([X0,Y1], [X,Y], Blacks, [[X,Y]|Visited], _, Visits));

  subtract(Blacks, Visited, [B|RestBlacks]),
  blackFill(B, [], [B|RestBlacks], [], Visited).
*/
  /*
  blackFill([X,Y], Prev, Blacks, Visited, Chains):-
    X0 is X - 1, Y0 is Y - 1, X1 is X + 1, Y1 is Y + 1,
    ([X0,Y0] \= Prev, member([X0,Y0], Blacks) -> blackFill([X0,Y0], [X,Y], Blacks, [[X,Y]|Visited], Chains));
    ([X1,Y0] \= Prev, member([X1,Y0], Blacks) -> blackFill([X1,Y0], [X,Y], Blacks, [[X,Y]|Visited], Chains));
    ([X1,Y1] \= Prev, member([X1,Y1], Blacks) -> blackFill([X1,Y1], [X,Y], Blacks, [[X,Y]|Visited], Chains));
    ([X0,Y1] \= Prev, member([X0,Y1], Blacks) -> blackFill([X0,Y1], [X,Y], Blacks, [[X,Y]|Visited], Chains));
    % No more neighbours, backtrack
  */

% TODO:
% - Create a stack for neighbours to be visited
% - Fix cyclic
% - Chain edge detection

% Find surrounding neighbours, add to list
% apply blackfill to each neighbour

onEdge(Chains, N):- E is N - 1, onEdge(Chains, E, 0).
onEdge([], _, C):- C < 2, !.
onEdge([[H|T]|Chains], E, C):-
  blackEdge(H, E), !,
  C1 is C + 1,
  onEdge([T|Chains], E, C1).
onEdge([_|Chains], E, C):-
  onEdge(Chains, E, C).

blackEdge([0,_], _).
blackEdge([_, 0], _).
blackEdge([E,_], E).
blackEdge([_, E], E).

cyclic(Next, Prev, Visited):- Next \= Prev, member(Next, Visited).

% Adds a black twice if the chain is cyclic
chains([B|Blacks], Chains):- blackFill(B, [], [B|Blacks], [], Chains).

blackFill([], _, [], _, []):- !.

blackFill(Next, Prev, Blacks, Visited, [[Next|Visited]|Chains]):-
  cyclic(Next, Prev, Visited), !,
  blackFill([], Next, [], _, Chains).

blackFill([X, Y], Prev, Blacks, Visited, Chains):- % Find neighbour
  X1 is X - 1, Y1 is Y + 1, [X1,Y1] \= Prev,  % Left down
  member([X1, Y1], Blacks), !,
  blackFill([X1,Y1], [X,Y], Blacks, [[X,Y]|Visited], Chains).

blackFill([X, Y], Prev, Blacks, Visited, Chains):- % Find neighbour
  X1 is X + 1, Y1 is Y + 1, [X1,Y1] \= Prev,  % Right down
  member([X1, Y1], Blacks), !,
  blackFill([X1,Y1], [X,Y], Blacks, [[X,Y]|Visited], Chains).

blackFill([X, Y], Prev, Blacks, Visited, Chains):- % Find neighbour
  X1 is X + 1, Y1 is Y - 1, [X1,Y1] \= Prev,  % Right up
  member([X1, Y1], Blacks), !,
  blackFill([X1,Y1], [X,Y], Blacks, [[X,Y]|Visited], Chains).

blackFill([X, Y], Prev, Blacks, Visited, Chains):- % Find neighbour
  X1 is X - 1, Y1 is Y - 1, [X1,Y1] \= Prev,  % Left up
  member([X1, Y1], Blacks), !,
  blackFill([X1,Y1], [X,Y], Blacks, [[X,Y]|Visited], Chains).

% No more neighbours

blackFill([X,Y], _, Blacks, Visited, [[[X,Y]|Visited]|Chains]):- % Single black
  subtract(Blacks, [[X,Y]|Visited], [B|BlacksRest]), !,
  blackFill(B, [], BlacksRest, [], Chains).

% Add neigbours to list, when no more to add, attempt to visit


blackFill(_, _, Blacks, [_], [Visited|Chains]):- % No neighbours, start next chain
  subtract(Blacks, Visited, [B|BlacksRest]), !,
  blackFill(B, _, BlacksRest, [], Chains).


chainAppend([], Ch, Ch).
chainAppend([[C1, C2]|Links], Ch, [C2|R]):-
  member(C1, Ch), !,
  chainAppend(Links, Ch, R).
chainAppend([[C1, C2]|Links], Ch, [C1|R]):-
  member(C2, Ch), !,
  chainAppend(Links, Ch, R).
chainAppend([_|Links], Ch, R):-
  chainAppend(Links, Ch, R).

test:-
  puzzle(P, [[3,5,4,1,3],[2,4,1,4,5],[2,2,4,5,3],[4,3,5,4,2],[3,4,3,2,1]],
            [[1,1,1,1,1],
             [1,0,1,1,1],
             [0,1,0,1,1],
             [1,0,1,1,1],
             [1,1,1,1,1]]),
  puzzle(P, _, C), length(C, N), blackList(C, N, Bl), chains(Bl, Ch), writeLine(Ch).
  %blackChains(P, R1), writeLine(R1).
  %chainAppend([[[1, 0], [0, 1]], [[1, 0], [2, 1]], [[0, 1], [1, 2]], [[2, 1], [1, 2]]], [[0,1],[0,0]], R), writeLine(R).

% R = [[[1, 0], [0, 1]], [[1, 0], [2, 1]], [[0, 1], [1, 2]], [[2, 1], [1, 2]]]
%     [ [[1,0],[0,1],[2,1],[1,2],[1,2]] ].

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

falseColor(0, 1).
falseColor(C, 0):- C \= 0.
falseList(N, C, L) :- length(L, N), maplist(falseColor, C, L).
generateChecked(N, C, R) :- length(R, N), maplist(falseList(N), C, R).


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
