-module(queens).
-export([queens/1]).

queens(N) -> queens(N, lists:seq(1,N,1)).

queens(0, _AllColumnPositions) -> [[]];
queens(N, AllColumnPositions) -> [
  [Column|ColumnsAlreadyTaken] ||
    ColumnsAlreadyTaken <- queens(N-1, AllColumnPositions),
    Column <- AllColumnPositions -- ColumnsAlreadyTaken,
    safe(Column, ColumnsAlreadyTaken)
].

safe(Column, ColumnsAlreadyTaken) -> safe(Column, ColumnsAlreadyTaken, 1).

safe(_, [], _) -> true;
safe(Column, [ColumnTaken|RestOfColumnsTaken], DistanceToColumnTaken) ->
  (Column /= ColumnTaken + DistanceToColumnTaken) and
  (Column /= ColumnTaken - DistanceToColumnTaken) and
  safe(Column, RestOfColumnsTaken, DistanceToColumnTaken + 1).
