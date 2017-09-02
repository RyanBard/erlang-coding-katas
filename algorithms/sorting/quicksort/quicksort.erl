-module(quicksort).

-export([quicksort/1]).


quicksort([]) ->
    [];
quicksort(List) when is_list(List) ->
    [Pivot | Tail] = List,
    {LessThanPivot, GreaterThanPivot} = lists:partition(
        fun(Value) -> Value < Pivot end,
        Tail
    ),
    Begin = quicksort(LessThanPivot),
    End = quicksort(GreaterThanPivot),
    Begin ++ [Pivot | End].
