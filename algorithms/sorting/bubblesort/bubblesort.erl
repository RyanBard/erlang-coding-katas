-module(bubblesort).

-export([bubblesort/1]).


bubblesort([]) ->
    [];
bubblesort([Value]) ->
    [Value];
bubblesort(List) when is_list(List) ->
    bubblesort_helper(List, length(List), true).


bubblesort_helper(List, 1, false) ->
    List;
bubblesort_helper(List, 1, true) ->
    bubblesort_helper(List, length(List), false);
bubblesort_helper(List, I, RecentlySwapped) ->
    Value1 = lists:nth(I - 1, List),
    Value2 = lists:nth(I, List),
    % Be careful here.  If not for the =<, duplicates would infinite recurse
    % due to the numbers that are the same constantly resulting in the
    % RecentlySwapped being passed in as true (2 swaps with 2, resulting in
    % the same List so the RecentlySwapped will never remain false for a whole
    % loop).
    case Value1 =< Value2 of
        true ->
            bubblesort_helper(List, I - 1, RecentlySwapped);
        false ->
            SwappedList = lists:sublist(List, I - 2) ++ [Value2, Value1] ++ lists:sublist(List, I + 1, length(List)),
            bubblesort_helper(SwappedList, I - 1, true)
    end.
