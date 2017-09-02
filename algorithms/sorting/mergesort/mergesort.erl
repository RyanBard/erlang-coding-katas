-module(mergesort).

-export([mergesort/1]).


mergesort([]) ->
    [];
mergesort([V]) ->
    [V];
mergesort(List) when is_list(List) ->
    Len = length(List),
    MidPoint = floor(Len / 2),
    FirstHalf = lists:sublist(List, MidPoint),
    % It is ok that MidPoint + Len is greater than the list here, it will just
    % return the rest of the list.
    LastHalf = lists:sublist(List, MidPoint + 1, Len),
    SortedFirstHalf = mergesort(FirstHalf),
    SortedLastHalf = mergesort(LastHalf),
    MergedInReverse = merge(SortedFirstHalf, SortedLastHalf, []),
    lists:reverse(MergedInReverse).


merge([], [], Accumulator) ->
    Accumulator;
merge([], List, Accumulator) ->
    [Head | Tail] = List,
    % I must recurse (or reverse) instead of just concat b/c we've merged the
    % list backwards (to later reverse) for efficiency purposes.
    merge([], Tail, [Head | Accumulator]);
merge(List, [], Accumulator) ->
    [Head | Tail] = List,
    % I must recurse (or reverse) instead of just concat b/c we've merged the
    % list backwards (to later reverse) for efficiency purposes.
    merge(Tail, [], [Head | Accumulator]);
merge(List1, List2, Accumulator) ->
    [Value1 | Rest1] = List1,
    [Value2 | Rest2] = List2,
    case Value1 < Value2 of
        true -> merge(Rest1, List2, [Value1 | Accumulator]);
        false -> merge(List1, Rest2, [Value2 | Accumulator])
    end.


%%
%% Floor and Ceiling are implemented in a later version of Erlang than I'm
%% using, so I must implement them at the moment.
%%
%% http://www2.erlangcentral.org/wiki/?title=Floating_Point_Rounding
%%
floor(Value) when Value < 0 ->
    Trunked = trunc(Value),
    case Value - Trunked == 0 of
        true -> Trunked;
        false -> Trunked - 1
    end;
floor(Value) -> 
    trunc(Value).
