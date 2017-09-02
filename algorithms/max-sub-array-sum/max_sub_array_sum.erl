-module(max_sub_array_sum).

-export([
    max_sub_array_sum/1,
    max_sub_array_sum_mr/1,
    max_sub_array_sum_kadane/1
]).


%%
%% Will calculate the max sum of a chain of integers in a list using brute force.
%%
max_sub_array_sum(List) when is_list(List) andalso length(List) > 0 ->
    max_sub_array_sum_helper(List, hd(List)).

max_sub_array_sum_helper([], CurrentMax) ->
    CurrentMax;
max_sub_array_sum_helper(SubList, CurrentMax) ->
    ChainMax = max_chain(SubList),
    PendingMax = max(CurrentMax, ChainMax),
    BrokenChainMax = max_sub_array_sum_helper(tl(SubList), PendingMax),
    max(PendingMax, BrokenChainMax).

max_chain([]) ->
    0;
max_chain(SubArray) ->
    [First | Rest] = SubArray,
    max(First, First + max_chain(Rest)).


%%
%% Will calculate the max sum of a chain of integers in a list using
%% map-reduce (map-fold).
%%
max_sub_array_sum_mr(List) when is_list(List) andalso length(List) > 0 ->
    max_sub_array_sum_mr_helper(List).

max_sub_array_sum_mr_helper(List) ->
    AllSubLists = sub_lists(List, 1, []),
    Result = lists:foldl(
        fun(SubList, MaxSum) ->
            max(lists:sum(SubList), MaxSum)
        end,
        hd(List),
        AllSubLists
    ),
    Result.


sub_lists(List, I, Accumulator) ->
    case length(List) >= I of
        true ->
            Accumulator1 = inner_sub_lists(List, I, 1, Accumulator),
            sub_lists(List, I + 1, Accumulator1);
        false ->
            Accumulator
    end.


inner_sub_lists(List, I, Len, Accumulator) ->
    case length(List) >= (I + Len - 1) of
        true ->
            ListToAdd = lists:sublist(List, I, Len),
            inner_sub_lists(List, I, Len + 1, [ListToAdd | Accumulator]);
        false ->
            Accumulator
    end.


%%
%% Will calculate the max sum of a chain of integers in a list using Kadane's
%% algorithm.
%%
max_sub_array_sum_kadane(List) when is_list(List) andalso length(List) > 0 ->
    %% We can start off the CurrentSubListMaxSum as 0 because it is added to a
    %% value in the List before used.  This prevents a 0 from being greater
    %% than a List of all negative values.
    max_sub_array_sum_kadane_helper(List, hd(List), 0).

max_sub_array_sum_kadane_helper([], CurrentMaxSum, _CurrentSubListMaxSum) ->
    CurrentMaxSum;
max_sub_array_sum_kadane_helper(List, CurrentMaxSum, CurrentSubListMaxSum) ->
    [Value | Rest] = List,
    CurrentSubListMaxSum1 = max(Value, Value + CurrentSubListMaxSum),
    CurrentMaxSum1 = max(CurrentMaxSum, CurrentSubListMaxSum1),
    max_sub_array_sum_kadane_helper(Rest, CurrentMaxSum1, CurrentSubListMaxSum1).
