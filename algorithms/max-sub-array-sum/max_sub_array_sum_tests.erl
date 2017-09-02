-module(max_sub_array_sum_tests).

-include_lib("eunit/include/eunit.hrl").


fibonacci_test_() ->
    [
        {"Should handle an atom gracefully", fun atom/0},
        {"Should handle a tuple gracefully", fun tuple/0},
        {"Should handle an empty list", fun empty_list/0},
        {"Should return the sum for an all positive list", fun all_positive/0},
        {"Should return the max for an all negative list", fun all_negative/0},
        {"Should handle a single positive element list", fun single_positive/0},
        {"Should handle a single negative element list", fun single_negative/0},
        {"Should return 6 for [-1, 1, 2, 3]", fun ends_with_chain/0},
        {"Should return 4 for [-1, 3, -2, 2, 1]", fun chain_with_negative/0},
        {"Should return 6 for [1, 2, -16, 3, 2, 1, -8]", fun chain_without_negative/0},
        {"(Map Reduce) Should handle an atom gracefully", fun atom_mr/0},
        {"(Map Reduce) Should handle a tuple gracefully", fun tuple_mr/0},
        {"(Map Reduce) Should handle an empty list", fun empty_list_mr/0},
        {"(Map Reduce) Should return the sum for an all positive list", fun all_positive_mr/0},
        {"(Map Reduce) Should return the max for an all negative list", fun all_negative_mr/0},
        {"(Map Reduce) Should handle a single positive element list", fun single_positive_mr/0},
        {"(Map Reduce) Should handle a single negative element list", fun single_negative_mr/0},
        {"(Map Reduce) Should return 6 for [-1, 1, 2, 3]", fun ends_with_chain_mr/0},
        {"(Map Reduce) Should return 4 for [-1, 3, -2, 2, 1]", fun chain_with_negative_mr/0},
        {"(Map Reduce) Should return 6 for [1, 2, -16, 3, 2, 1, -8]", fun chain_without_negative_mr/0},
        {"(Kadane's Alg) Should handle an atom gracefully", fun atom_kadane/0},
        {"(Kadane's Alg) Should handle a tuple gracefully", fun tuple_kadane/0},
        {"(Kadane's Alg) Should handle an empty list", fun empty_list_kadane/0},
        {"(Kadane's Alg) Should return the sum for an all positive list", fun all_positive_kadane/0},
        {"(Kadane's Alg) Should return the max for an all negative list", fun all_negative_kadane/0},
        {"(Kadane's Alg) Should handle a single positive element list", fun single_positive_kadane/0},
        {"(Kadane's Alg) Should handle a single negative element list", fun single_negative_kadane/0},
        {"(Kadane's Alg) Should return 6 for [-1, 1, 2, 3]", fun ends_with_chain_kadane/0},
        {"(Kadane's Alg) Should return 4 for [-1, 3, -2, 2, 1]", fun chain_with_negative_kadane/0},
        {"(Kadane's Alg) Should return 6 for [1, 2, -16, 3, 2, 1, -8]", fun chain_without_negative_kadane/0}
    ].

atom() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum(abc)).

tuple() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum({abc})).

empty_list() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum([])).

all_positive() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum([1, 2, 3])).

all_negative() ->
    ?assertEqual(-1, max_sub_array_sum:max_sub_array_sum([-2, -1, -3])).

single_positive() ->
    ?assertEqual(8, max_sub_array_sum:max_sub_array_sum([8])).

single_negative() ->
    ?assertEqual(-8, max_sub_array_sum:max_sub_array_sum([-8])).

ends_with_chain() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum([-1, 1, 2, 3])).

chain_with_negative() ->
    ?assertEqual(4, max_sub_array_sum:max_sub_array_sum([-1, 3, -2, 2, 1])).

chain_without_negative() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum([1, 2, -16, 3, 2, 1, -8])).


atom_mr() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum_mr(abc)).

tuple_mr() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum_mr({abc})).

empty_list_mr() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum_mr([])).

all_positive_mr() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum_mr([1, 2, 3])).

all_negative_mr() ->
    ?assertEqual(-1, max_sub_array_sum:max_sub_array_sum_mr([-2, -1, -3])).

single_positive_mr() ->
    ?assertEqual(8, max_sub_array_sum:max_sub_array_sum_mr([8])).

single_negative_mr() ->
    ?assertEqual(-8, max_sub_array_sum:max_sub_array_sum_mr([-8])).

ends_with_chain_mr() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum_mr([-1, 1, 2, 3])).

chain_with_negative_mr() ->
    ?assertEqual(4, max_sub_array_sum:max_sub_array_sum_mr([-1, 3, -2, 2, 1])).

chain_without_negative_mr() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum_mr([1, 2, -16, 3, 2, 1, -8])).


atom_kadane() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum_kadane(abc)).

tuple_kadane() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum_kadane({abc})).

empty_list_kadane() ->
    ?assertError(function_clause, max_sub_array_sum:max_sub_array_sum_kadane([])).

all_positive_kadane() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum_kadane([1, 2, 3])).

all_negative_kadane() ->
    ?assertEqual(-1, max_sub_array_sum:max_sub_array_sum_kadane([-2, -1, -3])).

single_positive_kadane() ->
    ?assertEqual(8, max_sub_array_sum:max_sub_array_sum_kadane([8])).

single_negative_kadane() ->
    ?assertEqual(-8, max_sub_array_sum:max_sub_array_sum_kadane([-8])).

ends_with_chain_kadane() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum_kadane([-1, 1, 2, 3])).

chain_with_negative_kadane() ->
    ?assertEqual(4, max_sub_array_sum:max_sub_array_sum_kadane([-1, 3, -2, 2, 1])).

chain_without_negative_kadane() ->
    ?assertEqual(6, max_sub_array_sum:max_sub_array_sum_kadane([1, 2, -16, 3, 2, 1, -8])).
