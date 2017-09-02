-module(quicksort_tests).

-include_lib("eunit/include/eunit.hrl").


quicksort_test_() ->
    [
        {"Should handle an atom gracefully", fun atom/0},
        {"Should handle an integer gracefully", fun integer/0},
        {"Should handle a float gracefully", fun float/0},
        {"Should return [] for []", fun empty_list/0},
        {"Should return [0] for [0]", fun single_elem_list/0},
        {"Should return [1, 2, 3] for [1, 2, 3]", fun already_sorted_list/0},
        {"Should return [1, 2, 3, 4, 5, 6] for [1, 6, 3, 4, 2, 5]", fun all_positive_even_num_list/0},
        {"Should return [1, 2, 3, 4, 5] for [4, 2, 3, 1, 5]", fun all_positive_odd_num_list/0},
        {"Should return [-6, -5, -4, -3, -2, -1] for [-1, -2, -3, -4, -5, -6]", fun all_negative_list/0},
        {"Should return [-6, -4, -3, -1, 0, 2, 5] for [-1, 2, -3, -4, 5, -6, 0]", fun positive_and_negative_elem_list/0},
        {"Should return [1, 2, 2, 2, 4, 6, 9] for [1, 2, 4, 2, 9, 6, 2]", fun duplicate_elem_list/0}
    ].

atom() ->
    ?assertError(function_clause, quicksort:quicksort(abc)).

integer() ->
    ?assertError(function_clause, quicksort:quicksort(123)).

float() ->
    ?assertError(function_clause, quicksort:quicksort(1.1)).

empty_list() ->
    Input = [],
    Expected = [],
    Actual = quicksort:quicksort(Input),
    ?assertEqual(Expected, Actual).

single_elem_list() ->
    Input = [0],
    Expected = [0],
    Actual = quicksort:quicksort(Input),
    ?assertEqual(Expected, Actual).

already_sorted_list() ->
    Input = [1, 2, 3],
    Expected = [1, 2, 3],
    Actual = quicksort:quicksort(Input),
    ?assertEqual(Expected, Actual).

all_positive_even_num_list() ->
    Input = [1, 6, 3, 4, 2, 5],
    Expected = [1, 2, 3, 4, 5, 6],
    Actual = quicksort:quicksort(Input),
    ?assertEqual(Expected, Actual).

all_positive_odd_num_list() ->
    Input = [4, 2, 3, 1, 5],
    Expected = [1, 2, 3, 4, 5],
    Actual = quicksort:quicksort(Input),
    ?assertEqual(Expected, Actual).

all_negative_list() ->
    Input = [-1, -2, -3, -4, -5, -6],
    Expected = [-6, -5, -4, -3, -2, -1],
    Actual = quicksort:quicksort(Input),
    ?assertEqual(Expected, Actual).

positive_and_negative_elem_list() ->
    Input = [-1, 2, -3, -4, 5, -6, 0],
    Expected = [-6, -4, -3, -1, 0, 2, 5],
    Actual = quicksort:quicksort(Input),
    ?assertEqual(Expected, Actual).

duplicate_elem_list() ->
    Input = [1, 2, 4, 2, 9, 6, 2],
    Expected = [1, 2, 2, 2, 4, 6, 9],
    Actual = quicksort:quicksort(Input),
    ?assertEqual(Expected, Actual).
