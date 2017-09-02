-module(fibonacci_tests).

-include_lib("eunit/include/eunit.hrl").


fibonacci_test_() ->
    [
        {"Should handle an atom gracefully", fun atom/0},
        {"Should handle a string gracefully", fun string/0},
        {"Should handle a float gracefully", fun float/0},
        {"Should handle a negative number gracefully", fun negative/0},
        {"Should return 1 for fib(0)", fun zero/0},
        {"Should return 1 for fib(1)", fun one/0},
        {"Should return 2 for fib(2)", fun two/0},
        {"Should return 3 for fib(3)", fun three/0},
        {"Should return 5 for fib(4)", fun four/0},
        {"Should return 8 for fib(5)", fun five/0},
        {timeout, 20, {"Should return 165580141 for fib(40) and take more than 4 seconds", fun forty/0}},
        {"(Memoization) Should handle an atom gracefully", fun atom_memo/0},
        {"(Memoization) Should handle a string gracefully", fun string_memo/0},
        {"(Memoization) Should handle a float gracefully", fun float_memo/0},
        {"(Memoization) Should handle a negative number gracefully", fun negative_memo/0},
        {"(Memoization) Should return 1 for fib(0)", fun zero_memo/0},
        {"(Memoization) Should return 1 for fib(1)", fun one_memo/0},
        {"(Memoization) Should return 2 for fib(2)", fun two_memo/0},
        {"(Memoization) Should return 3 for fib(3)", fun three_memo/0},
        {"(Memoization) Should return 5 for fib(4)", fun four_memo/0},
        {"(Memoization) Should return 8 for fib(5)", fun five_memo/0},
        {timeout, 20, {"(Memoization) Should return 165580141 for fib(40) and take less than 2 seconds", fun forty_memo/0}}
    ].

atom() ->
    ?assertError(function_clause, fibonacci:fibonacci(abc)).

string() ->
    ?assertError(function_clause, fibonacci:fibonacci("abc")).

float() ->
    ?assertError(function_clause, fibonacci:fibonacci(1.1)).

negative() ->
    ?assertError(function_clause, fibonacci:fibonacci(-1)).

zero() ->
    ?assertEqual(1, fibonacci:fibonacci(0)).

one() ->
    ?assertEqual(1, fibonacci:fibonacci(1)).

two() ->
    ?assertEqual(2, fibonacci:fibonacci(2)).

three() ->
    ?assertEqual(3, fibonacci:fibonacci(3)).

four() ->
    ?assertEqual(5, fibonacci:fibonacci(4)).

five() ->
    ?assertEqual(8, fibonacci:fibonacci(5)).

forty() ->
    {Time, Result} = timer:tc(
        fun() ->
            fibonacci:fibonacci(40)
        end
    ),
    ?assertEqual(165580141, Result),
    ?assert(Time > 4000).

atom_memo() ->
    ?assertError(function_clause, fibonacci:fibonacci_memo(abc)).

string_memo() ->
    ?assertError(function_clause, fibonacci:fibonacci_memo("abc")).

float_memo() ->
    ?assertError(function_clause, fibonacci:fibonacci_memo(1.1)).

negative_memo() ->
    ?assertError(function_clause, fibonacci:fibonacci_memo(-1)).

zero_memo() ->
    ?assertEqual(1, fibonacci:fibonacci_memo(0)).

one_memo() ->
    ?assertEqual(1, fibonacci:fibonacci_memo(1)).

two_memo() ->
    ?assertEqual(2, fibonacci:fibonacci_memo(2)).

three_memo() ->
    ?assertEqual(3, fibonacci:fibonacci_memo(3)).

four_memo() ->
    ?assertEqual(5, fibonacci:fibonacci_memo(4)).

five_memo() ->
    ?assertEqual(8, fibonacci:fibonacci_memo(5)).

forty_memo() ->
    {Time, Result} = timer:tc(
        fun() ->
            fibonacci:fibonacci_memo(40)
        end
    ),
    ?assertEqual(165580141, Result),
    ?assert(Time < 2000).
