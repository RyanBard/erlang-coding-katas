-module(factorial_tests).

-include_lib("eunit/include/eunit.hrl").


factorial_test_() ->
    [
        {"Should handle an atom gracefully", fun atom/0},
        {"Should handle a string gracefully", fun string/0},
        {"Should handle a float gracefully", fun float/0},
        {"Should handle a negative number gracefully", fun negative/0},
        {"Should return 1 for 0!", fun zero/0},
        {"Should return 1 for 1!", fun one/0},
        {"Should return 2 for 2!", fun two/0},
        {"Should return 6 for 3!", fun three/0},
        {"Should return 24 for 4!", fun four/0},
        {"Should return 120 for 5!", fun five/0}
    ].

atom() ->
    ?assertError(function_clause, factorial:factorial(abc)).

string() ->
    ?assertError(function_clause, factorial:factorial("abc")).

float() ->
    ?assertError(function_clause, factorial:factorial(1.1)).

negative() ->
    ?assertError(function_clause, factorial:factorial(-1)).

zero() ->
    ?assertEqual(1, factorial:factorial(0)).

one() ->
    ?assertEqual(1, factorial:factorial(1)).

two() ->
    ?assertEqual(2, factorial:factorial(2)).

three() ->
    ?assertEqual(6, factorial:factorial(3)).

four() ->
    ?assertEqual(24, factorial:factorial(4)).

five() ->
    ?assertEqual(120, factorial:factorial(5)).
