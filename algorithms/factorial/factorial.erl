-module(factorial).

-export([factorial/1]).


%%
%% Will return the factorial of the number passed in.
%%
factorial(0) ->
    1;
factorial(N) when is_number(N) andalso N >= 0 ->
    N * factorial(N - 1).
