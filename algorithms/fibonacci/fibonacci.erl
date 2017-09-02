-module(fibonacci).

-export([fibonacci/1, fibonacci_memo/1]).


%%
%% Will return the fibonacci numer in for the passed in number of the sequence
%% using simple recursion.
%%
fibonacci(0) ->
    1;
fibonacci(1) ->
    1;
fibonacci(N) when is_integer(N) andalso N >= 0 ->
    fibonacci(N - 2) + fibonacci(N - 1).


%%
%% Will return the fibonacci numer in for the passed in number of the sequence
%% using memoization to drastically reduce the number of calculations.
%%
fibonacci_memo(N) when is_integer(N) andalso N >= 0 ->
    % First prep the "cache"
    StartingDict1 = dict:new(),
    StartingDict2 = dict:store(0, 1, StartingDict1),
    StartingDict3 = dict:store(1, 1, StartingDict2),
    % Now call the helper function and pattern match out the result
    {Result, _} = fibonacci_memo(N, StartingDict3),
    Result.

fibonacci_memo(N, Accumulated) ->
    case dict:find(N, Accumulated) of
        {ok, Result} ->
            % This result was already computed, so just pass it and the state back to the caller
            {Result, Accumulated};
        error ->
            {NMinus2Result, Accumulated1} = fibonacci_memo(N - 2, Accumulated),
            % Potentially update the accumulator with the N-2 result
            Accumulated2 = dict:store(N - 2, NMinus2Result, Accumulated1),

            {NMinus1Result, Accumulated3} = fibonacci_memo(N - 1, Accumulated2),
            % Potentially update the accumulator with the N-1 result
            Accumulated4 = dict:store(N - 1, NMinus1Result, Accumulated3),

            Result = NMinus2Result + NMinus1Result,
            % Update the accumulator with the N result
            Accumulated5 = dict:store(N, Result, Accumulated4),

            % Now return the result and current state
            {Result, Accumulated5}
    end.
