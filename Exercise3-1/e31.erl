-module(e31).

-export([sum/1, sum/2]).

sum(N) -> sum(1, N).

sum(N, M) when is_integer(N), is_integer(M), N =< M ->
    sum(M, N, M).

sum(M, M, X) -> X;
sum(M, N, X) -> sum(M, N + 1, X + N).
