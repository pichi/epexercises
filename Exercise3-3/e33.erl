-module(e33).

-export([prints/1, prints_even/1]).

prints(N) -> prints(N, 1).

prints(N, M) when M =< N ->
    io:format("Number:~p~n", [M]), prints(N, M + 1);
prints(_, _) -> ok.

prints_even(N) -> prints_even(N, 1).

prints_even(N, M) when M =< N, M rem 2 =:= 0 ->
    io:format("Number:~p~n", [M]), prints_even(N, M + 1);
prints_even(N, M) when M =< N -> prints_even(N, M + 1);
prints_even(_, _) -> ok.
